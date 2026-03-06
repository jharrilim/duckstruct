/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import { execFile } from 'child_process';
import { fileURLToPath } from 'url';
import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult
} from 'vscode-languageserver/node';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

// The example settings
interface ExampleSettings {
	maxNumberOfProblems: number;
	duckstructPath: string;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000, duckstructPath: 'duckstruct' };
let globalSettings: ExampleSettings = defaultSettings;

// LSP diagnostic from Rust check --json
interface CheckDiagnostic {
	range: { start: { line: number; character: number }; end: { line: number; character: number } };
	message: string;
	severity: number;
}

// Cache the settings of all open documents
const documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
	} else {
		globalSettings = <ExampleSettings>(
			(change.settings.languageServerExample || defaultSettings)
		);
	}

	// Revalidate all open text documents
	documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace.getConfiguration({
			scopeUri: resource,
			section: 'languageServerExample'
		});
		documentSettings.set(resource, result);
	}
	return result;
}

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	const settings = await getDocumentSettings(textDocument.uri);
	const uri = textDocument.uri;
	if (!uri.startsWith('file:')) {
		connection.sendDiagnostics({ uri, diagnostics: [] });
		return;
	}
	const filePath = fileURLToPath(uri);
	const duckstructPath = settings.duckstructPath || 'duckstruct';

	const diagnostics: Diagnostic[] = await new Promise((resolve) => {
		execFile(duckstructPath, ['check', '--json', filePath], { timeout: 5000 }, (err: Error | null, stdout: string | Buffer, _stderr: string | Buffer) => {
			const out = (typeof stdout === 'string' ? stdout : stdout.toString()).trim();
			if (err) {
				// Binary missing or non-zero exit (e.g. parse errors are still printed to stdout)
				if (out) {
					try {
						const raw: CheckDiagnostic[] = JSON.parse(out);
						resolve(
							raw.map((d) => ({
								range: {
									start: { line: d.range.start.line, character: d.range.start.character },
									end: { line: d.range.end.line, character: d.range.end.character }
								},
								message: d.message,
								severity: d.severity === 1 ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
								source: 'duckstruct'
							}))
						);
						return;
					} catch (_e) {
						// fall through to empty
					}
				}
				resolve([]);
				return;
			}
			try {
				const raw: CheckDiagnostic[] = JSON.parse(out || '[]');
				resolve(
					raw.map((d) => ({
						range: {
							start: { line: d.range.start.line, character: d.range.start.character },
							end: { line: d.range.end.line, character: d.range.end.character }
						},
						message: d.message,
						severity: d.severity === 1 ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
						source: 'duckstruct'
					}))
				);
			} catch (_e) {
				resolve([]);
			}
		});
	});

	connection.sendDiagnostics({ uri, diagnostics });
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

// Duckstruct keywords and stdlib for completion
const KEYWORD_ITEMS: CompletionItem[] = [
	{ label: 'f', kind: CompletionItemKind.Keyword, data: 'kw_f', detail: 'function' },
	{ label: 'let', kind: CompletionItemKind.Keyword, data: 'kw_let', detail: 'binding' },
	{ label: 'if', kind: CompletionItemKind.Keyword, data: 'kw_if' },
	{ label: 'else', kind: CompletionItemKind.Keyword, data: 'kw_else' },
	{ label: 'for', kind: CompletionItemKind.Keyword, data: 'kw_for' },
	{ label: 'in', kind: CompletionItemKind.Keyword, data: 'kw_in' },
	{ label: 'where', kind: CompletionItemKind.Keyword, data: 'kw_where' },
	{ label: 'while', kind: CompletionItemKind.Keyword, data: 'kw_while' },
	{ label: 'use', kind: CompletionItemKind.Keyword, data: 'kw_use', detail: 'import' },
	{ label: 'as', kind: CompletionItemKind.Keyword, data: 'kw_as' },
	{ label: 'pub', kind: CompletionItemKind.Keyword, data: 'kw_pub', detail: 'public' },
	{ label: 'mod', kind: CompletionItemKind.Keyword, data: 'kw_mod', detail: 'module' },
	{ label: 'class', kind: CompletionItemKind.Keyword, data: 'kw_class' },
	{ label: 'true', kind: CompletionItemKind.Keyword, data: 'lit_true' },
	{ label: 'false', kind: CompletionItemKind.Keyword, data: 'lit_false' }
];
const STDLIB_ITEMS: CompletionItem[] = [
	{ label: 'file', kind: CompletionItemKind.Module, data: 'stdlib_file', detail: 'stdlib module', documentation: 'use file::{ read, write }' },
	{ label: 'read', kind: CompletionItemKind.Function, data: 'stdlib_read', detail: 'file.read(path)', documentation: 'Read file at path' },
	{ label: 'write', kind: CompletionItemKind.Function, data: 'stdlib_write', detail: 'file.write(path, content)', documentation: 'Write content to path' },
	{ label: 'print', kind: CompletionItemKind.Function, data: 'stdlib_print', detail: 'print(value)', documentation: 'Print value (LLVM backend)' },
	{ label: 'PI', kind: CompletionItemKind.Constant, data: 'stdlib_pi', detail: 'constant', documentation: 'Math constant π (LLVM backend)' }
];

connection.onCompletion(
	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		return [...KEYWORD_ITEMS, ...STDLIB_ITEMS];
	}
);

connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		const d = item.data as string | undefined;
		if (!d) return item;
		if (d.startsWith('kw_') || d.startsWith('lit_')) {
			item.documentation = item.detail ? `Keyword: ${item.detail}` : undefined;
		}
		return item;
	}
);

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
