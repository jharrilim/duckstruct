/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import * as fs from 'fs';
import * as path from 'path';
import { spawn } from 'child_process';
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
	InitializeResult,
	Hover,
	MarkupKind,
	Location,
	Range,
	Position
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

/** Resolved workspace roots (filesystem paths) for manifest discovery. */
let workspaceFolderPaths: string[] = [];

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

	if (params.workspaceFolders && params.workspaceFolders.length > 0) {
		workspaceFolderPaths = params.workspaceFolders.map((f) => fileURLToPath(f.uri));
	} else if (params.rootUri) {
		workspaceFolderPaths = [fileURLToPath(params.rootUri)];
	} else {
		workspaceFolderPaths = [];
	}

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			completionProvider: {
				resolveProvider: true
			},
			hoverProvider: true,
			definitionProvider: true
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
		connection.workspace.onDidChangeWorkspaceFolders((event) => {
			for (const f of event.removed) {
				const p = fileURLToPath(f.uri);
				workspaceFolderPaths = workspaceFolderPaths.filter((x) => x !== p);
			}
			for (const f of event.added) {
				workspaceFolderPaths.push(fileURLToPath(f.uri));
			}
		});
	}
});

// The example settings
interface ExampleSettings {
	maxNumberOfProblems: number;
	duckstructPath: string;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000, duckstructPath: 'ds' };
let globalSettings: ExampleSettings = defaultSettings;

/**
 * JSON array element from `duckstruct check --json` (LSP-aligned).
 * - `range`: 0-based line, `character` is UTF-16 offset within the line (LSP).
 * - `severity`: 1 = Error, 2 = Warning (matches LSP DiagnosticSeverity).
 * - `code`, `related_information`: optional; forwarded when the client supports related info.
 */
interface CheckDiagnostic {
	range: { start: { line: number; character: number }; end: { line: number; character: number } };
	message: string;
	severity: number;
	code?: string;
	/** Matches Rust serde field name on `LspDiagnostic`. */
	related_information?: Array<{
		location: { uri: string; range: CheckDiagnostic['range'] };
		message: string;
	}>;
}

function severityFromCheck(s: number): DiagnosticSeverity {
	switch (s) {
		case 1:
			return DiagnosticSeverity.Error;
		case 2:
			return DiagnosticSeverity.Warning;
		case 3:
			return DiagnosticSeverity.Information;
		case 4:
			return DiagnosticSeverity.Hint;
		default:
			return DiagnosticSeverity.Error;
	}
}

function checkDiagnosticToLsp(d: CheckDiagnostic): Diagnostic {
	const out: Diagnostic = {
		range: {
			start: { line: d.range.start.line, character: d.range.start.character },
			end: { line: d.range.end.line, character: d.range.end.character }
		},
		message: d.message,
		severity: severityFromCheck(d.severity),
		source: 'duckstruct'
	};
	if (d.code) {
		out.code = d.code;
	}
	if (d.related_information && hasDiagnosticRelatedInformationCapability) {
		out.relatedInformation = d.related_information.map((ri) => ({
			location: {
				uri: ri.location.uri,
				range: {
					start: {
						line: ri.location.range.start.line,
						character: ri.location.range.start.character
					},
					end: { line: ri.location.range.end.line, character: ri.location.range.end.character }
				}
			},
			message: ri.message
		}));
	}
	return out;
}

/** JSON line from `ds ide query` (Rust `IdeQueryResult`). */
interface IdeQueryJson {
	hover?: string | null;
	definition?: IdeLocationJson | null;
	error?: string | null;
}

interface IdeLocationJson {
	uri: string;
	range: {
		start: { line: number; character: number };
		end: { line: number; character: number };
	};
}

function duckstructProjectRootForFile(filePath: string): string | undefined {
	for (const root of workspaceFolderPaths) {
		const normRoot = path.resolve(root);
		const normFile = path.resolve(filePath);
		if (normFile === normRoot || normFile.startsWith(normRoot + path.sep)) {
			const manifest = path.join(normRoot, 'duckstruct.toml');
			if (fs.existsSync(manifest)) {
				return normRoot;
			}
		}
	}
	let dir = path.dirname(path.resolve(filePath));
	for (let i = 0; i < 100; i++) {
		const manifest = path.join(dir, 'duckstruct.toml');
		if (fs.existsSync(manifest)) {
			return dir;
		}
		const parent = path.dirname(dir);
		if (parent === dir) {
			return undefined;
		}
		dir = parent;
	}
	return undefined;
}

function ideLocationToLsp(loc: IdeLocationJson): Location {
	return {
		uri: loc.uri,
		range: Range.create(
			Position.create(loc.range.start.line, loc.range.start.character),
			Position.create(loc.range.end.line, loc.range.end.character)
		)
	};
}

async function runIdeQuery(
	duckstructPath: string,
	filePath: string,
	source: string,
	line: number,
	character: number,
	projectRoot: string | undefined
): Promise<IdeQueryJson | null> {
	const args = [
		'ide',
		'query',
		'--file',
		filePath,
		'--line',
		String(line),
		'--character',
		String(character)
	];
	if (projectRoot) {
		args.push('--project', projectRoot);
	}
	return await new Promise((resolve) => {
		const child = spawn(duckstructPath, args, {
			stdio: ['pipe', 'pipe', 'pipe']
		});
		let stdout = '';
		let stderr = '';
		const timer = setTimeout(() => child.kill('SIGTERM'), 30_000);
		child.stdout.setEncoding('utf8');
		child.stderr.setEncoding('utf8');
		child.stdout.on('data', (chunk: string) => {
			stdout += chunk;
		});
		child.stderr.on('data', (chunk: string) => {
			stderr += chunk;
		});
		child.on('error', (err: NodeJS.ErrnoException) => {
			clearTimeout(timer);
			connection.console.error(`duckstruct ide query (${duckstructPath}): ${err.message}`);
			resolve(null);
		});
		child.on('close', () => {
			clearTimeout(timer);
			if (stderr.trim()) {
				connection.console.warn(`duckstruct ide: ${stderr.trim()}`);
			}
			const out = stdout.trim();
			if (!out) {
				resolve(null);
				return;
			}
			try {
				resolve(JSON.parse(out) as IdeQueryJson);
			} catch (e) {
				connection.console.error(`duckstruct ide query: invalid JSON (${e})`);
				resolve(null);
			}
		});
		child.stdin.write(source, 'utf8');
		child.stdin.end();
	});
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
	const duckstructPath = settings.duckstructPath || defaultSettings.duckstructPath;
	const source = textDocument.getText();

	// Ranges come from `ds check --json`; point duckstructPath at a current `ds` (stale builds mis-map lines).

	// Run check on the in-memory buffer so diagnostics match unsaved edits. The CLI reads stdin
	// and honors env `DUCKSTRUCT_LSP_DOCUMENT_URI` for JSON / relatedInformation URIs.
	const diagnostics: Diagnostic[] = await new Promise((resolve) => {
		const child = spawn(duckstructPath, ['check', '--json'], {
			env: { ...process.env, DUCKSTRUCT_LSP_DOCUMENT_URI: uri },
			stdio: ['pipe', 'pipe', 'pipe']
		});

		let stdout = '';
		let stderr = '';
		const timer = setTimeout(() => child.kill('SIGTERM'), 30_000);

		child.stdout.setEncoding('utf8');
		child.stderr.setEncoding('utf8');
		child.stdout.on('data', (chunk: string) => {
			stdout += chunk;
		});
		child.stderr.on('data', (chunk: string) => {
			stderr += chunk;
		});

		child.on('error', (err: NodeJS.ErrnoException) => {
			clearTimeout(timer);
			connection.console.error(
				`duckstruct check failed (${duckstructPath}): ${err.message}`
			);
			resolve([]);
		});

		child.on('close', () => {
			clearTimeout(timer);
			const out = stdout.trim();
			const errText = stderr.trim();
			if (errText) {
				connection.console.warn(`duckstruct: ${errText}`);
			}
			if (!out) {
				resolve([]);
				return;
			}
			try {
				const raw: CheckDiagnostic[] = JSON.parse(out);
				resolve(raw.map(checkDiagnosticToLsp));
			} catch (e) {
				connection.console.error(`duckstruct check: invalid JSON (${e})`);
				resolve([]);
			}
		});

		child.stdin.write(source, 'utf8');
		child.stdin.end();
	});

	connection.sendDiagnostics({ uri, diagnostics });
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

connection.onHover(async (params): Promise<Hover | null> => {
	const uri = params.textDocument.uri;
	if (!uri.startsWith('file:')) {
		return null;
	}
	const doc = documents.get(uri);
	if (!doc) {
		return null;
	}
	const settings = await getDocumentSettings(uri);
	const duckstructPath = settings.duckstructPath || defaultSettings.duckstructPath;
	const fp = fileURLToPath(uri);
	const project = duckstructProjectRootForFile(fp);
	const q = await runIdeQuery(
		duckstructPath,
		fp,
		doc.getText(),
		params.position.line,
		params.position.character,
		project
	);
	if (!q) {
		return null;
	}
	if (q.error) {
		return {
			contents: {
				kind: MarkupKind.Markdown,
				value: '```\n' + q.error + '\n```'
			}
		};
	}
	const h = q.hover;
	if (h === null || h === undefined || h === '') {
		return null;
	}
	return {
		contents: {
			kind: MarkupKind.Markdown,
			value: '`' + h + '`'
		}
	};
});

connection.onDefinition(async (params): Promise<Location | null> => {
	const uri = params.textDocument.uri;
	if (!uri.startsWith('file:')) {
		return null;
	}
	const doc = documents.get(uri);
	if (!doc) {
		return null;
	}
	const settings = await getDocumentSettings(uri);
	const duckstructPath = settings.duckstructPath || defaultSettings.duckstructPath;
	const fp = fileURLToPath(uri);
	const project = duckstructProjectRootForFile(fp);
	const q = await runIdeQuery(
		duckstructPath,
		fp,
		doc.getText(),
		params.position.line,
		params.position.character,
		project
	);
	if (!q?.definition) {
		return null;
	}
	return ideLocationToLsp(q.definition);
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
	{ label: 'struct', kind: CompletionItemKind.Keyword, data: 'kw_struct' },
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
