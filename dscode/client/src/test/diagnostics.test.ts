/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function waitForDiagnostics(
	docUri: vscode.Uri,
	opts: { min: number; timeoutMs: number }
): Promise<vscode.Diagnostic[]> {
	const deadline = Date.now() + opts.timeoutMs;
	while (Date.now() < deadline) {
		const d = vscode.languages.getDiagnostics(docUri);
		if (d.length >= opts.min) {
			return d;
		}
		await sleep(200);
	}
	return vscode.languages.getDiagnostics(docUri);
}

suite('Should get diagnostics', () => {
	const docUri = getDocUri('diagnostics.ds');

	test('Diagnoses parse errors in duckstruct', async () => {
		await activate(docUri);
		const requireDiagnostics = Boolean(process.env.DUCKSTRUCT_E2E_DS_PATH);
		const actualDiagnostics = await waitForDiagnostics(docUri, {
			min: requireDiagnostics ? 1 : 0,
			timeoutMs: 20_000
		});
		assert.ok(
			actualDiagnostics.length >= (requireDiagnostics ? 1 : 0),
			requireDiagnostics
				? 'expected at least one diagnostic (set DUCKSTRUCT_E2E_DS_PATH and ensure ds check --json works)'
				: 'diagnostics'
		);
		if (actualDiagnostics.length >= 1) {
			assert.ok(actualDiagnostics[0].message.length > 0, 'diagnostic message');
			assert.strictEqual(actualDiagnostics[0].severity, vscode.DiagnosticSeverity.Error);
			assert.strictEqual(actualDiagnostics[0].source, 'duckstruct');
		}
	});

	test('Parse diagnostic range is on the bad statement line, not the comment (E2E)', async () => {
		if (!process.env.DUCKSTRUCT_E2E_DS_PATH) {
			return;
		}
		const uri = getDocUri('parse-span.ds');
		await activate(uri);
		const d = await waitForDiagnostics(uri, { min: 1, timeoutMs: 20_000 });
		assert.ok(d.length >= 1, 'expected a parse diagnostic');
		assert.ok(
			d[0].range.start.line >= 2,
			`expected first error on or after line 3 (0-based line >= 2), got line ${d[0].range.start.line} col ${d[0].range.start.character}: ${d[0].message}`
		);
	});
});
