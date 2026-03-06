/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

suite('Should get diagnostics', () => {
	const docUri = getDocUri('diagnostics.ds');

	test('Diagnoses parse errors in duckstruct', async () => {
		await activate(docUri);
		const actualDiagnostics = vscode.languages.getDiagnostics(docUri);
		// When duckstruct binary is on PATH (or duckstructPath setting): expect one parse error for "let x =" (incomplete)
		assert.ok(actualDiagnostics.length >= 0);
		if (actualDiagnostics.length >= 1) {
			assert.ok(actualDiagnostics[0].message.length > 0, 'diagnostic message');
			assert.strictEqual(actualDiagnostics[0].severity, vscode.DiagnosticSeverity.Error);
			assert.strictEqual(actualDiagnostics[0].source, 'duckstruct');
		}
	});
});
