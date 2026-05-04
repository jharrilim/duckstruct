/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

suite('Should do completion', () => {
	const docUri = getDocUri('completion.ds');

	test('Completes in duckstruct file', async () => {
		await testCompletion(docUri, new vscode.Position(0, 0), {
			items: [
				{ label: 'f', kind: vscode.CompletionItemKind.Keyword },
				{ label: 'let', kind: vscode.CompletionItemKind.Keyword }
			]
		});
	});
});

async function testCompletion(
	docUri: vscode.Uri,
	position: vscode.Position,
	expectedCompletionList: vscode.CompletionList
) {
	await activate(docUri);

	// Executing the command `vscode.executeCompletionItemProvider` to simulate triggering completion
	const actualCompletionList = (await vscode.commands.executeCommand(
		'vscode.executeCompletionItemProvider',
		docUri,
		position
	)) as vscode.CompletionList;

	assert.ok(actualCompletionList.items.length >= expectedCompletionList.items.length);
	for (const expectedItem of expectedCompletionList.items) {
		const actualItem = actualCompletionList.items.find((i) => i.label === expectedItem.label);
		assert.ok(actualItem, `missing completion item ${expectedItem.label}`);
		assert.strictEqual(actualItem!.kind, expectedItem.kind);
	}
}
