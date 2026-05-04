import * as assert from 'assert';
import * as vscode from 'vscode';
import { activate, getDocUri } from './helper';

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

suite('IDE hover and definition', () => {
	test('hover shows type on variable reference (E2E)', async function () {
		if (!process.env.DUCKSTRUCT_E2E_DS_PATH) {
			this.skip();
		}
		const uri = getDocUri('ide-hover.ds');
		await activate(uri);
		for (let i = 0; i < 25; i++) {
			const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
				'vscode.executeHoverProvider',
				uri,
				new vscode.Position(1, 0)
			);
			if (hovers && hovers.length > 0) {
				const md = hovers[0].contents.map((c) => (typeof c === 'string' ? c : c.value)).join('\n');
				assert.ok(/\b1\b|`1`|number/.test(md), `expected hover type for x, got: ${md}`);
				return;
			}
			await sleep(400);
		}
		assert.fail('no hover from executeHoverProvider after waiting');
	});

	test('definition jumps to let binding (E2E)', async function () {
		if (!process.env.DUCKSTRUCT_E2E_DS_PATH) {
			this.skip();
		}
		const uri = getDocUri('ide-hover.ds');
		await activate(uri);
		for (let i = 0; i < 25; i++) {
			const locs = await vscode.commands.executeCommand<vscode.Location[]>(
				'vscode.executeDefinitionProvider',
				uri,
				new vscode.Position(1, 0)
			);
			if (locs && locs.length > 0) {
				assert.strictEqual(locs[0].uri.toString(), uri.toString());
				assert.strictEqual(locs[0].range.start.line, 0);
				return;
			}
			await sleep(400);
		}
		assert.fail('no definition from executeDefinitionProvider after waiting');
	});
});
