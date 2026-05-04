/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as path from 'path';

import {
	downloadAndUnzipVSCode,
	resolveCliPathFromVSCodeExecutablePath,
	runTests
} from '@vscode/test-electron';

async function main() {
	try {
		// The folder containing the Extension Manifest package.json
		// Passed to `--extensionDevelopmentPath`
		const extensionDevelopmentPath = path.resolve(__dirname, '../../../');

		// The path to test runner
		// Passed to --extensionTestsPath
		const extensionTestsPath = path.resolve(__dirname, './index');

		const workspace =
			process.env.CODE_TESTS_WORKSPACE && process.env.CODE_TESTS_WORKSPACE.length > 0
				? path.resolve(process.env.CODE_TESTS_WORKSPACE)
				: path.join(extensionDevelopmentPath, 'client', 'testFixture');

		const downloadOptions = {
			extensionDevelopmentPath,
			extensionTestsPath
		};

		// Match runTests() so version resolution (from engines) stays identical.
		let vscodeExecutablePath = await downloadAndUnzipVSCode(downloadOptions);

		// `downloadDirToExecutablePath` points at `.../MacOS/Electron`, which is a stub that does not
		// accept extension-test CLI flags. Use the `bin/code` launcher (runs Code + cli.js) instead.
		if (process.platform === 'darwin') {
			vscodeExecutablePath = resolveCliPathFromVSCodeExecutablePath(vscodeExecutablePath);
		}

		await runTests({
			...downloadOptions,
			vscodeExecutablePath,
			launchArgs: [workspace]
		});
	} catch (err) {
		console.error('Failed to run tests', err);
		process.exit(1);
	}
}

main();
