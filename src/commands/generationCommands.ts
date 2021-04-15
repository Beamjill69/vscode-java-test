// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license.

import { commands, ExtensionContext, TextEdit, Uri, window, workspace, WorkspaceEdit } from 'vscode';
import * as protocolConverter from 'vscode-languageclient/lib/protocolConverter';
import * as commandUtils from '../utils/commandUtils';

const converter: protocolConverter.Converter = protocolConverter.createConverter();
export async function generateTests(uri: Uri, startPosition: number): Promise<void> {
    const edit: WorkspaceEdit = converter.asWorkspaceEdit(await commandUtils.generateTests(uri, startPosition));
    if (edit) {
        await workspace.applyEdit(edit);
        const entries: [Uri, TextEdit[]][] = edit.entries();
        if (entries?.[0]?.[0]) {
            await window.showTextDocument(entries[0][0], {
                preserveFocus: true,
            });
        }
    }
}

export async function registerSelectTestFrameworkCommand(context: ExtensionContext): Promise<void> {
    context.subscriptions.push(commands.registerCommand('_java.test.askClientForChoice', async (placeHolder: string, choices: string[], canPickMany: boolean) => {
        const choice: string | undefined = await window.showQuickPick(choices, {
            placeHolder,
            canPickMany,
        });
        return choice;
    }));
}

export async function registerAskForInputCommand(context: ExtensionContext): Promise<void> {
    context.subscriptions.push(commands.registerCommand('_java.test.askClientForInput', async (prompt: string, value: string) => {
        const ans: string | undefined = await window.showInputBox({
            value,
            prompt,
            validateInput: (input: string) => {
                if (!input.trim()) {
                    return "Empty value is not allowed.";
                }

                return undefined;
            }
        });
        return ans;
    }));
}
