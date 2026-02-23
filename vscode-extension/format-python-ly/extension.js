const vscode = require('vscode');

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    let disposable = vscode.commands.registerCommand('solmisasi.formatWithPythonLy', async function () {
        try {
            const editor = vscode.window.activeTextEditor;
            let fileUri;
            if (editor) {
                fileUri = editor.document.uri;
                if (editor.document.isDirty) {
                    // Save current buffer before formatting so formatter sees latest contents
                    await editor.document.save();
                }
            }

            await vscode.commands.executeCommand('workbench.action.tasks.runTask', 'Format current LilyPond file (python-ly)');

            // After task completes, reload the file from disk so the editor shows formatted content
            if (fileUri) {
                try {
                    // Revert will reload the file from disk (no-op if unchanged)
                    await vscode.commands.executeCommand('workbench.action.files.revert');
                } catch (err) {
                    // fallback: open the document again
                    const doc = await vscode.workspace.openTextDocument(fileUri);
                    await vscode.window.showTextDocument(doc, { preview: false });
                }
            }
        } catch (e) {
            vscode.window.showErrorMessage('Failed to run python-ly formatter: ' + String(e));
        }
    });

    context.subscriptions.push(disposable);
}

function deactivate() { }

module.exports = { activate, deactivate };
