// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

let lintIssueCollection: vscode.DiagnosticCollection;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
	console.log('DelphiLint activated.');

	lintIssueCollection = vscode.languages.createDiagnosticCollection("delphilint");
	context.subscriptions.push(lintIssueCollection);

	let analyzeThisFileCommand = vscode.commands.registerCommand('delphilint-vscode.analyzeThisFile', () => {
		let activeTextEditor = vscode.window.activeTextEditor;
		if (activeTextEditor === undefined) {
			vscode.window.showErrorMessage("There is no active file to analyze.");
			return;
		}

		let path = activeTextEditor.document.fileName;

		vscode.window.showInformationMessage("Analyzing " + path + "...");
		let diagnostics: vscode.Diagnostic[] = [];

		let issueRange = new vscode.Range(0, 0, 0, 10);
		diagnostics.push(new vscode.Diagnostic(issueRange, "Test message", vscode.DiagnosticSeverity.Warning));

		lintIssueCollection.set(activeTextEditor.document.uri, diagnostics);
	});
	context.subscriptions.push(analyzeThisFileCommand);
}

// This method is called when your extension is deactivated
export function deactivate() {}
