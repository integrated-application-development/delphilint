import * as vscode from 'vscode';
import { LintServer } from './server';


let lintIssueCollection: vscode.DiagnosticCollection;
let server = new LintServer(14000);

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

		server.initialize(
			{
				bdsPath: "",
				apiToken: "",
				compilerVersion: "VER350",
				defaultSonarDelphiJarPath: "",
				sonarHostUrl: ""
			},
			() => { vscode.window.showInformationMessage("Server initialized!"); },
			(err) => { vscode.window.showErrorMessage(err); });
		});
	context.subscriptions.push(analyzeThisFileCommand);
}

export function deactivate() {}
