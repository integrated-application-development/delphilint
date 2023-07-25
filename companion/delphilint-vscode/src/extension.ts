import * as vscode from 'vscode';
import { LintIssue, LintServer } from './server';
import * as path from 'path';

let lintIssueCollection: vscode.DiagnosticCollection;
let server = new LintServer(14000);
let settingsPath = path.join(process.env.APPDATA as string, "DelphiLint");
let sonarDelphiJarPath = path.join(settingsPath, "sonar-delphi-plugin.jar");

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

		let currentFileUri = activeTextEditor.document.uri;

		vscode.window.showInformationMessage("Initializing server...");
		server.initialize(
			{
				bdsPath: vscode.workspace.getConfiguration().get("delphilint-vscode.bdsPath") as string,
				apiToken: "",
				compilerVersion: vscode.workspace.getConfiguration().get("delphilint-vscode.compilerVersion") as string,
				defaultSonarDelphiJarPath: sonarDelphiJarPath,
				sonarHostUrl: ""
			},
			() => {
				vscode.window.showInformationMessage("Analyzing " + currentFileUri.fsPath + "...");
				server.analyze(
					{
						apiToken: "",
						baseDir: path.parse(currentFileUri.fsPath).dir,
						inputFiles: [currentFileUri.fsPath],
						projectKey: "",
						projectPropertiesPath: "",
						sonarHostUrl: ""
					},
					(issues: LintIssue[]) => {
						lintIssueCollection.set(currentFileUri, undefined);

						let diagnostics: vscode.Diagnostic[] = [];

						for(const element of issues) {
							let issue = element;
							if(issue.range) {
								let issueRange = new vscode.Range(issue.range.startLine - 1, issue.range.startOffset, issue.range.endLine - 1, issue.range.endOffset);
								diagnostics.push(new vscode.Diagnostic(issueRange, issue.message, vscode.DiagnosticSeverity.Warning));
							}
						}

						lintIssueCollection.set(currentFileUri, diagnostics);
					},
					(err) => { vscode.window.showErrorMessage(err); });
			},
			(err) => { vscode.window.showErrorMessage(err); });
		});
	context.subscriptions.push(analyzeThisFileCommand);
}