import * as vscode from 'vscode';
import { LintServer } from './server';
import { analyzeThisFile } from './command';
import { registerVersion } from './settings';

let server: LintServer;

export function activate(context: vscode.ExtensionContext) {
  registerVersion(context.extension.packageJSON.version);

  server = new LintServer();

  let lintIssueCollection = vscode.languages.createDiagnosticCollection("delphilint");
  context.subscriptions.push(lintIssueCollection);

  let analyzeThisFileCommand = vscode.commands.registerCommand(
    'delphilint-vscode.analyzeThisFile',
    async () => await analyzeThisFile(server, lintIssueCollection)
  );
  context.subscriptions.push(analyzeThisFileCommand);
}

export function deactivate() {
  server.quit();
}