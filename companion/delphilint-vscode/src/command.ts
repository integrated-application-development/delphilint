import * as vscode from 'vscode';
import * as path from 'path';
import { LintServer, LintIssue } from './server';
import * as display from './display';
import * as settings from './settings';

export function analyzeThisFile(server: LintServer, issueCollection: vscode.DiagnosticCollection) {
  let activeTextEditor = vscode.window.activeTextEditor;
  if (activeTextEditor === undefined) {
    display.showError("There is no active file to analyze.");
    return;
  }

  let currentFileUri = activeTextEditor.document.uri;

  display.showInfo("Initializing server...");
  server.initialize(
    {
      bdsPath: vscode.workspace.getConfiguration().get("delphilint-vscode.bdsPath") as string,
      apiToken: "",
      compilerVersion: vscode.workspace.getConfiguration().get("delphilint-vscode.compilerVersion") as string,
      defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
      sonarHostUrl: ""
    },
    () => {
      display.showInfo("Analyzing " + currentFileUri.fsPath + "...");
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
          display.showIssues(issues, issueCollection);
        },
        display.showError);
    },
    display.showError);
}
