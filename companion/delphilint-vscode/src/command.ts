import * as vscode from 'vscode';
import * as path from 'path';
import { LintServer, LintIssue } from './server';
import * as display from './display';
import * as settings from './settings';

export function analyzeThisFile(server: LintServer, issueCollection: vscode.DiagnosticCollection) {
  if(!server.ready()) {
    display.showError("The DelphiLint server is not connected.");
  }

  let activeTextEditor = vscode.window.activeTextEditor;
  if (activeTextEditor === undefined) {
    display.showError("There is no active file for DelphiLint to analyze.");
    return;
  }

  let currentFileUri = activeTextEditor.document.uri;

  server.initialize({
      bdsPath: settings.getBdsPath(),
      apiToken: "",
      compilerVersion: settings.getCompilerVersion(),
      defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
      sonarHostUrl: ""
  })
    .then(() => {
      display.showInfo(`Analyzing ${(path.basename(currentFileUri.fsPath))}...`);
      server.analyze({
          apiToken: "",
          baseDir: path.parse(currentFileUri.fsPath).dir,
          inputFiles: [currentFileUri.fsPath],
          projectKey: "",
          projectPropertiesPath: "",
          sonarHostUrl: ""
      })
        .then((issues: LintIssue[]) => {
          display.showIssues(issues, issueCollection);
        })
        .catch(display.showError);
    })
    .catch(display.showError);
}
