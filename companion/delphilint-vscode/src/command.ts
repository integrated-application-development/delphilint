import * as vscode from 'vscode';
import * as path from 'path';
import { LintServer, LintIssue, RequestAnalyze } from './server';
import * as display from './display';
import * as settings from './settings';

const DELPHI_SOURCE_EXTENSIONS = [".pas", ".dpk", ".dpr"];

let inAnalysis: boolean = false;

async function analyzeFiles(server: LintServer, issueCollection: vscode.DiagnosticCollection, msg: RequestAnalyze) {
  let sourceFiles = msg.inputFiles.filter(file => DELPHI_SOURCE_EXTENSIONS.includes(path.extname(file).toLowerCase()));

  if(sourceFiles.length === 0) {
    display.showError("There are no Delphi source files for DelphiLint to analyze.");
    return;
  }

  let flagshipFile = path.basename(sourceFiles[0]);
  let otherSourceFilesMsg = sourceFiles.length > 1 ? ` + ${(sourceFiles.length - 1)} more` : "";

  await vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    cancellable: false,
    title: `Analyzing ${flagshipFile}${otherSourceFilesMsg}...`
  }, async (progress) => {
    try {
      let issues = await server.analyze(msg);
      display.showIssues(issues, issueCollection);

      let issueWord = issues.length === 1 ? "issue" : "issues";
      let fileWord = sourceFiles.length ===1 ? "file" : "files";
      display.showInfo(`${issues.length} ${issueWord} found in ${(sourceFiles.length)} ${fileWord}.`);
    } catch(err) {
      display.showError(err as string);
    }
  });
}

export async function analyzeThisFile(server: LintServer, issueCollection: vscode.DiagnosticCollection) {
  if(!server.ready()) {
    display.showError("The DelphiLint server is not connected.");
  }

  let activeTextEditor = vscode.window.activeTextEditor;
  if (!activeTextEditor) {
    display.showError("There is no active file for DelphiLint to analyze.");
    return;
  }

  if(inAnalysis) {
    display.showError("A DelphiLint analysis is already in progress.");
    return;
  }

  let currentFileUri = activeTextEditor.document.uri;

  try {
    await server.initialize({
      bdsPath: settings.getBdsPath(),
      apiToken: "",
      compilerVersion: settings.getCompilerVersion(),
      defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
      sonarHostUrl: ""
    });

    await analyzeFiles(server, issueCollection, {
      baseDir: path.parse(currentFileUri.fsPath).dir,
      inputFiles: [currentFileUri.fsPath],
      projectKey: "",
      projectPropertiesPath: "",
      sonarHostUrl: "",
      apiToken: "",
    });
  } catch(err) {
    display.showError(err as string);
  }
}
