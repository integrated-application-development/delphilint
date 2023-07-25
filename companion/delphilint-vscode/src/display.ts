import * as vscode from 'vscode';
import { LintIssue } from './server';

export function showIssues(issues: LintIssue[], issueCollection: vscode.DiagnosticCollection) {
  let files: Map<string, LintIssue[]> = issues.reduce(
    (aggregate, issue) => aggregate.set(issue.file, [...aggregate.get(issue.file) ?? [], issue]),
    new Map<string, LintIssue[]>()
  );

  for(const fsPath of files.keys()) {
    const uri = vscode.Uri.file(fsPath);
    let diagnostics: vscode.Diagnostic[] = [];

    for(const element of files.get(fsPath) as LintIssue[]) {
      let issue = element;
      if(issue.range) {
        let issueRange = new vscode.Range(issue.range.startLine - 1, issue.range.startOffset, issue.range.endLine - 1, issue.range.endOffset);
        diagnostics.push(new vscode.Diagnostic(issueRange, issue.message, vscode.DiagnosticSeverity.Warning));
      }
    }

    issueCollection.set(uri, diagnostics);
  }
}

export function showInfo(msg: string) {
  vscode.window.showInformationMessage(msg);
}

export function showError(err: string) {
  vscode.window.showErrorMessage(err);
}