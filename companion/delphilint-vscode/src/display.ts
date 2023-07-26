import * as vscode from "vscode";
import * as path from "path";
import { LintIssue } from "./server";

export function showIssues(
  issues: LintIssue[],
  issueCollection: vscode.DiagnosticCollection
) {
  let files: Map<string, LintIssue[]> = issues.reduce(
    (aggregate, issue) =>
      aggregate.set(issue.file, [...(aggregate.get(issue.file) ?? []), issue]),
    new Map<string, LintIssue[]>()
  );

  for (const [fsPath, fileIssues] of files.entries()) {
    const uri = vscode.Uri.file(fsPath);
    let diagnostics: vscode.Diagnostic[] = [];

    for (const issue of fileIssues) {
      if (issue.range) {
        let issueRange = new vscode.Range(
          issue.range.startLine - 1,
          issue.range.startOffset,
          issue.range.endLine - 1,
          issue.range.endOffset
        );
        diagnostics.push(
          new vscode.Diagnostic(
            issueRange,
            issue.message,
            vscode.DiagnosticSeverity.Warning
          )
        );
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