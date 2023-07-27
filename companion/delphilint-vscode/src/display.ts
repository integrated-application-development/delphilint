import * as vscode from "vscode";
import { LintIssue } from "./server";
import {
  addActiveProjectChangedListener,
  getActiveProject,
} from "./delphiProjectUtils";
import { LintStatusItem } from "./statusBar";

let statusItem: LintStatusItem | undefined;

export function getStatusItem(): LintStatusItem {
  if (!statusItem) {
    statusItem = new LintStatusItem(getActiveProject());
    addActiveProjectChangedListener(statusItem.setActiveProject);
  }

  return statusItem;
}

export function setStatusAction(action?: string) {
  let statusItem = getStatusItem();
  statusItem.setAction(action);
}

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
