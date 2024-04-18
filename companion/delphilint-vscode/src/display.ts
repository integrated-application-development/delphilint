/*
 * DelphiLint VSCode
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
import * as vscode from "vscode";
import { LintIssue } from "./server";
import { getActiveProject } from "./delphiProjectUtils";
import { LintStatusItem } from "./statusBar";
import { Exclusive } from "./resource";

let statusItem: Exclusive<LintStatusItem> | undefined;

export function getStatusItem(): Exclusive<LintStatusItem> {
  if (!statusItem) {
    const statusItemRaw = new LintStatusItem(getActiveProject());
    statusItem = new Exclusive(statusItemRaw);
  }

  return statusItem;
}

export function showIssues(
  issues: LintIssue[],
  issueCollection: vscode.DiagnosticCollection
) {
  const files: Map<string, LintIssue[]> = issues.reduce(
    (aggregate, issue) =>
      aggregate.set(issue.file, [...(aggregate.get(issue.file) ?? []), issue]),
    new Map<string, LintIssue[]>()
  );

  for (const [fsPath, fileIssues] of files.entries()) {
    const uri = vscode.Uri.file(fsPath);
    const diagnostics: vscode.Diagnostic[] = [];

    for (const issue of fileIssues) {
      if (issue.range) {
        const diagnostic = new vscode.Diagnostic(
          new vscode.Range(
            issue.range.startLine - 1,
            issue.range.startOffset,
            issue.range.endLine - 1,
            issue.range.endOffset
          ),
          issue.message,
          vscode.DiagnosticSeverity.Warning
        );
        diagnostic.code = issue.ruleKey;

        diagnostics.push(diagnostic);
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
