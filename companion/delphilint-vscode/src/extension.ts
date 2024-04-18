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
import { LintServer } from "./server";
import {
  analyzeAllOpenFiles,
  analyzeThisFile,
  chooseActiveProject,
  clearThisFile,
} from "./command";
import { getStatusItem } from "./display";
import * as settings from "./settings";

let server: LintServer | undefined;
let serverPromise: Promise<LintServer> | undefined;
let serverOutputChannel: vscode.OutputChannel | undefined;

async function getServer() {
  if (!server) {
    if (!serverPromise) {
      serverPromise = createServer();
    }
    const s = await serverPromise;
    serverPromise = undefined;
    return s;
  } else {
    return server;
  }
}

async function createServer(): Promise<LintServer> {
  if (!serverOutputChannel) {
    serverOutputChannel =
      vscode.window.createOutputChannel("DelphiLint Server");
  }

  const s = new LintServer(
    settings.getServerJar(),
    settings.getJavaExe(),
    settings.SETTINGS_DIR,
    (msg) => serverOutputChannel?.append(msg)
  );
  await s.startExternalServer();
  server = s;

  return server;
}

export function activate(context: vscode.ExtensionContext) {
  settings.registerVersion(context.extension.packageJSON.version);

  const lintIssueCollection =
    vscode.languages.createDiagnosticCollection("delphilint");
  context.subscriptions.push(lintIssueCollection);

  const analyzeThisFileCommand = vscode.commands.registerCommand(
    "delphilint-vscode.analyzeThisFile",
    async () => analyzeThisFile(getServer, lintIssueCollection)
  );
  context.subscriptions.push(analyzeThisFileCommand);

  const analyzeAllOpenFilesCommand = vscode.commands.registerCommand(
    "delphilint-vscode.analyzeAllOpenFiles",
    async () => analyzeAllOpenFiles(getServer, lintIssueCollection)
  );
  context.subscriptions.push(analyzeAllOpenFilesCommand);

  const clearThisFileCommand = vscode.commands.registerCommand(
    "delphilint-vscode.clearThisFile",
    () => clearThisFile(lintIssueCollection)
  );

  const chooseActiveProjectCommand = vscode.commands.registerCommand(
    "delphilint-vscode.chooseActiveProject",
    async () => chooseActiveProject()
  );
  context.subscriptions.push(chooseActiveProjectCommand);

  getStatusItem();
}

export function deactivate() {
  if (server) {
    server.quit();
  }
}
