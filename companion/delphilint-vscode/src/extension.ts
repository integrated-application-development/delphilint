import * as vscode from "vscode";
import { LintServer } from "./server";
import {
  analyzeAllOpenFiles,
  analyzeThisFile,
  chooseActiveProject,
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
    let s = await serverPromise;
    serverPromise = undefined;
    return s;
  } else {
    return server;
  }
}

async function createServer(): Promise<LintServer> {
  if (serverOutputChannel) {
    serverOutputChannel.dispose();
  }
  serverOutputChannel = vscode.window.createOutputChannel("DelphiLint Server");

  let s = new LintServer();
  await s.startExternalServer(
    settings.getServerJar(),
    settings.getJavaExe(),
    settings.SETTINGS_DIR,
    (msg) => serverOutputChannel?.append(msg)
  );
  server = s;
  return s;
}

export function activate(context: vscode.ExtensionContext) {
  settings.registerVersion(context.extension.packageJSON.version);

  let lintIssueCollection =
    vscode.languages.createDiagnosticCollection("delphilint");
  context.subscriptions.push(lintIssueCollection);

  let analyzeThisFileCommand = vscode.commands.registerCommand(
    "delphilint-vscode.analyzeThisFile",
    async () => await analyzeThisFile(getServer, lintIssueCollection)
  );
  context.subscriptions.push(analyzeThisFileCommand);

  let analyzeAllOpenFilesCommand = vscode.commands.registerCommand(
    "delphilint-vscode.analyzeAllOpenFiles",
    async () => await analyzeAllOpenFiles(getServer, lintIssueCollection)
  );
  context.subscriptions.push(analyzeAllOpenFilesCommand);

  let chooseActiveProjectCommand = vscode.commands.registerCommand(
    "delphilint-vscode.chooseActiveProject",
    async () => await chooseActiveProject()
  );
  context.subscriptions.push(chooseActiveProjectCommand);

  getStatusItem();
}

export function deactivate() {
  if (server) {
    server.quit();
  }
}
