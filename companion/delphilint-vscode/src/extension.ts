import * as vscode from "vscode";
import { LintServer } from "./server";
import { analyzeThisFile, chooseActiveDproj } from "./command";
import * as settings from "./settings";

let server: LintServer | undefined;
let serverPromise: Promise<LintServer> | undefined;

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
  let s = new LintServer();
  await s.startExternalServer(
    settings.getServerJar(),
    settings.getJavaExe(),
    settings.SETTINGS_DIR,
    settings.getShowConsole()
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
    async () => await analyzeThisFile(await getServer(), lintIssueCollection)
  );
  context.subscriptions.push(analyzeThisFileCommand);

  let chooseActiveDprojCommand = vscode.commands.registerCommand(
    "delphilint-vscode.chooseActiveDproj",
    async () => await chooseActiveDproj()
  );
  context.subscriptions.push(chooseActiveDprojCommand);
}

export function deactivate() {
  if (server) {
    server.quit();
  }
}
