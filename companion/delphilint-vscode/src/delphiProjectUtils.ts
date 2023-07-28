import * as vscode from "vscode";
import * as fs from "fs";
import { ProjectOptions } from "./projectOptions";

let activeProjectFile: string | undefined = undefined;

export function getActiveProject(): string | undefined {
  return activeProjectFile;
}

function setActiveProject(value: string | undefined) {
  activeProjectFile = value;
}

async function getProjectFilesInWorkspace() {
  return (await vscode.workspace.findFiles("**/*.dproj")).map(
    (uri) => uri.fsPath
  );
}

export async function promptProject(): Promise<string | undefined> {
  let projectFiles: string[] = await getProjectFilesInWorkspace();

  return await vscode.window.showQuickPick(projectFiles, {
    canPickMany: false,
    title: "Choose Active Delphi Project for DelphiLint",
    ignoreFocusOut: true,
  });
}

export async function promptActiveProject(): Promise<string | undefined> {
  let proj = await promptProject();
  if (proj) {
    setActiveProject(proj);
  }

  return getActiveProject();
}

export async function getOrPromptActiveProject(): Promise<string | undefined> {
  if (!activeProjectFile) {
    setActiveProject(await promptProject());
  }

  return getActiveProject();
}

export function getProjectOptions(
  projectFilePath: string
): ProjectOptions | undefined {
  let projectOptionsPath = projectFilePath
    .toLowerCase()
    .replace(".dproj", ".delphilint");
  if (fs.existsSync(projectOptionsPath)) {
    return new ProjectOptions(projectOptionsPath);
  }
}
