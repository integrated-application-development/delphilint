import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";
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

  let quickPickItems = projectFiles
    .map((file) => {
      let basename = path.basename(file);

      let containingFolders = vscode.workspace.workspaceFolders?.filter(
        (workspace) =>
          file.toLowerCase().startsWith(workspace.uri.fsPath.toLowerCase())
      );

      let longPath: string = file.replace(path.sep + basename, "");
      if (containingFolders && containingFolders.length > 0) {
        longPath = path.relative(containingFolders[0].uri.fsPath, longPath);
      }

      return {
        label: basename,
        description: longPath,
        path: file,
      };
    })
    .sort((a, b) => a.label.localeCompare(b.label));

  let chosenItem = await vscode.window.showQuickPick(quickPickItems, {
    canPickMany: false,
    title: "DelphiLint: Choose Active Delphi Project",
    ignoreFocusOut: true,
  });

  if (chosenItem) {
    return chosenItem.path;
  }
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
