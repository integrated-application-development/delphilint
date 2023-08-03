import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";
import { ProjectOptions } from "./projectOptions";

export type ProjectChoice = string | false;

let activeProjectFile: ProjectChoice | undefined = undefined;

export function getActiveProject(): ProjectChoice | undefined {
  return activeProjectFile;
}

function setActiveProject(value: ProjectChoice) {
  activeProjectFile = value;
}

async function getProjectFilesInWorkspace() {
  return (await vscode.workspace.findFiles("**/*.dproj")).map(
    (uri) => uri.fsPath
  );
}

export async function promptProject(): Promise<ProjectChoice> {
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

  quickPickItems.unshift({
    label: "None (run standalone)",
    description: "",
    path: "__none__",
  });

  let chosenItem = await vscode.window.showQuickPick(quickPickItems, {
    canPickMany: false,
    title: "DelphiLint: Choose Active Delphi Project",
    ignoreFocusOut: true,
  });

  if (!chosenItem || chosenItem.path === "__none__") {
    return false;
  } else {
    return chosenItem.path;
  }
}

export async function promptActiveProject(): Promise<ProjectChoice> {
  let proj = await promptProject();
  setActiveProject(proj);

  return proj;
}

export async function getOrPromptActiveProject(): Promise<ProjectChoice> {
  let proj: ProjectChoice | undefined = getActiveProject();
  if (proj === undefined) {
    proj = await promptProject();
    setActiveProject(proj);
  }

  return proj;
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
