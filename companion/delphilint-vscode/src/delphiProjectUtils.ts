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
  const projectFiles: string[] = await getProjectFilesInWorkspace();

  const quickPickItems = projectFiles
    .map((file) => {
      const basename = path.basename(file);

      const containingFolders = vscode.workspace.workspaceFolders?.filter(
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

  const chosenItem = await vscode.window.showQuickPick(quickPickItems, {
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
  const proj = await promptProject();
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
  const projectOptionsPath = projectFilePath
    .toLowerCase()
    .replace(".dproj", ".delphilint");
  if (fs.existsSync(projectOptionsPath)) {
    return new ProjectOptions(projectOptionsPath);
  }
}
