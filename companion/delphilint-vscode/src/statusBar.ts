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
import * as path from "path";
import { ProjectChoice } from "./delphiProjectUtils";

export class LintStatusItem {
  private readonly statusItem: vscode.StatusBarItem;
  private activeProject?: ProjectChoice;
  private action?: string;
  private progress: boolean;

  constructor(activeProject?: ProjectChoice) {
    this.statusItem = vscode.window.createStatusBarItem(
      vscode.StatusBarAlignment.Left,
      0
    );
    this.activeProject = activeProject;
    this.action = undefined;
    this.progress = false;

    this.statusItem.name = "DelphiLint";
    this.statusItem.command = "delphilint-vscode.chooseActiveProject";
    this.statusItem.show();

    this.update();

    this.setActiveProject = this.setActiveProject.bind(this);
    this.setAction = this.setAction.bind(this);
  }

  update() {
    let activeProjectText: string;
    if (this.activeProject === undefined) {
      activeProjectText = "No project selected";
    } else {
      activeProjectText = this.activeProject
        ? path.basename(this.activeProject).replace(".dproj", "")
        : "Standalone";
    }

    const iconText = this.progress ? "$(loading~spin) " : "";
    const actionText = this.action ?? "Idle";

    this.statusItem.text = `${iconText}DelphiLint: ${actionText} (${activeProjectText})`;
  }

  setActiveProject(activeProject?: ProjectChoice) {
    this.activeProject = activeProject;
    this.update();
  }

  setAction(action?: string) {
    this.action = action;
    this.update();
  }

  startProgress() {
    this.progress = true;
    this.update();
  }

  stopProgress() {
    this.progress = false;
    this.update();
  }
}
