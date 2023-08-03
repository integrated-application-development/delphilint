import * as vscode from "vscode";
import * as path from "path";
import { ProjectChoice } from "./delphiProjectUtils";

export class LintStatusItem {
  private statusItem: vscode.StatusBarItem;
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

    let iconText = this.progress ? "$(loading~spin) " : "";
    let actionText = this.action ?? "Idle";

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
