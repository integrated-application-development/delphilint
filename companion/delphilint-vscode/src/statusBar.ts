import * as vscode from "vscode";
import * as path from "path";

export class LintStatusItem {
  private statusItem: vscode.StatusBarItem;
  private activeProject?: string;
  private action?: string;

  constructor(activeProject?: string, action?: string) {
    this.statusItem = vscode.window.createStatusBarItem(
      vscode.StatusBarAlignment.Left,
      0
    );
    this.activeProject = activeProject;
    this.action = action;

    this.statusItem.name = "DelphiLint";
    this.statusItem.command = "delphilint-vscode.chooseActiveProject";
    this.statusItem.show();

    this.update();

    this.setActiveProject = this.setActiveProject.bind(this);
  }

  update() {
    let activeProjectText = this.activeProject
      ? path.basename(this.activeProject)
      : "No project selected";

    let iconText = this.action ? "$(loading~spin) " : "";
    let actionText = this.action ?? "Idle";

    this.statusItem.text = `${iconText}DelphiLint: ${actionText} (${activeProjectText})`;
  }

  setActiveProject(activeProject?: string) {
    this.activeProject = activeProject;
    this.update();
  }

  setAction(action?: string) {
    this.action = action;
    this.update();
  }
}
