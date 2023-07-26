import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import { LintServer, RequestAnalyze } from "./server";
import * as display from "./display";
import * as settings from "./settings";
import { ProjectOptions } from "./project";

const DELPHI_SOURCE_EXTENSIONS = [".pas", ".dpk", ".dpr"];

let inAnalysis: boolean = false;
let activeDproj: string | undefined = undefined;

async function getDprojFilesInWorkspace() {
  return (await vscode.workspace.findFiles("**/*.dproj")).map(
    (uri) => uri.fsPath
  );
}

export async function chooseActiveDproj() {
  let dprojFiles: string[] = await getDprojFilesInWorkspace();

  let newDproj = await vscode.window.showQuickPick(dprojFiles, {
    canPickMany: false,
    title: "Choose Active Delphi Project for DelphiLint",
    ignoreFocusOut: true,
  });

  if (newDproj) {
    activeDproj = newDproj;
  }
}

async function getActiveDproj(): Promise<string> {
  if (!activeDproj) {
    await chooseActiveDproj();
  }

  if (activeDproj) {
    return activeDproj;
  } else {
    throw new Error("There is no active Delphi project.");
  }
}

function getProjectOptionsForDproj(
  dprojPath: string
): ProjectOptions | undefined {
  let projectOptionsPath = dprojPath
    .toLowerCase()
    .replace(".dproj", ".delphilint");
  if (fs.existsSync(projectOptionsPath)) {
    return new ProjectOptions(projectOptionsPath);
  }
}

async function analyzeFiles(
  server: LintServer,
  issueCollection: vscode.DiagnosticCollection,
  msg: RequestAnalyze
) {
  let sourceFiles = msg.inputFiles.filter((file) =>
    DELPHI_SOURCE_EXTENSIONS.includes(path.extname(file).toLowerCase())
  );

  if (sourceFiles.length === 0) {
    display.showError(
      "There are no Delphi source files for DelphiLint to analyze."
    );
    return;
  }

  let flagshipFile = path.basename(sourceFiles[0]);
  let otherSourceFilesMsg =
    sourceFiles.length > 1 ? ` + ${sourceFiles.length - 1} more` : "";

  let dprojFile = await getActiveDproj();
  if (dprojFile) {
    msg.inputFiles.push(dprojFile);

    let projectOptions = getProjectOptionsForDproj(dprojFile);
    if (projectOptions) {
      msg.baseDir = projectOptions.baseDir();
      msg.projectPropertiesPath = projectOptions.projectPropertiesPath();
      if (projectOptions.connectedMode()) {
        msg.sonarHostUrl = projectOptions.sonarHostUrl();
        msg.projectKey = projectOptions.projectKey();
        msg.apiToken = projectOptions.apiToken();
      }
    }
  }

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      cancellable: false,
      title: `Analyzing ${flagshipFile}${otherSourceFilesMsg}...`,
    },
    async (progress) => {
      try {
        let issues = await server.analyze(msg);
        display.showIssues(issues, issueCollection);

        let issueWord = issues.length === 1 ? "issue" : "issues";
        let fileWord = sourceFiles.length === 1 ? "file" : "files";
        display.showInfo(
          `${issues.length} ${issueWord} found in ${sourceFiles.length} ${fileWord}.`
        );
      } catch (err) {
        display.showError(err as string);
      }
    }
  );
}

async function analyzeFilesWithProjectOptions(
  server: LintServer,
  issueCollection: vscode.DiagnosticCollection,
  files: string[]
) {
  let apiToken = "";
  let sonarHostUrl = "";
  let projectKey = "";
  let baseDir = "";
  let projectPropertiesPath = "";

  let dproj = await getActiveDproj();
  baseDir = path.dirname(dproj);

  let projectOptions = getProjectOptionsForDproj(dproj);
  if (projectOptions) {
    baseDir = projectOptions.baseDir();
    projectPropertiesPath = projectOptions.projectPropertiesPath();
    if (projectOptions.connectedMode()) {
      sonarHostUrl = projectOptions.sonarHostUrl();
      projectKey = projectOptions.projectKey();
      apiToken = projectOptions.apiToken();
    }
  }

  await server.initialize({
    bdsPath: settings.getBdsPath(),
    apiToken: apiToken,
    compilerVersion: settings.getCompilerVersion(),
    defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
    sonarHostUrl: sonarHostUrl,
  });

  await analyzeFiles(server, issueCollection, {
    baseDir: baseDir,
    inputFiles: [...files, dproj],
    projectKey: projectKey,
    projectPropertiesPath: projectPropertiesPath,
    sonarHostUrl: sonarHostUrl,
    apiToken: apiToken,
  });
}

export async function analyzeThisFile(
  server: LintServer,
  issueCollection: vscode.DiagnosticCollection
) {
  if (!server.ready()) {
    display.showError("The DelphiLint server is not connected.");
  }

  let activeTextEditor = vscode.window.activeTextEditor;
  if (!activeTextEditor) {
    display.showError("There is no active file for DelphiLint to analyze.");
    return;
  }

  if (inAnalysis) {
    display.showError("A DelphiLint analysis is already in progress.");
    return;
  }

  let currentFileUri = activeTextEditor.document.uri;

  try {
    await analyzeFilesWithProjectOptions(server, issueCollection, [
      currentFileUri.fsPath,
    ]);
  } catch (err) {
    display.showError(err as string);
  }
}
