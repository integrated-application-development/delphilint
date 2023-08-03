import * as vscode from "vscode";
import * as path from "path";
import { LintServer, RequestAnalyze } from "./server";
import * as display from "./display";
import * as settings from "./settings";
import { LintError, NoAnalyzableFileError } from "./error";
import {
  getOrPromptActiveProject,
  getProjectOptions,
  promptActiveProject,
} from "./delphiProjectUtils";
import { LintStatusItem } from "./statusBar";

const DELPHI_SOURCE_EXTENSIONS = [".pas", ".dpk", ".dpr"];

let inAnalysis: boolean = false;

export type ServerSupplier = () => Promise<LintServer>;
export type LoggerFunction = (msg: string) => void;

function isFileDelphiSource(filePath: string): boolean {
  return DELPHI_SOURCE_EXTENSIONS.includes(path.extname(filePath));
}

function isFileInProject(filePath: string, baseDir: string): boolean {
  return path
    .normalize(filePath)
    .toLowerCase()
    .startsWith(path.normalize(baseDir).toLowerCase());
}

function getDefaultBaseDir(inputFiles: string[]): string {
  let workspaces = inputFiles
    .map((file) => vscode.workspace.getWorkspaceFolder(vscode.Uri.file(file)))
    .filter((dir) => dir) as vscode.WorkspaceFolder[];

  if (workspaces.length === 0) {
    throw new NoAnalyzableFileError(
      "There are no source files that are analyzable under an open workspace."
    );
  }

  let baseWorkspace = workspaces[0];

  if (workspaces.length > 1) {
    display.showInfo(
      `Files from multiple workspaces are open. Analyzing only files under ${baseWorkspace.name}.`
    );
  }

  return baseWorkspace.uri.fsPath;
}

function constructInputFiles(
  inputFiles: string[],
  baseDir: string,
  dproj?: string
): string[] | undefined {
  if (inputFiles.length === 0) {
    return undefined;
  }

  let sourceFiles = inputFiles.filter(
    (file) => isFileDelphiSource(file) && isFileInProject(file, baseDir)
  );

  if (sourceFiles.length > 0) {
    if (dproj) {
      return [...sourceFiles, dproj];
    } else {
      return sourceFiles;
    }
  }
}

async function doAnalyze(
  server: LintServer,
  issueCollection: vscode.DiagnosticCollection,
  statusUpdate: LoggerFunction,
  msg: RequestAnalyze
) {
  let sourceFiles = msg.inputFiles.filter((file) => isFileDelphiSource(file));

  let flagshipFile = path.basename(sourceFiles[0]);
  let otherSourceFilesMsg =
    sourceFiles.length > 1 ? ` + ${sourceFiles.length - 1} more` : "";
  let analyzingMsg = `Analyzing ${flagshipFile}${otherSourceFilesMsg}...`;
  statusUpdate(analyzingMsg);

  let issues = await server.analyze(msg);
  display.showIssues(issues, issueCollection);

  let issueWord = issues.length === 1 ? "issue" : "issues";
  statusUpdate(`${issues.length} ${issueWord} found`);
}

async function analyzeFiles(
  serverSupplier: ServerSupplier,
  issueCollection: vscode.DiagnosticCollection,
  files: string[],
  statusItem: LintStatusItem
) {
  inAnalysis = true;
  try {
    statusItem.setAction("Selecting project...");

    let apiToken = "";
    let sonarHostUrl = "";
    let projectKey = "";
    let baseDir: string | undefined = undefined;
    let projectPropertiesPath = "";

    let projectChoice = await getOrPromptActiveProject();
    statusItem.setActiveProject(projectChoice);
    let projectFile = projectChoice || undefined;

    if (projectFile) {
      baseDir = path.dirname(projectFile);

      let projectOptions = getProjectOptions(projectFile);
      if (projectOptions) {
        baseDir = projectOptions.baseDir();
        projectPropertiesPath = projectOptions.projectPropertiesPath();
        if (projectOptions.connectedMode()) {
          sonarHostUrl = projectOptions.sonarHostUrl();
          projectKey = projectOptions.projectKey();
          apiToken = projectOptions.apiToken();
        }
      }
    } else {
      baseDir = getDefaultBaseDir(files);
    }

    statusItem.setAction("Checking files...");

    let inputFiles = constructInputFiles(files, baseDir, projectFile);
    if (!inputFiles) {
      throw new NoAnalyzableFileError(
        "There are no selected Delphi files that are analyzable under the current project."
      );
    }

    statusItem.setAction("Starting server...");
    let server = await serverSupplier();
    if (!server.ready()) {
      display.showError("Unable to connect to the DelphiLint server.");
    }

    statusItem.setAction("Initializing server...");
    await server.initialize({
      bdsPath: settings.getBdsPath(),
      apiToken: apiToken,
      compilerVersion: settings.getCompilerVersion(),
      defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
      sonarHostUrl: sonarHostUrl,
    });

    statusItem.setAction("Analyzing...");
    await doAnalyze(server, issueCollection, statusItem.setAction, {
      baseDir: baseDir,
      inputFiles: projectFile ? [...files, projectFile] : files,
      projectKey: projectKey,
      projectPropertiesPath: projectPropertiesPath,
      sonarHostUrl: sonarHostUrl,
      apiToken: apiToken,
    });
  } finally {
    inAnalysis = false;
  }
}

export async function analyzeThisFile(
  serverSupplier: ServerSupplier,
  issueCollection: vscode.DiagnosticCollection
) {
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

  await display.getStatusItem().with(async (statusItem) => {
    try {
      statusItem.startProgress();

      await analyzeFiles(
        serverSupplier,
        issueCollection,
        [currentFileUri.fsPath],
        statusItem
      );
    } catch (err) {
      statusItem.setAction("Analysis failed");
      if (err instanceof LintError) {
        display.showError(err.message);
      } else if (err instanceof Error) {
        display.showError("Unexpected error: " + err.message);
      }
    } finally {
      statusItem.stopProgress();
    }
  });
}

export async function analyzeAllOpenFiles(
  serverSupplier: ServerSupplier,
  issueCollection: vscode.DiagnosticCollection
) {
  let uris = vscode.window.tabGroups.all.flatMap((group) =>
    group.tabs
      .map((tab) => {
        if (typeof tab.input === "object" && tab.input && "uri" in tab.input) {
          return tab.input.uri as vscode.Uri;
        } else {
          return undefined;
        }
      })
      .filter((tab) => tab !== undefined)
  );

  let openTextEditors = uris.map((uri) => (uri as vscode.Uri).fsPath);

  if (openTextEditors.length === 0) {
    display.showError("There are no open files for DelphiLint to analyze.");
    return;
  }

  if (inAnalysis) {
    display.showError("A DelphiLint analysis is already in progress.");
    return;
  }

  await display.getStatusItem().with(async (statusItem) => {
    try {
      statusItem.startProgress();

      await analyzeFiles(
        serverSupplier,
        issueCollection,
        [...openTextEditors],
        statusItem
      );
    } catch (err) {
      statusItem.setAction("Analysis failed");
      if (err instanceof LintError) {
        display.showError(err.message);
      } else if (err instanceof Error) {
        display.showError("Unexpected error: " + err.message);
      }
    } finally {
      statusItem.stopProgress();
    }
  });
}

export async function chooseActiveProject() {
  let activeProject = await promptActiveProject();
  display.getStatusItem().with(async (resource) => {
    resource.setActiveProject(activeProject);
  });
}
