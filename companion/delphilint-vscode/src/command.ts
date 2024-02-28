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
  const workspaces = inputFiles
    .map((file) => vscode.workspace.getWorkspaceFolder(vscode.Uri.file(file)))
    .filter((dir) => dir) as vscode.WorkspaceFolder[];

  if (workspaces.length === 0) {
    throw new NoAnalyzableFileError(
      "There are no source files that are analyzable under an open workspace."
    );
  }

  const baseWorkspace = workspaces[0];

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

  const sourceFiles = inputFiles.filter(
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
  const sourceFiles = msg.inputFiles.filter((file) => isFileDelphiSource(file));

  const flagshipFile = path.basename(sourceFiles[0]);
  const otherSourceFilesMsg =
    sourceFiles.length > 1 ? ` + ${sourceFiles.length - 1} more` : "";
  const analyzingMsg = `Analyzing ${flagshipFile}${otherSourceFilesMsg}...`;
  statusUpdate(analyzingMsg);

  const issues = await server.analyze(msg);
  display.showIssues(issues, issueCollection);

  const issueWord = issues.length === 1 ? "issue" : "issues";
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
    let baseDir: string | null = null;
    let projectPropertiesPath = "";

    const projectChoice = await getOrPromptActiveProject();
    statusItem.setActiveProject(projectChoice);
    const projectFile = projectChoice || null;

    if (projectFile) {
      baseDir = path.dirname(projectFile);

      const projectOptions = getProjectOptions(projectFile);
      if (projectOptions) {
        baseDir = projectOptions.baseDir();
        projectPropertiesPath = projectOptions.projectPropertiesPath();
        if (projectOptions.connectedMode()) {
          sonarHostUrl = projectOptions.sonarHostUrl();
          projectKey = projectOptions.projectKey();
          console.log(settings.getSonarTokens());
          apiToken =
            settings.getSonarTokens()?.[projectOptions.sonarHostUrl()]?.[
              projectOptions.projectKey()
            ] ?? "";
        }
      }
    } else {
      baseDir = getDefaultBaseDir(files);
    }

    statusItem.setAction("Checking files...");

    const inputFiles = constructInputFiles(
      files,
      baseDir,
      projectFile ?? undefined
    );
    if (!inputFiles) {
      throw new NoAnalyzableFileError(
        "There are no selected Delphi files that are analyzable under the current project."
      );
    }

    statusItem.setAction("Starting server...");
    const server = await serverSupplier();

    statusItem.setAction("Initializing server...");
    await server.initialize({
      bdsPath: settings.getBdsPath(),
      apiToken,
      compilerVersion: settings.getCompilerVersion(),
      defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
      sonarHostUrl,
    });

    statusItem.setAction("Analyzing...");
    await doAnalyze(server, issueCollection, statusItem.setAction, {
      baseDir,
      inputFiles: projectFile ? [...inputFiles, projectFile] : inputFiles,
      projectKey,
      projectPropertiesPath,
      sonarHostUrl,
      apiToken,
    });
  } finally {
    inAnalysis = false;
  }
}

export async function analyzeThisFile(
  serverSupplier: ServerSupplier,
  issueCollection: vscode.DiagnosticCollection
) {
  const activeTextEditor = vscode.window.activeTextEditor;
  if (!activeTextEditor) {
    display.showError("There is no active file for DelphiLint to analyze.");
    return;
  }

  if (inAnalysis) {
    display.showError("A DelphiLint analysis is already in progress.");
    return;
  }

  const currentFileUri = activeTextEditor.document.uri;

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
  const uris = vscode.window.tabGroups.all.flatMap((group) =>
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

  const openTextEditors = uris.map((uri) => (uri as vscode.Uri).fsPath);

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
  const activeProject = await promptActiveProject();
  display.getStatusItem().with(async (resource) => {
    resource.setActiveProject(activeProject);
  });
}
