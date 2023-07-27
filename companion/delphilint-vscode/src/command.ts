import * as vscode from "vscode";
import * as path from "path";
import { LintServer, RequestAnalyze } from "./server";
import * as display from "./display";
import * as settings from "./settings";
import {
  LintError,
  NoAnalyzableFileError,
  NoDelphiProjectError,
} from "./error";
import {
  getOrPromptActiveProject,
  getProjectOptions,
} from "./delphiProjectUtils";

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

function constructInputFiles(
  inputFiles: string[],
  baseDir: string,
  dproj: string
): string[] | undefined {
  let sourceFiles = inputFiles.filter(
    (file) => isFileDelphiSource(file) && isFileInProject(file, baseDir)
  );

  if (sourceFiles.length > 0) {
    return [...sourceFiles, dproj];
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
  let fileWord = sourceFiles.length === 1 ? "file" : "files";

  display.showInfo(
    `${issues.length} ${issueWord} found in ${sourceFiles.length} ${fileWord}.`
  );
}

async function analyzeFiles(
  server: LintServer,
  issueCollection: vscode.DiagnosticCollection,
  files: string[],
  statusUpdate: LoggerFunction
) {
  inAnalysis = true;
  try {
    statusUpdate("Selecting project...");

    let apiToken = "";
    let sonarHostUrl = "";
    let projectKey = "";
    let baseDir = "";
    let projectPropertiesPath = "";

    let projectFileTmp = await getOrPromptActiveProject();
    if (!projectFileTmp) {
      throw new NoDelphiProjectError("There is no active Delphi project.");
    }
    let projectFile = projectFileTmp;

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

    statusUpdate("Checking files...");

    let inputFiles = constructInputFiles(files, baseDir, projectFile);
    if (!inputFiles) {
      throw new NoAnalyzableFileError(
        "There are no selected Delphi files that are analyzable under the current project."
      );
    }

    statusUpdate("Initializing server...");

    await server.initialize({
      bdsPath: settings.getBdsPath(),
      apiToken: apiToken,
      compilerVersion: settings.getCompilerVersion(),
      defaultSonarDelphiJarPath: settings.getSonarDelphiJar(),
      sonarHostUrl: sonarHostUrl,
    });

    await doAnalyze(server, issueCollection, display.setStatusAction, {
      baseDir: baseDir,
      inputFiles: [...files, projectFile],
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

  try {
    display.setStatusAction("Starting server...");
    let server = await serverSupplier();

    if (!server.ready()) {
      display.showError("Unable to connect to the DelphiLint server.");
    }

    await analyzeFiles(
      server,
      issueCollection,
      [currentFileUri.fsPath],
      display.setStatusAction
    );
  } catch (err) {
    if (err instanceof LintError) {
      display.showError(err.message);
    } else if (err instanceof Error) {
      display.showError("Unexpected error: " + err.message);
    }
  } finally {
    display.setStatusAction();
  }
}
