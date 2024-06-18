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

  for (const filePath of sourceFiles) {
    issueCollection.set(vscode.Uri.file(filePath), undefined);
  }

  display.showIssues(issues, issueCollection);

  const issueWord = issues.length === 1 ? "issue" : "issues";
  statusUpdate(`${issues.length} ${issueWord} found`);
}

type Configuration = {
  apiToken: string;
  sonarHostUrl: string;
  projectKey: string;
  baseDir: string;
  projectPropertiesPath: string;
};

function getDefaultConfiguration(): Configuration {
  return {
    apiToken: "",
    sonarHostUrl: "",
    projectKey: "",
    baseDir: "",
    projectPropertiesPath: "",
  };
}

async function retrieveEffectiveConfiguration(
  projectFile: string
): Promise<Configuration> {
  let config = getDefaultConfiguration();

  const projectOptions = getProjectOptions(projectFile);
  if (projectOptions) {
    config.baseDir = projectOptions.baseDir();
    config.projectPropertiesPath = projectOptions.projectPropertiesPath();
    if (projectOptions.connectedMode()) {
      config.sonarHostUrl = projectOptions.sonarHostUrl();
      config.projectKey = projectOptions.projectKey();
      console.log(settings.getSonarTokens());
      config.apiToken =
        settings.getSonarTokens()?.[projectOptions.sonarHostUrl()]?.[
          projectOptions.projectKey()
        ] ?? "";
    }
  } else {
    config.baseDir = path.dirname(projectFile);
  }

  return config;
}

async function selectProjectFile(
  statusItem: LintStatusItem
): Promise<string | null> {
  statusItem.setAction("Selecting project...");
  const projectChoice = await getOrPromptActiveProject();
  statusItem.setActiveProject(projectChoice);
  return projectChoice || null;
}

async function analyzeFiles(
  serverSupplier: ServerSupplier,
  issueCollection: vscode.DiagnosticCollection,
  files: string[],
  statusItem: LintStatusItem
) {
  inAnalysis = true;
  try {
    const projectFile = await selectProjectFile(statusItem);

    let config;
    if (projectFile) {
      config = await retrieveEffectiveConfiguration(projectFile);
    } else {
      config = getDefaultConfiguration();
      config.baseDir = getDefaultBaseDir(files);
    }

    statusItem.setAction("Checking files...");
    const inputFiles = constructInputFiles(
      files,
      config.baseDir,
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
      apiToken: config.apiToken,
      compilerVersion: settings.getCompilerVersion(),
      sonarHostUrl: config.sonarHostUrl,
      sonarDelphiVersion: settings.getSonarDelphiVersion(),
    });

    statusItem.setAction("Analyzing...");
    await doAnalyze(server, issueCollection, statusItem.setAction, {
      baseDir: config.baseDir,
      inputFiles: projectFile ? [...inputFiles, projectFile] : inputFiles,
      projectKey: config.projectKey,
      projectPropertiesPath: config.projectPropertiesPath,
      sonarHostUrl: config.sonarHostUrl,
      apiToken: config.apiToken,
      disabledRules:
        config.sonarHostUrl === "" && !settings.getUseDefaultRules()
          ? settings.getDisabledRules()
          : undefined,
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

export async function clearThisFile(
  issueCollection: vscode.DiagnosticCollection
) {
  const activeTextEditor = vscode.window.activeTextEditor;
  if (activeTextEditor) {
    issueCollection.set(activeTextEditor.document.uri, undefined);
  }
}
