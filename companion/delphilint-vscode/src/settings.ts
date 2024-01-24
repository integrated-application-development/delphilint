import * as path from "path";
import * as ini from "ini";
import * as fs from "fs";
import * as vscode from "vscode";
import { NoServerJarError } from "./error";

export const SETTINGS_DIR = path.join(
  process.env.APPDATA as string,
  "DelphiLint"
);
export const SETTINGS_FILE = path.join(SETTINGS_DIR, "delphilint.ini");

let version: string = "";

export function registerVersion(ver: string) {
  version = ver;
}

type LintSettingsIni = {
  Resources?: {
    JavaExeOverride?: string;
    ServerJarOverride?: string;
    SonarDelphiJarOverride?: string;
  };
};

function getSettings(path: string): LintSettingsIni {
  const settingsStr = fs.readFileSync(path, "utf8");
  const settings = ini.parse(settingsStr);
  return settings as LintSettingsIni;
}

function getVscodeConfig(section: string): string {
  return vscode.workspace.getConfiguration().get(section) as string;
}

export function getSonarDelphiJar(): string {
  const settings = getSettings(SETTINGS_FILE);
  return (
    settings?.Resources?.SonarDelphiJarOverride ||
    path.join(SETTINGS_DIR, "sonar-delphi-plugin.jar")
  );
}

export function getServerJar(): string {
  const settings = getSettings(SETTINGS_FILE);

  if (
    settings?.Resources?.ServerJarOverride &&
    settings.Resources.ServerJarOverride.length > 0
  ) {
    return settings.Resources.ServerJarOverride;
  }

  const serverVersionConfig = getVscodeConfig("delphilint.serverVersion");

  if (serverVersionConfig.length > 0) {
    return path.join(
      SETTINGS_DIR,
      `delphilint-server-${serverVersionConfig}.jar`
    );
  } else if (version.endsWith("+dev")) {
    const serverNameStart = "delphilint-server-" + version.toLowerCase();

    const validServers = fs
      .readdirSync(SETTINGS_DIR)
      .map((file) => file.toLowerCase())
      .filter(
        (file) => file.startsWith(serverNameStart) && file.endsWith(".jar")
      )
      .map((file) => path.join(SETTINGS_DIR, file));

    if (validServers.length === 1) {
      return validServers[0];
    } else if (validServers.length > 1) {
      throw new NoServerJarError(
        `Multiple (${validServers.length}) DelphiLint ${version} candidates were found. Please set the exact version to use in DelphiLint's VSCode settings.`
      );
    } else {
      throw new NoServerJarError(
        `No candidate server found. Please ensure that DelphiLint Server ${version} is installed.`
      );
    }
  } else {
    const serverPath = path.join(
      SETTINGS_DIR,
      `delphilint-server-${version}.jar`
    );
    if (fs.existsSync(serverPath)) {
      return serverPath;
    } else {
      throw new NoServerJarError(
        `No candidate server found. Please ensure that DelphiLint Server ${version} is installed.`
      );
    }
  }
}

export function getJavaExe(): string {
  const override = getSettings(SETTINGS_FILE).Resources?.JavaExeOverride;
  if (override) {
    return override;
  }

  const javaHome = process.env.JAVA_HOME;
  if (!javaHome) {
    return "";
  } else {
    return path.join(javaHome, "bin", "java.exe");
  }
}

export function getBdsPath(): string {
  return getVscodeConfig("delphilint.bdsPath");
}

export function getCompilerVersion(): string {
  return getVscodeConfig("delphilint.compilerVersion");
}
