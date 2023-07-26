import * as path from "path";
import * as ini from "ini";
import * as fs from "fs";
import * as vscode from "vscode";

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
  Server: {
    ShowConsole: number;
    AutoLaunch: number;
  };
  Resources: {
    JavaExe: string;
    ServerJarOverride: string;
    SonarDelphiJarOverride: string;
  };
};

function getSettings(path: string): LintSettingsIni {
  let settingsStr = fs.readFileSync(path, "utf8");
  let settings = ini.parse(settingsStr);
  return settings as LintSettingsIni;
}

function getVscodeConfig(section: string): string {
  return vscode.workspace.getConfiguration().get(section) as string;
}

function getServerVersion(): string {
  return getVscodeConfig("delphilint.serverVersion") || version;
}

export function getSonarDelphiJar(): string {
  let settings = getSettings(SETTINGS_FILE);
  return (
    settings.Resources.SonarDelphiJarOverride ||
    path.join(SETTINGS_DIR, "sonar-delphi-plugin.jar")
  );
}

export function getServerJar(): string {
  let settings = getSettings(SETTINGS_FILE);
  return (
    settings.Resources.ServerJarOverride ||
    path.join(SETTINGS_DIR, "delphilint-server-" + getServerVersion() + ".jar")
  );
}

export function getJavaExe(): string {
  return getSettings(SETTINGS_FILE).Resources.JavaExe;
}

export function getShowConsole(): boolean {
  return getSettings(SETTINGS_FILE).Server.ShowConsole !== 0;
}

export function getAutoLaunch(): boolean {
  return getSettings(SETTINGS_FILE).Server.AutoLaunch !== 0;
}

export function getBdsPath(): string {
  return getVscodeConfig("delphilint.bdsPath");
}

export function getCompilerVersion(): string {
  return getVscodeConfig("delphilint.compilerVersion");
}
