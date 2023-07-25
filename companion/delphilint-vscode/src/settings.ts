import * as path from 'path';
import * as ini from 'ini';
import * as fs from 'fs';
import * as vscode from 'vscode';

export const SETTINGS_DIR = path.join(process.env.APPDATA as string, "DelphiLint");
export const SETTINGS_FILE = path.join(SETTINGS_DIR, "delphilint.ini");

let version: string = "";

export function registerVersion(ver: string) {
  version = ver;
}

type LintSettingsIni = {
  ServerJarOverride: string,
  SonarDelphiJarOverride: string,
  JavaExe: string,
  ShowConsole: boolean,
  AutoLaunch: boolean
};

function getSettings(path: string): LintSettingsIni {
  let settingsStr = fs.readFileSync(path, 'utf8');
  let settings = ini.parse(settingsStr);
  return settings as LintSettingsIni;
}

export function getSonarDelphiJar(): string {
  let settings = getSettings(SETTINGS_FILE);
  return settings.SonarDelphiJarOverride || path.join(SETTINGS_DIR, "sonar-delphi-plugin.jar");
}

export function getServerJar(): string {
  let settings = getSettings(SETTINGS_FILE);
  return settings.ServerJarOverride || path.join(SETTINGS_DIR, "delphilint-server-" + version + ".jar");
}

export function getJavaExe(): string {
  return getSettings(SETTINGS_FILE).JavaExe;
}

export function getShowConsole(): boolean {
  return getSettings(SETTINGS_FILE).ShowConsole;
}

export function getAutoLaunch(): boolean {
  return getSettings(SETTINGS_FILE).AutoLaunch;
}

export function getBdsPath(): string {
  return vscode.workspace.getConfiguration().get("delphilint-vscode.bdsPath") as string;
}

export function getCompilerVersion(): string {
  return vscode.workspace.getConfiguration().get("delphilint-vscode.compilerVersion") as string;
}