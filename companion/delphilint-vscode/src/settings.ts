import * as path from 'path';

export const SETTINGS_PATH = path.join(process.env.APPDATA as string, "DelphiLint");

export type LintSettings = {
  serverJar: string,
  sonarDelphiJar: string,
  javaExe: string,
  showConsole: boolean,
  autoLaunch: boolean
};

export function getSonarDelphiJar(): string {
  return path.join(SETTINGS_PATH, "sonar-delphi-plugin.jar");
}