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
  };
  SonarHost?: {
    Tokens?: string;
  };
  Server?: {
    SonarDelphiVersionOverride?: string;
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

export function getSonarDelphiVersion(): string {
  const override =
    getSettings(SETTINGS_FILE).Server?.SonarDelphiVersionOverride;
  if (override) {
    return override;
  } else {
    return "1.4.0";
  }
}

type SonarTokensMap = { [key: string]: { [key: string]: string } };

export function getSonarTokens(): SonarTokensMap {
  const tokensStr = getSettings(SETTINGS_FILE).SonarHost?.Tokens;
  if (tokensStr === undefined) {
    return {};
  }

  const kvps = tokensStr.split(",");
  const res: SonarTokensMap = {};

  for (const kvpStr of kvps) {
    const kvp = kvpStr.split("=");
    if (kvp.length !== 2) {
      continue;
    }

    const key = kvp[0];
    const value = kvp[1];

    const splitKey = key.split("@");
    if (splitKey.length !== 2) {
      continue;
    }

    const projectKey = splitKey[0];
    const host = splitKey[1];

    res[host] = { [projectKey]: value };
  }

  return res;
}

export function getBdsPath(): string {
  return getVscodeConfig("delphilint.bdsPath");
}

export function getCompilerVersion(): string {
  return getVscodeConfig("delphilint.compilerVersion");
}
