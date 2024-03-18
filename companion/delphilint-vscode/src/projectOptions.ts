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

type ProjectOptionsIni = {
  SonarHost: {
    ProjectKey: string;
    Url: string;
    DownloadPlugin: "0" | "1";
  };
  Analysis: {
    BaseDir: string;
    ReadProperties: "0" | "1";
    ConnectedMode: "0" | "1";
  };
};

export class ProjectOptions {
  private readonly optionsIni: ProjectOptionsIni;
  private readonly iniPath: string;

  constructor(path: string) {
    this.iniPath = path;
    const optionsStr = fs.readFileSync(this.iniPath, "utf8");
    this.optionsIni = ini.parse(optionsStr) as ProjectOptionsIni;
  }

  projectKey(): string {
    return this.optionsIni.SonarHost.ProjectKey;
  }

  sonarHostUrl(): string {
    return this.optionsIni.SonarHost.Url;
  }

  downloadPlugin(): boolean {
    return this.optionsIni.SonarHost.DownloadPlugin !== "0";
  }

  baseDir(): string {
    return path.resolve(
      path.dirname(this.iniPath),
      this.optionsIni.Analysis.BaseDir
    );
  }

  projectPropertiesPath(): string {
    if (this.optionsIni.Analysis.ReadProperties !== "0") {
      return path.join(this.baseDir(), "sonar-project.properties");
    } else {
      return "";
    }
  }

  connectedMode(): boolean {
    return this.optionsIni.Analysis.ConnectedMode !== "0";
  }
}
