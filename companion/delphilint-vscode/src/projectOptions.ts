import * as path from "path";
import * as ini from "ini";
import * as fs from "fs";

type ProjectOptionsIni = {
  SonarHost: {
    ProjectKey: string;
    Url: string;
    Token: string;
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

  apiToken(): string {
    return this.optionsIni.SonarHost.Token;
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
