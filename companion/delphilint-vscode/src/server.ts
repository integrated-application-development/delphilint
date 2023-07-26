import { Socket } from "net";
import { TextDecoder, TextEncoder } from "util";
import { spawn, ChildProcess } from "child_process";
import * as tmp from "tmp";
import * as fs from "fs";
import * as settings from "./settings";

export enum LintMessageType {
  quit = 15,
  initialize = 20,
  initialized = 25,
  initializeError = 26,
  analyze = 30,
  analyzeResult = 35,
  analyzeError = 36,
}

export type RequestInitialize = {
  bdsPath: string;
  compilerVersion: string;
  defaultSonarDelphiJarPath: string;
  sonarHostUrl: string;
  apiToken: string;
};

export type RequestAnalyze = {
  baseDir: string;
  inputFiles: Iterable<string>;
  sonarHostUrl: string;
  projectKey: string;
  apiToken: string;
  projectPropertiesPath: string;
};

export type LintIssue = {
  ruleKey: string;
  message: string;
  file: string;
  range?: {
    startLine: number;
    startOffset: number;
    endLine: number;
    endOffset: number;
  };
};

export type LintMessage = {
  category: LintMessageType;
  data?: any;
};

export type LintResponseAction = (message: LintMessage) => void;

class ExternalServer {
  private process: ChildProcess;
  private isExited: boolean;

  constructor(command: string, args: string[], workingDir: string, showConsole: boolean) {
    this.isExited = false;

    this.process = spawn(
      command,
      args,
      { cwd: workingDir, windowsHide: !showConsole, detached: showConsole }
    );
    this.process.on("exit", () => (this.isExited = true));
  }

  exited() {
    return this.isExited;
  }

  kill() {
    this.process.kill();
  }
}

export class LintServer {
  private tcpClient: Socket;
  private textDecoder: TextDecoder;
  private textEncoder: TextEncoder;
  private nextId: number;
  private responseActions: Map<number, LintResponseAction>;
  private externalServer?: ExternalServer;
  private serverStarted: boolean;

  constructor() {
    this.textDecoder = new TextDecoder();
    this.textEncoder = new TextEncoder();
    this.nextId = 0;
    this.serverStarted = false;
    this.responseActions = new Map<number, LintResponseAction>();

    this.tcpClient = new Socket();
    this.tcpClient.on("data", (buffer) => {
      this.onReceiveMessage(buffer);
    });

    this.startExternalServer(
      settings.getServerJar(),
      settings.getJavaExe(),
      settings.SETTINGS_DIR,
      settings.getShowConsole()
    )
      .then((port) => {
        this.serverStarted = true;
        this.tcpClient.connect(port);
      })
      .catch((err) => console.error(err));
  }

  private startExternalServer(
    jar: string,
    javaExe: string,
    workingDir: string,
    showConsole: boolean
  ): Promise<number> {
    return new Promise((resolve, reject) => {
      try {
        let portFile = tmp.fileSync().name;

        fs.watchFile(portFile, { interval: 50 }, (before, after) => {
          fs.unwatchFile(portFile);
          let portStr = fs.readFileSync(portFile, "utf8");
          resolve(parseInt(portStr));
          fs.rmSync(portFile);
        });

        this.externalServer = new ExternalServer(
          javaExe,
          ["-jar", jar, portFile],
          workingDir,
          showConsole
        );
      } catch (err) {
        reject(err);
      }
    });
  }

  ready(): boolean {
    return this.serverStarted;
  }

  private onReceiveMessage(buffer: Buffer) {
    let category = buffer.readUint8(0);
    let id = buffer.readInt32BE(1);

    let dataStr = buffer.toString("utf8", 1 + 4 + 4);
    let dataObj = JSON.parse(dataStr);

    let onResponse = this.responseActions.get(id);
    if (onResponse) {
      onResponse({
        category: category,
        data: dataObj,
      });
      this.responseActions.delete(id);
    }
  }

  private sendMessageWithId(
    category: LintMessageType,
    id: number,
    data: Object
  ) {
    let dataStr = JSON.stringify(data);
    let dataBytes = this.textEncoder.encode(dataStr);

    let messageBytes = Buffer.alloc(1 + 4 + 4 + dataBytes.length);
    messageBytes.writeUint8(category.valueOf(), 0);
    messageBytes.writeInt32BE(id, 1);
    messageBytes.writeInt32BE(dataBytes.length, 1 + 4);
    messageBytes.write(dataStr, 1 + 4 + 4, "utf8");

    this.tcpClient.write(messageBytes);
  }

  private sendMessage(
    category: LintMessageType,
    data: any,
    onResponse?: LintResponseAction
  ) {
    let id = this.nextId;
    this.nextId += 1;
    if (onResponse) {
      this.responseActions.set(id, onResponse);
    }
    this.sendMessageWithId(category, id, data);
  }

  initialize(msg: RequestInitialize): Promise<void> {
    return new Promise((resolve, reject) => {
      this.sendMessage(LintMessageType.initialize, msg, (msg) => {
        if (msg.category === LintMessageType.initialized) {
          resolve();
        } else if (msg.category === LintMessageType.initializeError) {
          reject(msg.data);
        } else {
          reject("Unknown category " + msg.category.valueOf());
        }
      });
    });
  }

  analyze(msg: RequestAnalyze): Promise<LintIssue[]> {
    return new Promise((resolve, reject) => {
      this.sendMessage(LintMessageType.analyze, msg, (msg) => {
        if (msg.category === LintMessageType.analyzeResult) {
          resolve(msg.data.issues);
        } else if (msg.category === LintMessageType.analyzeError) {
          reject(msg.data);
        } else {
          reject("Unknown category " + msg.category.valueOf());
        }
      });
    });
  }

  quit() {
    if(!this.externalServer) {
      return;
    }

    this.sendMessage(LintMessageType.quit, null);
    setTimeout(() => {
      if(this.externalServer && !this.externalServer.exited) {
        this.externalServer.kill();
      }
    }, 2000);
  }
}
