import { Socket } from "net";
import { TextDecoder, TextEncoder } from "util";
import { spawn, ChildProcess } from "child_process";
import * as tmp from "tmp";
import * as fs from "fs";
import * as settings from "./settings";
import { AnalyzeError, InitializeError, ServerError } from "./error";

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
  inputFiles: string[];
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

export class LintServer {
  private tcpClient: Socket;
  private textDecoder: TextDecoder;
  private textEncoder: TextEncoder;
  private nextId: number;
  private responseActions: Map<number, LintResponseAction>;
  private process: ChildProcess | undefined;

  constructor() {
    this.textDecoder = new TextDecoder();
    this.textEncoder = new TextEncoder();
    this.nextId = 0;
    this.process = undefined;
    this.responseActions = new Map<number, LintResponseAction>();

    this.tcpClient = new Socket();
    this.tcpClient.on("data", (buffer) => {
      this.onReceiveMessage(buffer);
    });
  }

  async startExternalServer(
    jar: string,
    javaExe: string,
    workingDir: string,
    processLog?: (msg: string) => void
  ): Promise<void> {
    if (this.process) {
      throw new ServerError("Server is already started.");
    }

    let port = await new Promise<number>((resolve, reject) => {
      let portFile = tmp.fileSync().name;

      fs.watchFile(portFile, { interval: 50 }, (before, after) => {
        fs.unwatchFile(portFile);
        let portStr = fs.readFileSync(portFile, "utf8");
        resolve(parseInt(portStr));
        fs.rmSync(portFile);
      });

      this.process = spawn(javaExe, ["-jar", jar, portFile], {
        cwd: workingDir,
      });

      this.process.on("error", (err) => reject(err));
      this.process.on("exit", () => (this.process = undefined));

      if (processLog) {
        this.process.stdout?.on("data", (data) => {
          processLog(data.toString());
        });
      }
    });

    this.tcpClient.connect(port);
  }

  ready(): boolean {
    return this.process !== undefined;
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

  async initialize(msg: RequestInitialize): Promise<void> {
    return await new Promise<void>((resolve, reject) => {
      this.sendMessage(LintMessageType.initialize, msg, (msg) => {
        if (msg.category === LintMessageType.initialized) {
          resolve();
        } else if (msg.category === LintMessageType.initializeError) {
          reject(new InitializeError(msg.data));
        } else {
          reject(new ServerError("Unknown category " + msg.category.valueOf()));
        }
      });
    });
  }

  async analyze(msg: RequestAnalyze): Promise<LintIssue[]> {
    return await new Promise<LintIssue[]>((resolve, reject) => {
      this.sendMessage(LintMessageType.analyze, msg, (msg) => {
        if (msg.category === LintMessageType.analyzeResult) {
          resolve(msg.data.issues);
        } else if (msg.category === LintMessageType.analyzeError) {
          reject(new AnalyzeError(msg.data));
        } else {
          reject(new ServerError("Unknown category " + msg.category.valueOf()));
        }
      });
    });
  }

  quit() {
    if (this.process) {
      this.sendMessage(LintMessageType.quit, null);

      setTimeout(() => {
        if (this.process) {
          this.process.kill();
        }
      }, 2000);
    }
  }
}
