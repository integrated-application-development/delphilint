import { Socket } from "net";
import { TextEncoder } from "util";
import { spawn, ChildProcess } from "child_process";
import * as tmp from "tmp";
import * as fs from "fs";
import {
  AnalyzeError,
  InitializeError,
  NoJavaExeError,
  ServerError,
} from "./error";

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

class ExtServer {
  private _process?: ChildProcess;
  private _socket: Socket;
  private _ready: boolean;
  private _exited: boolean;
  private _readyListeners: (() => void)[];
  private _exitListeners: (() => void)[];

  constructor(
    jar: string,
    javaExe: string,
    workingDir: string,
    onLogMessage: (msg: string) => void,
    onReceiveMessage: (buffer: Buffer) => void
  ) {
    this._ready = false;
    this._exited = false;
    this._readyListeners = [];
    this._exitListeners = [];
    this._socket = new Socket();
    this._socket.on("data", onReceiveMessage);
    this.setup(jar, javaExe, workingDir, onLogMessage);

    this.onProcessExit.bind(this);
  }

  addExitListener(listener: () => void) {
    this._exitListeners.push(listener);
  }

  private onProcessExit() {
    this._exited = true;
    this._exitListeners.forEach((listener) => listener());
  }

  async setup(
    jar: string,
    javaExe: string,
    workingDir: string,
    onLogMessage: (msg: string) => void
  ) {
    let port = await new Promise<number>((resolve, reject) => {
      let portFile = tmp.fileSync().name;

      fs.watchFile(portFile, { interval: 50 }, (before, after) => {
        fs.unwatchFile(portFile);
        let portStr = fs.readFileSync(portFile, "utf8");
        resolve(parseInt(portStr));
        fs.rmSync(portFile);
      });

      this._process = spawn(javaExe, ["-jar", jar, portFile], {
        cwd: workingDir,
      });

      this._process.on("error", (err) => reject(err));
      this._process.on("exit", this.onProcessExit);
      this._process.stdout?.on("data", (data) => {
        onLogMessage(data.toString());
      });
    });

    this._socket.connect(port);
    this._ready = true;
    this._readyListeners.forEach((listener) => listener());
  }

  ready(): boolean {
    return this._ready;
  }

  exited(): boolean {
    return this._exited;
  }

  readyWait(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      if (this._ready) {
        resolve();
      } else {
        this._readyListeners.push(resolve);
      }
    });
  }

  destroy() {
    this._process?.kill();
  }

  process(): ChildProcess | undefined {
    return this._process;
  }

  socket(): Socket {
    return this._socket;
  }
}

export class LintServer {
  private textEncoder: TextEncoder;
  private nextId: number;
  private responseActions: Map<number, LintResponseAction>;
  private rejections: Map<number, (err: any) => void>;
  private jar: string;
  private javaExe: string;
  private workingDir: string;
  private processLog: (msg: string) => void;
  private _extServer?: ExtServer;
  private _partialMessage?: Buffer;

  constructor(
    jar: string,
    javaExe: string,
    workingDir: string,
    processLog: (msg: string) => void
  ) {
    this.onReceiveMessage = this.onReceiveMessage.bind(this);

    this.jar = jar;
    this.javaExe = javaExe;
    this.workingDir = workingDir;
    this.processLog = processLog;

    this.textEncoder = new TextEncoder();
    this.nextId = 0;
    this.responseActions = new Map<number, LintResponseAction>();
    this.rejections = new Map<number, (err: any) => void>();
  }

  private async externalServer(): Promise<ExtServer> {
    if (this._extServer) {
      return this._extServer;
    } else {
      return await this.startExternalServer();
    }
  }

  async startExternalServer(): Promise<ExtServer> {
    if (this._extServer) {
      throw new ServerError("Server is already started.");
    }

    if (!fs.existsSync(this.javaExe)) {
      throw new NoJavaExeError(`Java exe does not exist at ${this.javaExe}`);
    }

    if (!fs.existsSync(this.jar)) {
      throw new NoJavaExeError(`Server jar does not exist at ${this.jar}.`);
    }

    this._extServer = new ExtServer(
      this.jar,
      this.javaExe,
      this.workingDir,
      this.processLog,
      this.onReceiveMessage
    );

    await this._extServer.readyWait();
    return this._extServer;
  }

  stopExternalServer(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      if (this._extServer) {
        this._extServer.addExitListener(() => {
          this._extServer = undefined;
          resolve();
        });
        this.sendMessage(LintMessageType.quit, null, (err) => {});

        setTimeout(() => {
          let process = this._extServer ? this._extServer.process() : undefined;
          if (process) {
            process.kill();
          }
        }, 2000);
      }
    });
  }

  private onReceiveMessage(buffer: Buffer) {
    if (this._partialMessage) {
      buffer = Buffer.concat([this._partialMessage.valueOf(), buffer]);
    }

    let category = buffer.readUint8(0);
    let id = buffer.readInt32BE(1);

    try {
      let length = buffer.readInt32BE(1 + 4);
      if (buffer.byteLength !== 1 + 4 + 4 + length) {
        this._partialMessage = buffer;
        return;
      }
      this._partialMessage = undefined;

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
    } catch (err) {
      let reject = this.rejections.get(id);
      if (reject) {
        reject(err);
      } else {
        throw err;
      }
    }
  }

  private async sendMessageWithId(
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

    let extServer = await this.externalServer();
    extServer.socket().write(messageBytes);
  }

  private async sendMessage(
    category: LintMessageType,
    data: any,
    rejection: (err: any) => void,
    onResponse?: LintResponseAction
  ) {
    let id = this.nextId;
    this.nextId += 1;
    this.rejections.set(id, rejection);
    if (onResponse) {
      this.responseActions.set(id, onResponse);
    }
    await this.sendMessageWithId(category, id, data);
  }

  async initialize(msg: RequestInitialize): Promise<void> {
    return await new Promise<void>((resolve, reject) => {
      this.sendMessage(LintMessageType.initialize, msg, reject, (msg) => {
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
      this.sendMessage(LintMessageType.analyze, msg, reject, (msg) => {
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
    this.stopExternalServer();
  }
}
