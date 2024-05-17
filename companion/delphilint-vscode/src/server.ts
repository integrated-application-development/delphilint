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
  sonarHostUrl: string;
  apiToken: string;
  sonarDelphiVersion: string;
};

export type RequestAnalyze = {
  baseDir: string;
  inputFiles: string[];
  sonarHostUrl: string;
  projectKey: string;
  apiToken: string;
  projectPropertiesPath: string;
  disabledRules: string[] | undefined;
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
  private readonly _socket: Socket;
  private _ready: boolean;
  private _exited: boolean;
  private readonly _readyListeners: (() => void)[];
  private readonly _exitListeners: (() => void)[];

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
    const port = await new Promise<number>((resolve, reject) => {
      const portFile = tmp.fileSync().name;

      fs.watchFile(portFile, { interval: 50 }, (_before, _after) => {
        fs.unwatchFile(portFile);
        const portStr = fs.readFileSync(portFile, "utf8");
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
    return new Promise<void>((resolve, _reject) => {
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
  private readonly textEncoder: TextEncoder;
  private nextId: number;
  private readonly responseActions: Map<number, LintResponseAction>;
  private readonly rejections: Map<number, (err: any) => void>;
  private readonly jar: string;
  private readonly javaExe: string;
  private readonly workingDir: string;
  private readonly processLog: (msg: string) => void;
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
      return this.startExternalServer();
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
    return new Promise<void>((resolve, _reject) => {
      if (this._extServer) {
        this._extServer.addExitListener(() => {
          this._extServer = undefined;
          resolve();
        });
        this.sendMessage(LintMessageType.quit, null, (_err) => {});

        setTimeout(() => {
          const process = this._extServer
            ? this._extServer.process()
            : undefined;
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

    const category = buffer.readUint8(0);
    const id = buffer.readInt32BE(1);

    try {
      const length = buffer.readInt32BE(1 + 4);
      if (buffer.byteLength !== 1 + 4 + 4 + length) {
        this._partialMessage = buffer;
        return;
      }
      this._partialMessage = undefined;

      const dataStr = buffer.toString("utf8", 1 + 4 + 4);
      const dataObj = JSON.parse(dataStr);

      const onResponse = this.responseActions.get(id);
      if (onResponse) {
        onResponse({
          category,
          data: dataObj,
        });
        this.responseActions.delete(id);
      }
    } catch (err) {
      const reject = this.rejections.get(id);
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
    const dataStr = JSON.stringify(data);
    const dataBytes = this.textEncoder.encode(dataStr);

    const messageBytes = Buffer.alloc(1 + 4 + 4 + dataBytes.length);
    messageBytes.writeUint8(category.valueOf(), 0);
    messageBytes.writeInt32BE(id, 1);
    messageBytes.writeInt32BE(dataBytes.length, 1 + 4);
    messageBytes.write(dataStr, 1 + 4 + 4, "utf8");

    const extServer = await this.externalServer();
    extServer.socket().write(messageBytes);
  }

  private async sendMessage(
    category: LintMessageType,
    data: any,
    rejection: (err: any) => void,
    onResponse?: LintResponseAction
  ) {
    const id = this.nextId;
    this.nextId += 1;
    this.rejections.set(id, rejection);
    if (onResponse) {
      this.responseActions.set(id, onResponse);
    }
    await this.sendMessageWithId(category, id, data);
  }

  async initialize(msg: RequestInitialize): Promise<void> {
    return new Promise<void>((resolve, reject) => {
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
    return new Promise<LintIssue[]>((resolve, reject) => {
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
