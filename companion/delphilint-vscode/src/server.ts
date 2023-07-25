import { Socket } from 'net';
import { TextDecoder, TextEncoder } from 'util';

export enum LintMessageType {
  initialize = 20,
  initialized = 25,
  initializeError = 26,
  analyze = 30,
  analyzeResult = 35,
  analyzeError = 36
};

export type RequestInitialize = {
  bdsPath: string,
  compilerVersion: string,
  defaultSonarDelphiJarPath: string,
  sonarHostUrl: string,
  apiToken: string
};

export type RequestAnalyze = {
  baseDir: string,
  inputFiles: Iterable<string>,
  sonarHostUrl: string,
  projectKey: string,
  apiToken: string,
  projectPropertiesPath: string
};

export type LintIssue = {
  ruleKey: string,
  message: string,
  file: string,
  range?: {
    startLine: number,
    startOffset: number,
    endLine: number,
    endOffset: number
  }
};

export type LintMessage = {
  category: LintMessageType,
  data?: any
};

export type LintResponseAction = (message: LintMessage) => void;

export class LintServer {
  private tcpClient: Socket;
  private textDecoder: TextDecoder;
  private textEncoder: TextEncoder;
  private nextId: number;
  private responseActions: Map<number, LintResponseAction>;

  constructor(port: number) {
    this.textDecoder = new TextDecoder();
    this.textEncoder = new TextEncoder();
    this.nextId = 0;
    this.responseActions = new Map<number, LintResponseAction>();

    this.tcpClient = new Socket();
    this.tcpClient.on("data", (buffer) => {
      this.onReceiveMessage(buffer);
    });
    this.tcpClient.connect(port);
  }

  private onReceiveMessage(buffer: Buffer) {
    let category = buffer.readUint8(0);
    let id = buffer.readInt32BE(1);

    let dataStr = buffer.toString('utf8', 1 + 4 + 4);
    let dataObj = JSON.parse(dataStr);

    let onResponse = this.responseActions.get(id);
    if (onResponse) {
      onResponse({
        category: category,
        data: dataObj
      });
      this.responseActions.delete(id);
    }
  }

  private sendMessageWithId(category: LintMessageType, id: number, data: Object) {
    let dataStr = JSON.stringify(data);
    let dataBytes = this.textEncoder.encode(dataStr);

    let messageBytes = Buffer.alloc(1 + 4 + 4 + dataBytes.length);
    messageBytes.writeUint8(category.valueOf(), 0);
    messageBytes.writeInt32BE(id, 1);
    messageBytes.writeInt32BE(dataBytes.length, 1 + 4);
    messageBytes.write(dataStr, 1 + 4 + 4, 'utf8');

    this.tcpClient.write(messageBytes);
  }

  private sendMessage(category: LintMessageType, data: any, onResponse?: LintResponseAction) {
    let id = this.nextId;
    this.nextId += 1;
    if(onResponse) {
      this.responseActions.set(id, onResponse);
    }
    this.sendMessageWithId(category, id, data);
  }

  initialize(msg: RequestInitialize, onInitialized: () => void, onError: (err: string) => void) {
    this.sendMessage(LintMessageType.initialize, msg, (msg) => {
      if(msg.category === LintMessageType.initialized) {
        onInitialized();
      } else if (msg.category === LintMessageType.initializeError) {
        onError(msg.data);
      } else {
        onError("Unknown category " + msg.category.valueOf());
      }
    });
  }

  analyze(msg: RequestAnalyze, onResult: (issues: LintIssue[]) => void, onError: (err: string) => void) {
    this.sendMessage(LintMessageType.analyze, msg, (msg) => {
      if(msg.category === LintMessageType.analyzeResult) {
        onResult(msg.data.issues);
      } else if (msg.category === LintMessageType.analyzeError) {
        onError(msg.data);
      } else {
        onError("Unknown category " + msg.category.valueOf());
      }
    });
  }
}