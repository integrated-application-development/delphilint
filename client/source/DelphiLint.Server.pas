{
DelphiLint Client for RAD Studio
Copyright (C) 2023 Integrated Application Development

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.Server;

interface

uses
    IdTCPClient
  , System.JSON
  , System.Classes
  , System.SysUtils
  , DelphiLint.Data
  , System.Generics.Collections
  , System.SyncObjs
  ;

const
  C_Ping = 1;
  C_Pong = 5;
  C_Quit = 15;
  C_Initialize = 20;
  C_Analyze = 30;
  C_AnalyzeResult = 35;
  C_AnalyzeError = 36;
  C_Initialized = 25;
  C_Uninitialized = 26;
  C_InvalidRequest = 241;
  C_UnexpectedError = 242;

  C_Timeout = 10000;

type

//______________________________________________________________________________________________________________________

  TLintMessage = class(TObject)
  private
    FCategory: Byte;
    FData: TJSONValue;
  public
    constructor Create(Category: Byte); overload;
    constructor Create(Category: Byte; Data: TJSONValue); overload;
    destructor Destroy; override;
    class function Initialize(
      BdsPath: string;
      CompilerVersion: string;
      SonarDelphiJarPath: string
    ): TLintMessage; static;
    class function Analyze(
      BaseDir: string;
      InputFiles: TArray<string>;
      SonarHostUrl: string = '';
      ProjectKey: string = ''
    ): TLintMessage; static;
    class function Quit: TLintMessage; static;
    property Category: Byte read FCategory;
    property Data: TJSONValue read FData;
  end;

//______________________________________________________________________________________________________________________

  TResponseAction = reference to procedure (const Message: TLintMessage);
  TAnalyzeResultAction = reference to procedure(Issues: TObjectList<TLintIssue>);
  TAnalyzeErrorAction = reference to procedure(Message: string);

  TLintServer = class(TThread)
  private
    FTcpClient: TIdTCPClient;
    FResponseActions: TDictionary<Integer, TResponseAction>;
    FNextId: Integer;
    FTcpLock: TMutex;

    procedure SendMessage(Req: TLintMessage; OnResponse: TResponseAction = nil); overload;
    procedure SendMessage(Req: TLintMessage; Id: Integer); overload;
    procedure OnUnhandledMessage(Message: TLintMessage);
    procedure ReceiveMessage;

    procedure StartExtServer(Jar: string; JavaExe: string; Port: Integer; ShowConsole: Boolean);
    procedure StopExtServer;

    procedure OnInitializeResponse(
      const Response: TLintMessage;
      InitializeEvent: TEvent
    );
    procedure OnAnalyzeResponse(
      Response: TLintMessage;
      OnResult: TAnalyzeResultAction;
      OnError: TAnalyzeErrorAction
    );

  public
    constructor Create(Port: Integer);
    destructor Destroy; override;

    procedure Execute; override;

    procedure Initialize;
    procedure Analyze(
      BaseDir: string;
      DelphiFiles: TArray<string>;
      OnResult: TAnalyzeResultAction;
      OnError: TAnalyzeErrorAction;
      SonarHostUrl: string = '';
      ProjectKey: string = '');
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    IdGlobal
  , DelphiLint.Logger
  , ToolsAPI
  , Winapi.Windows
  , DelphiLint.Settings
  ;

//______________________________________________________________________________________________________________________

constructor TLintMessage.Create(Category: Byte; Data: TJSONValue);
begin
  FCategory := Category;
  FData := Data;
end;

//______________________________________________________________________________________________________________________

constructor TLintMessage.Create(Category: Byte);
begin
  FCategory := Category;
  FData := nil;
end;

//______________________________________________________________________________________________________________________

destructor TLintMessage.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.Initialize(
  BdsPath: string;
  CompilerVersion: string;
  SonarDelphiJarPath: string
): TLintMessage;
var
  Json: TJSONObject;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestInitialize
  Json := TJSONObject.Create;
  Json.AddPair('bdsPath', BdsPath);
  Json.AddPair('compilerVersion', CompilerVersion);
  Json.AddPair('sonarDelphiJarPath', SonarDelphiJarPath);

  Result := TLintMessage.Create(C_Initialize, Json);
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.Analyze(
  BaseDir: string;
  InputFiles: TArray<string>;
  SonarHostUrl: string = '';
  ProjectKey: string = ''
): TLintMessage;
var
  InputFilesJson: TJSONArray;
  Json: TJSONObject;
  InputFile: string;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestAnalyze
  InputFilesJson := TJSONArray.Create;
  for InputFile in InputFiles do begin
    InputFilesJson.Add(InputFile);
  end;

  Json := TJSONObject.Create;
  Json.AddPair('baseDir', BaseDir);
  Json.AddPair('inputFiles', InputFilesJson);
  Json.AddPair('sonarHostUrl', SonarHostUrl);
  Json.AddPair('projectKey', ProjectKey);

  Result := TLintMessage.Create(C_Analyze, Json);
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.Quit: TLintMessage;
begin
  Result := TLintMessage.Create(C_Quit);
end;

//______________________________________________________________________________________________________________________

constructor TLintServer.Create(Port: Integer);
begin
  inherited Create(False);

  FNextId := 1;
  FTcpLock := TMutex.Create;

  FResponseActions := TDictionary<Integer, TResponseAction>.Create;

  if LintSettings.ServerAutoLaunch then begin
    StartExtServer(LintSettings.ServerJar, LintSettings.ServerJavaExe, Port, LintSettings.ServerShowConsole);
    Sleep(LintSettings.ServerStartDelay);
  end;

  FTcpClient := TIdTCPClient.Create;
  FTcpClient.Host := '127.0.0.1';
  FTcpClient.Port := Port;
  FTcpClient.Connect;

  Log.Info('Server connected.');
end;

//______________________________________________________________________________________________________________________

destructor TLintServer.Destroy;
begin
  StopExtServer;
  FreeAndNil(FTcpClient);
  FreeAndNil(FResponseActions);
  FreeAndNil(FTcpLock);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Execute;
begin
  inherited;
  while True do begin
    ReceiveMessage;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Initialize;
const
  // TODO: Expose compiler version as a setting
  C_CompilerVersion = 'VER350';
  C_LanguageKey = 'delph';
var
  InitializeMsg: TLintMessage;
  InitializeCompletedEvent: TEvent;
begin
  Log.Info('Requesting initialization.');

  InitializeMsg := TLintMessage.Initialize(
    (BorlandIDEServices as IOTAServices).GetRootDirectory,
    C_CompilerVersion,
    LintSettings.SonarDelphiJar);

  InitializeCompletedEvent := TEvent.Create;
  try
    SendMessage(
      InitializeMsg,
      procedure(const Response: TLintMessage) begin
        OnInitializeResponse(Response, InitializeCompletedEvent);
      end);

    if InitializeCompletedEvent.WaitFor(C_Timeout) <> wrSignaled then begin
      raise Exception.Create('Initialize timed out');
    end;

    Log.Info('Initialization complete.');
  finally
    FreeAndNil(InitializeCompletedEvent);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnInitializeResponse(const Response: TLintMessage; InitializeEvent: TEvent);
begin
  if Assigned(InitializeEvent) then begin
    InitializeEvent.SetEvent;
  end;

  if Response.Category <> C_Initialized then begin
    Log.Info(Format('Initialize error (%d): %s', [Response.Category, Response.Data.ToString]));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Analyze(
  BaseDir: string;
  DelphiFiles: TArray<string>;
  OnResult: TAnalyzeResultAction;
  OnError: TAnalyzeErrorAction;
  SonarHostUrl: string = '';
  ProjectKey: string = '');
begin
  Log.Info('Requesting analysis.');
  Initialize;

  SendMessage(
    TLintMessage.Analyze(BaseDir, DelphiFiles, SonarHostUrl, ProjectKey),
    procedure (const Response: TLintMessage) begin
      OnAnalyzeResponse(Response, OnResult, OnError);
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnAnalyzeResponse(
  Response: TLintMessage;
  OnResult: TAnalyzeResultAction;
  OnError: TAnalyzeErrorAction
);
var
  IssuesArrayJson: TJSONArray;
  Index: Integer;
  Issues: TObjectList<TLintIssue>;
  ErrorMsg: string;
  ErrorCat: Byte;
begin
  Log.Info(Format('Analysis response received (%d)', [Response.Category]));

  if Response.Category <> C_AnalyzeResult then begin
    ErrorMsg := Response.Data.AsType<string>;
    ErrorCat := Response.Category;
    Log.Info(Format('Analyze error (%d): %s', [ErrorCat, ErrorMsg]));
    Synchronize(
      procedure begin
        OnError(ErrorMsg);
      end);
  end
  else begin
    IssuesArrayJson := Response.Data.GetValue<TJSONArray>('issues');
    try
      Issues := TObjectList<TLintIssue>.Create;
      for Index := 0 to IssuesArrayJson.Count - 1 do begin
        Issues.Add(TLintIssue.FromJson(IssuesArrayJson[Index] as TJSONObject));
      end;
    finally
      FreeAndNil(IssuesArrayJson);
    end;

    Log.Info('Calling post-analyze action.');
    Synchronize(
      procedure begin
        OnResult(Issues);
      end);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnUnhandledMessage(Message: TLintMessage);
begin
  Log.Info(Format('Unhandled message (code %d) received: <%s>', [Message.Category, Message.Data]));
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.SendMessage(Req: TLintMessage; Id: Integer);
var
  DataBytes: TArray<Byte>;
  DataByte: Byte;
begin
  FTcpLock.Acquire;
  try
    FTcpClient.IOHandler.Write(Req.Category);
    FTcpClient.IOHandler.Write(Id);

    if Assigned(Req.Data) then begin
      DataBytes := TEncoding.UTF8.GetBytes(Req.Data.ToString);

      FTcpClient.IOHandler.Write(Length(DataBytes));
      for DataByte in DataBytes do begin
        FTcpClient.IOHandler.Write(DataByte);
      end;
    end
    else begin
      FTcpClient.IOHandler.Write(Integer(0));
    end;
  finally
    FTcpLock.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.StartExtServer(Jar: string; JavaExe: string; Port: Integer; ShowConsole: Boolean);
const
  C_Title = 'DelphiLint Server';
var
  CommandLine: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ErrorCode: Integer;
  CreationFlags: Cardinal;
begin
  if not FileExists(Jar) then begin
    raise Exception.CreateFmt('Server jar not found at path "%s".', [Jar]);
  end;

  if not FileExists(JavaExe) then begin
    raise Exception.CreateFmt('Java executable not found at path "%s".', [JavaExe]);
  end;

  CommandLine := Format(' -jar "%s" %d', [Jar, Port]);

  ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);

  CreationFlags := NORMAL_PRIORITY_CLASS;
  if ShowConsole then begin
    StartupInfo.lpTitle := C_Title;
    CreationFlags := CreationFlags or CREATE_NEW_CONSOLE;
  end
  else begin
    CreationFlags := CreationFlags or DETACHED_PROCESS;
  end;

  if not CreateProcess(
    PChar(JavaExe),
    PChar(CommandLine),
    nil,
    nil,
    False,
    CreationFlags,
    nil,
    nil,
    StartupInfo,
    ProcessInfo
  ) then begin
    ErrorCode := GetLastError;
    raise Exception.CreateFmt(
      'DelphiLint server could not be started (error code %d: %s)',
      [ErrorCode, SysErrorMessage(ErrorCode)]);
  end;

  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.StopExtServer;
begin
  SendMessage(TLintMessage.Quit);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.SendMessage(Req: TLintMessage; OnResponse: TResponseAction = nil);
var
  Id: Integer;
begin
  FTcpLock.Acquire;
  try
    Id := FNextId;
    Inc(FNextId);
    if Assigned(OnResponse) then begin
      FResponseActions.Add(Id, OnResponse);
    end;
  finally
    FTcpLock.Release;
  end;

  SendMessage(Req, Id);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.ReceiveMessage;
var
  Id: SmallInt;
  Category: Byte;
  Length: Integer;
  DataStr: string;
  DataJsonValue: TJSONValue;
  Message: TLintMessage;
begin
  Category := FTcpClient.IOHandler.ReadByte;
  FTcpLock.Acquire;
  try
    Id := FTcpClient.IOHandler.ReadInt32;
    Length := FTcpClient.IOHandler.ReadInt32;
    DataStr := FTcpClient.IOHandler.ReadString(Length, IndyTextEncoding_UTF8);
  finally
    FTcpLock.Release;
  end;

  DataJsonValue := TJSONValue.ParseJSONValue(DataStr);
  Message := TLintMessage.Create(Category, DataJsonValue);
  try
    if FResponseActions.ContainsKey(Id) then begin
      FResponseActions[Id](Message);
    end
    else begin
      OnUnhandledMessage(Message);
    end;
  finally
    FreeAndNil(Message);
  end;
end;

//______________________________________________________________________________________________________________________

end.
