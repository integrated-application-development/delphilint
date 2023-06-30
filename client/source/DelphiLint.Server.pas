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
  C_RuleRetrieve = 40;
  C_RuleRetrieveResult = 45;
  C_RuleRetrieveError = 46;
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
      ProjectKey: string = '';
      ApiToken: string = '';
      ProjectPropertiesPath: string = ''
    ): TLintMessage; static;
    class function RuleRetrieve(
      SonarHostUrl: string = '';
      ProjectKey: string = '';
      ApiToken: string = ''
    ): TLintMessage; static;
    class function Quit: TLintMessage; static;
    property Category: Byte read FCategory;
    property Data: TJSONValue read FData;
  end;

//______________________________________________________________________________________________________________________

  TResponseAction = reference to procedure (const Message: TLintMessage);
  TAnalyzeResultAction = reference to procedure(Issues: TObjectList<TLintIssue>);
  TRuleRetrieveResultAction = reference to procedure(Rules: TObjectDictionary<string, TRule>);
  TErrorAction = reference to procedure(Message: string);

  ELintServerError = class(Exception)
  end;

  ELintServerFailed = class(ELintServerError)
  end;

  ELintServerMisconfigured = class(ELintServerError)
  end;

  ELintServerTimedOut = class(ELintServerError)
  end;

  ELintPortRefused = class(ELintServerError)
  end;

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

    procedure StartExtServer(
      Jar: string;
      JavaExe: string;
      Port: Integer;
      WorkingDir: string;
      ShowConsole: Boolean);
    procedure StopExtServer;

    procedure OnInitializeResponse(
      const Response: TLintMessage;
      InitializeEvent: TEvent
    );
    procedure OnAnalyzeResponse(
      Response: TLintMessage;
      OnResult: TAnalyzeResultAction;
      OnError: TErrorAction
    );

    procedure OnRuleRetrieveResponse(
      Response: TLintMessage;
      OnResult: TRuleRetrieveResultAction;
      OnError: TErrorAction
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
      OnError: TErrorAction;
      SonarHostUrl: string = '';
      ProjectKey: string = '';
      ApiToken: string = '';
      ProjectPropertiesPath: string = '');
    procedure RetrieveRules(
      SonarHostUrl: string;
      ProjectKey: string;
      OnResult: TRuleRetrieveResultAction;
      OnError: TErrorAction;
      ApiToken: string = '');
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    IdGlobal
  , DelphiLint.Logger
  , ToolsAPI
  , Winapi.Windows
  , DelphiLint.Settings
  , IdStack
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
  ProjectKey: string = '';
  ApiToken: string = '';
  ProjectPropertiesPath: string = ''
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
  Json.AddPair('apiToken', ApiToken);
  Json.AddPair('projectPropertiesPath', ProjectPropertiesPath);

  Result := TLintMessage.Create(C_Analyze, Json);
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.RuleRetrieve(
  SonarHostUrl: string = '';
  ProjectKey: string = '';
  ApiToken: string = ''
): TLintMessage;
var
  Json: TJSONObject;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestRuleRetrieve
  Json := TJSONObject.Create;
  Json.AddPair('sonarHostUrl', SonarHostUrl);
  Json.AddPair('projectKey', ProjectKey);
  Json.AddPair('apiToken', ApiToken);

  Result := TLintMessage.Create(C_RuleRetrieve, Json);
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
    StartExtServer(
      LintSettings.ServerJar,
      LintSettings.ServerJavaExe,
      Port,
      LintSettings.SettingsDirectory,
      LintSettings.ServerShowConsole);
    Sleep(LintSettings.ServerStartDelay);
  end;

  FTcpClient := TIdTCPClient.Create;
  FTcpClient.Host := '127.0.0.1';
  FTcpClient.Port := Port;

  try
    FTcpClient.Connect;
  except
    on EIdSocketError do begin
      raise ELintPortRefused.CreateFmt('Connection refused to port %d', [Port]);
    end;
  end;

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
    try
      ReceiveMessage;
    except
      on E: Exception do begin
        FTcpClient.CheckForGracefulDisconnect(False);
        if not FTcpClient.Connected then begin
          Log.Info('TCP connection to server was unexpectedly terminated.');
          Break;
        end
        else begin
          Log.Info('Error occurred in server thread: ' + E.Message);
        end;
      end;
    end;
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
      raise ELintServerTimedOut.Create('Initialize timed out');
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
    Log.Info('Initialize error (%d): %s', [Response.Category, Response.Data.ToString]);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Analyze(
  BaseDir: string;
  DelphiFiles: TArray<string>;
  OnResult: TAnalyzeResultAction;
  OnError: TErrorAction;
  SonarHostUrl: string = '';
  ProjectKey: string = '';
  ApiToken: string = '';
  ProjectPropertiesPath: string = '');
begin
  Log.Info('Requesting analysis.');
  Initialize;

  SendMessage(
    TLintMessage.Analyze(BaseDir, DelphiFiles, SonarHostUrl, ProjectKey, ApiToken, ProjectPropertiesPath),
    procedure (const Response: TLintMessage) begin
      OnAnalyzeResponse(Response, OnResult, OnError);
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnAnalyzeResponse(
  Response: TLintMessage;
  OnResult: TAnalyzeResultAction;
  OnError: TErrorAction
);

  function ParseIssues(Json: TJSONArray): TObjectList<TLintIssue>;
  var
    Index: Integer;
  begin
    Result := TObjectList<TLintIssue>.Create;
    for Index := 0 to Json.Count - 1 do begin
      Result.Add(TLintIssue.FromJson(Json[Index] as TJSONObject));
    end;
  end;

var
  Issues: TObjectList<TLintIssue>;
  ErrorMsg: string;
  ErrorCat: Byte;
begin
  Log.Info('Analysis response received (%d)', [Response.Category]);

  if Response.Category <> C_AnalyzeResult then begin
    ErrorMsg := Response.Data.AsType<string>;
    ErrorCat := Response.Category;

    Log.Info('Analyze error (%d): %s', [ErrorCat, ErrorMsg]);
    OnError(ErrorMsg);
  end
  else begin
    Issues := ParseIssues(Response.Data.GetValue<TJSONArray>('issues'));

    Log.Info('Calling post-analyze action.');
    OnResult(Issues);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.RetrieveRules(
  SonarHostUrl: string;
  ProjectKey: string;
  OnResult: TRuleRetrieveResultAction;
  OnError: TErrorAction;
  ApiToken: string = ''
);
begin
  SendMessage(
    TLintMessage.RuleRetrieve(SonarHostUrl, ProjectKey, ApiToken),
    procedure (const Response: TLintMessage) begin
      Log.Info('Rule retrieval response retrieved');
      OnRuleRetrieveResponse(Response, OnResult, OnError);
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnRuleRetrieveResponse(
  Response: TLintMessage;
  OnResult: TRuleRetrieveResultAction;
  OnError: TErrorAction
);

  function ParseRules(Json: TJSONObject): TObjectDictionary<string, TRule>;
  var
    Pair: TJSONPair;
  begin
    Result := TObjectDictionary<string, TRule>.Create;
    for Pair in Json do begin
      Result.Add(Pair.JsonString.Value, TRule.FromJson(TJSONObject(Pair.JsonValue)));
    end;
  end;

var
  ErrorMsg: string;
  ErrorCat: Byte;
  Rules: TObjectDictionary<string, TRule>;
begin
  Log.Info('Rule retrieve response received (%d)', [Response.Category]);

  if Response.Category <> C_RuleRetrieveResult then begin
    ErrorMsg := Response.Data.AsType<string>;
    ErrorCat := Response.Category;

    Log.Info('Rule retrieve error (%d): %s', [ErrorCat, ErrorMsg]);
    OnError(ErrorMsg);
  end
  else begin
    Rules := ParseRules(Response.Data.GetValue<TJSONObject>('rules'));

    Log.Info('Calling post-rule retrieve action.');
    OnResult(Rules);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnUnhandledMessage(Message: TLintMessage);
begin
  Log.Info('Unhandled message (code %d) received: <%s>', [Message.Category, Message.Data]);
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

procedure TLintServer.StartExtServer(
  Jar: string;
  JavaExe: string;
  Port: Integer;
  WorkingDir: string;
  ShowConsole: Boolean);
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
    raise ELintServerMisconfigured.CreateFmt('Server jar not found at path "%s"', [Jar]);
  end;

  if not FileExists(JavaExe) then begin
    raise ELintServerMisconfigured.CreateFmt('Java executable not found at path "%s"', [JavaExe]);
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
    PChar(WorkingDir),
    StartupInfo,
    ProcessInfo
  ) then begin
    ErrorCode := GetLastError;
    raise ELintServerFailed.CreateFmt(
      'DelphiLint server could not be started (error code %d: %s)',
      [ErrorCode, SysErrorMessage(ErrorCode)]);
  end;

  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.StopExtServer;
begin
  if Assigned(FTcpClient) then begin
    FTcpClient.CheckForGracefulDisconnect(False);
    if FTcpClient.Connected then begin
      SendMessage(TLintMessage.Quit);
    end;
  end;
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
      try
        FResponseActions[Id](Message);
      except
        on E: Exception do begin
          Log.Info(
            'Registered handler for incoming message (type %d) failed with exception %s %s',
            [Category, E.ClassName, E.Message])
        end;
      end;
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
