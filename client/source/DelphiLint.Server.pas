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
  C_RuleRetrieve = 40;
  C_RuleRetrieveResult = 45;
  C_Initialized = 25;

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
    class function Initialize(Options: TInitializeOptions): TLintMessage; static;
    class function Analyze(Options: TAnalyzeOptions): TLintMessage; static;
    class function RuleRetrieve(Options: TSonarProjectOptions): TLintMessage; static;
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

  TTaggedMessage = class(TObject)
  private
    FMessage: TLintMessage;
    FId: Integer;
  public
    constructor Create(Message: TLintMessage; Id: Integer);
    destructor Destroy; override;

    function Extract: TLintMessage;

    property Message: TLintMessage read FMessage;
    property Id: Integer read FId;
  end;

  ILintServerConnection = interface
    ['{89B2C950-2364-4A9B-B7B3-3B4448399FB9}']
    function GetConnected: Boolean;
    procedure SendMessage(Msg: TTaggedMessage);
    function ReceiveMessage: TTaggedMessage;
    function ReceiveMessageWithTimeout(Timeout: Integer): TTaggedMessage;
    property Connected: Boolean read GetConnected;
  end;

  TLintServerTcpConnection = class(TInterfacedObject, ILintServerConnection)
  private
    FTcpClient: TIdTCPClient;

    function DoReceiveMessage: TTaggedMessage;
    procedure DoSendMessage(Msg: TTaggedMessage);

    function GetConnected: Boolean;
  public
    constructor Create(Host: string; Port: Integer);
    destructor Destroy; override;

    procedure SendMessage(Msg: TTaggedMessage);
    function ReceiveMessage: TTaggedMessage;
    function ReceiveMessageWithTimeout(Timeout: Integer): TTaggedMessage;

    property Connected: Boolean read GetConnected;
  end;

  TLintServer = class(TObject)
  private
    FConnection: ILintServerConnection;
    FResponseActions: TDictionary<Integer, TResponseAction>;
    FNextId: Integer;
    FExtProcessHandle: THandle;

    procedure SendMessage(Req: TLintMessage; OnResponse: TResponseAction = nil); overload;
    procedure OnReceivedMessage(Message: TLintMessage; Id: Integer);
    procedure OnUnhandledMessage(Message: TLintMessage);

    function StartExtServer(
      Jar: string;
      JavaExe: string;
      WorkingDir: string;
      ShowConsole: Boolean): Integer;
    procedure StopExtServer;

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

  protected
    function BuildServerConnection(Host: string; Port: Integer): ILintServerConnection; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Process: Boolean;

    procedure Initialize(HostOptions: TSonarHostOptions; DownloadPlugin: Boolean);
    procedure Analyze(
      Options: TAnalyzeOptions;
      OnResult: TAnalyzeResultAction;
      OnError: TErrorAction;
      DownloadPlugin: Boolean = True);
    procedure RetrieveRules(
      SonarOptions: TSonarProjectOptions;
      OnResult: TRuleRetrieveResultAction;
      OnError: TErrorAction;
      DownloadPlugin: Boolean = True);
  end;

  TLintServerThread = class(TThread)
  private
    FLock: TMutex;
    FServer: TLintServer;
    FServerDone: Boolean;

    function AcquireServerPossibleUninit: TLintServer;
  protected
    procedure DoTerminate; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute; override;

    function AcquireServer: TLintServer;
    procedure RefreshServer;
    procedure ReleaseServer;
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    IdGlobal
  , Winapi.Windows
  , IdStack
  , System.IOUtils
  , System.TimeSpan
  , DelphiLint.Utils
  , DelphiLint.Context
  ;

//______________________________________________________________________________________________________________________

constructor TLintMessage.Create(Category: Byte; Data: TJSONValue);
begin
  inherited Create;
  FCategory := Category;
  FData := Data;
end;

//______________________________________________________________________________________________________________________

constructor TLintMessage.Create(Category: Byte);
begin
  inherited Create;
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

class function TLintMessage.Initialize(Options: TInitializeOptions): TLintMessage;
var
  Json: TJSONObject;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestInitialize
  Json := TJSONObject.Create;
  Json.AddPair('bdsPath', Options.BdsPath);
  Json.AddPair('compilerVersion', Options.CompilerVersion);
  Json.AddPair('defaultSonarDelphiJarPath', Options.DefaultSonarDelphiJarPath);
  Json.AddPair('sonarHostUrl', Options.SonarHost.Url);
  Json.AddPair('apiToken', Options.SonarHost.Token);

  Result := TLintMessage.Create(C_Initialize, Json);
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.Analyze(Options: TAnalyzeOptions): TLintMessage;
var
  InputFilesJson: TJSONArray;
  Json: TJSONObject;
  InputFile: string;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestAnalyze
  InputFilesJson := TJSONArray.Create;
  for InputFile in Options.InputFiles do begin
    InputFilesJson.Add(InputFile);
  end;

  Json := TJSONObject.Create;
  Json.AddPair('baseDir', Options.BaseDir);
  Json.AddPair('inputFiles', InputFilesJson);
  Json.AddPair('projectPropertiesPath', Options.ProjectPropertiesPath);
  Json.AddPair('sonarHostUrl', Options.Sonar.Host.Url);
  Json.AddPair('projectKey', Options.Sonar.ProjectKey);
  Json.AddPair('apiToken', Options.Sonar.Host.Token);

  Result := TLintMessage.Create(C_Analyze, Json);
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.RuleRetrieve(Options: TSonarProjectOptions): TLintMessage;
var
  Json: TJSONObject;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestRuleRetrieve
  Json := TJSONObject.Create;
  Json.AddPair('sonarHostUrl', Options.Host.Url);
  Json.AddPair('projectKey', Options.ProjectKey);
  Json.AddPair('apiToken', Options.Host.Token);

  Result := TLintMessage.Create(C_RuleRetrieve, Json);
end;

//______________________________________________________________________________________________________________________

class function TLintMessage.Quit: TLintMessage;
begin
  Result := TLintMessage.Create(C_Quit);
end;

//______________________________________________________________________________________________________________________

function TLintServer.BuildServerConnection(Host: string; Port: Integer): ILintServerConnection;
begin
  Result := TLintServerTcpConnection.Create(Host, Port);
end;

//______________________________________________________________________________________________________________________

constructor TLintServer.Create;
var
  Port: Integer;
begin
  inherited;

  FNextId := 1;
  FResponseActions := TDictionary<Integer, TResponseAction>.Create;

  if not LintContext.ValidateSetup then begin
    raise ELintServerMisconfigured.Create('DelphiLint external resources are misconfigured');
  end;

  if LintContext.Settings.DebugExternalServer then begin
    Port := 14000;
    Log.Info('Attempting to connect to DelphiLint server on port %d', [Port]);
    Log.Info('If there is no server on the given port, the application may crash');
  end
  else begin
    Port := StartExtServer(
      LintContext.Settings.ServerJar,
      LintContext.Settings.JavaExe,
      LintContext.Settings.SettingsDirectory,
      LintContext.Settings.DebugShowConsole);
  end;

  try
    FConnection := BuildServerConnection('127.0.0.1', Port);
  except
    on EIdSocketError do begin
      raise ELintPortRefused.Create('Connection refused for TCP client');
    end;
  end;

  Log.Info('Connection initialised to DelphiLint server on port %d', [Port]);
end;

//______________________________________________________________________________________________________________________

destructor TLintServer.Destroy;
begin
  StopExtServer;
  FConnection := nil;
  FreeAndNil(FResponseActions);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TLintServer.Process: Boolean;
var
  TaggedMsg: TTaggedMessage;
  Msg: TLintMessage;
begin
  Result := True;
  try
    TaggedMsg := FConnection.ReceiveMessageWithTimeout(50);
    try
      if Assigned(TaggedMsg) then begin
        Msg := TaggedMsg.Extract;
        OnReceivedMessage(Msg, TaggedMsg.Id);
      end;
    finally
      FreeAndNil(TaggedMsg);
    end;
  except
    on E: ELintServerFailed do begin
      Result := False;
    end;
    on E: Exception do begin
      Log.Warn('Error occurred in server thread: ' + E.Message);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Initialize(HostOptions: TSonarHostOptions; DownloadPlugin: Boolean);
var
  InitializeOptions: TInitializeOptions;
  InitializeMsg: TLintMessage;
  InitializeCompleted: Boolean;
  ErrorMsg: string;
  TimeWaitStarted: TDateTime;
begin
  Log.Debug('Requesting initialization');

  if not LintContext.ValidateSetup then begin
    raise ELintServerMisconfigured.Create('DelphiLint external resources are misconfigured');
  end;

  InitializeOptions := TInitializeOptions.Create(
    LintContext.IDEServices.GetRootDirectory,
    GetDelphiVersion,
    LintContext.Settings.SonarDelphiJar
  );

  if DownloadPlugin then begin
    InitializeOptions.SonarHost := HostOptions;
  end;

  try
    InitializeMsg := TLintMessage.Initialize(InitializeOptions);
    InitializeCompleted := False;

    SendMessage(
      InitializeMsg,
      procedure(const Response: TLintMessage) begin
        if Response.Category = C_Initialized then begin
          InitializeCompleted := True;
        end
        else begin
          ErrorMsg := Response.Data.Value;
          Log.Warn('Initialize error (%d): %s', [Response.Category, ErrorMsg]);
        end;
      end);
  finally
    FreeAndNil(InitializeMsg);
  end;

  TimeWaitStarted := Now;
  while TTimeSpan.Subtract(Now, TimeWaitStarted).TotalSeconds < 3 do begin
    Process;

    if InitializeCompleted or (ErrorMsg <> '') then begin
      Break;
    end;
  end;

  if not InitializeCompleted then begin
    Log.Warn('Initialize was not successful');

    if (ErrorMsg <> '') then begin
      raise ELintServerFailed.Create(ErrorMsg);
    end
    else begin
      raise ELintServerTimedOut.Create('Initialize timed out');
    end;
  end;

  Log.Debug('Initialized successfully');
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Analyze(
  Options: TAnalyzeOptions;
  OnResult: TAnalyzeResultAction;
  OnError: TErrorAction;
  DownloadPlugin: Boolean = True);
var
  AnalyzeMsg: TLintMessage;
begin
  try
    Initialize(Options.Sonar.Host, DownloadPlugin);
  except
    on E: ELintServerError do begin
      OnError(E.Message);
      Exit;
    end;
  end;

  Log.Debug('Sending analysis request to server');
  AnalyzeMsg := TLintMessage.Analyze(Options);
  try
    SendMessage(
      AnalyzeMsg,
      procedure (const Response: TLintMessage) begin
        OnAnalyzeResponse(Response, OnResult, OnError);
      end);
  finally
    FreeAndNil(AnalyzeMsg);
  end;
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
      Result.Add(TLintIssue.CreateFromJson(Json[Index] as TJSONObject));
    end;
  end;

var
  Issues: TObjectList<TLintIssue>;
  ErrorMsg: string;
  ErrorCat: Byte;
begin
  if Response.Category <> C_AnalyzeResult then begin
    ErrorMsg := Response.Data.Value;
    ErrorCat := Response.Category;

    Log.Warn('Analysis returned error (%d): %s', [ErrorCat, ErrorMsg]);
    OnError(ErrorMsg);
  end
  else begin
    Issues := ParseIssues(Response.Data.GetValue<TJSONArray>('issues'));

    Log.Info('Analysis returned %d issues', [Issues.Count]);
    OnResult(Issues);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.RetrieveRules(
  SonarOptions: TSonarProjectOptions;
  OnResult: TRuleRetrieveResultAction;
  OnError: TErrorAction;
  DownloadPlugin: Boolean = True
);
var RuleRetrieveMsg: TLintMessage;
begin
  try
    Initialize(SonarOptions.Host, DownloadPlugin);
  except
    on E: ELintServerError do begin
      OnError(E.Message);
      Exit;
    end;
  end;

  RuleRetrieveMsg := TLintMessage.RuleRetrieve(SonarOptions);

  try
    SendMessage(
      RuleRetrieveMsg,
      procedure (const Response: TLintMessage) begin
        OnRuleRetrieveResponse(Response, OnResult, OnError);
      end);
  finally
    FreeAndNil(RuleRetrieveMsg);
  end;
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
      Result.Add(Pair.JsonString.Value, TRule.CreateFromJson(TJSONObject(Pair.JsonValue)));
    end;
  end;

var
  ErrorMsg: string;
  ErrorCat: Byte;
  Rules: TObjectDictionary<string, TRule>;
begin
  if Response.Category <> C_RuleRetrieveResult then begin
    ErrorMsg := Response.Data.AsType<string>;
    ErrorCat := Response.Category;

    Log.Warn('Rule retrieve returned error (%d): %s', [ErrorCat, ErrorMsg]);
    OnError(ErrorMsg);
  end
  else begin
    Rules := ParseRules(Response.Data.GetValue<TJSONObject>('rules'));

    Log.Info('Rule retrieve returned %d rules', [Rules.Count]);
    OnResult(Rules);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnUnhandledMessage(Message: TLintMessage);
begin
  Log.Warn('Unhandled message (%d) received: <%s>', [Message.Category, Message.Data]);
end;

//______________________________________________________________________________________________________________________

function TLintServer.StartExtServer(
  Jar: string;
  JavaExe: string;
  WorkingDir: string;
  ShowConsole: Boolean): Integer;
const
  C_Title = 'DelphiLint Server';
var
  CommandLine: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ErrorCode: Integer;
  CreationFlags: Cardinal;
  PortFile: string;
  PortStr: string;
  ClientChangedTime: TDateTime;
  ChangedTime: TDateTime;
  TempChangedTime: TDateTime;
  Attempts: Integer;
begin
  if not FileExists(Jar) then begin
    raise ELintServerMisconfigured.CreateFmt('Server jar not found at path "%s"', [Jar]);
  end;

  if not FileExists(JavaExe) then begin
    raise ELintServerMisconfigured.CreateFmt('Java executable not found at path "%s"', [JavaExe]);
  end;

  PortFile := TPath.GetTempFileName;
  if not FileAge(PortFile, ClientChangedTime) then begin
    raise EFileNotFoundException.Create('Could not get details of temp file containing port');
  end;

  CommandLine := Format(' -jar "%s" "%s"', [Jar, PortFile]);

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

  FExtProcessHandle := ProcessInfo.hProcess;
  CloseHandle(ProcessInfo.hThread);

  ChangedTime := ClientChangedTime;
  Attempts := 0;
  repeat
    Sleep(50);
    if FileAge(PortFile, TempChangedTime) then begin
      ChangedTime := TempChangedTime;
    end;
    Inc(Attempts);
  until (ChangedTime <> ClientChangedTime) or (Attempts > 50);

  if ChangedTime = ClientChangedTime then begin
    raise ELintServerFailed.Create('Server failed to communicate port');
  end;

  PortStr := '';
  Attempts := 0;
  repeat
    try
      PortStr := TFile.ReadAllText(PortFile);
    except
      on EReadError do begin
        // File was locked, try again
        Sleep(50);
      end;
    end;
    Inc(Attempts);
  until (PortStr <> '') or (Attempts > 5);

  if (PortStr = '') then begin
    raise ELintServerFailed.Create('Server port file was unreadable');
  end
  else if not IsNumeric(PortStr) then begin
    raise ELintServerFailed.CreateFmt('Server reported nonsense port "%s"', [PortStr]);
  end;
  TFile.Delete(PortFile);

  Result := StrToInt(PortStr);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.StopExtServer;
var
  QuitMsg: TLintMessage;
begin
  if FConnection.Connected then begin
    QuitMsg := TLintMessage.Quit;
    try
      SendMessage(QuitMsg);
    finally
      FreeAndNil(QuitMsg);
    end;

    Log.Debug('Quit message sent to the server. Waiting for termination');
  end;

  FConnection := nil;

  if FExtProcessHandle <> 0 then begin
    if WaitForSingleObject(FExtProcessHandle, 1000) <> WAIT_OBJECT_0 then begin
      Log.Warn('External server did not terminate gracefully. Force terminating');
      TerminateProcess(FExtProcessHandle, 1);
    end;
    CloseHandle(FExtProcessHandle);

    Log.Info('External server terminated');
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.SendMessage(Req: TLintMessage; OnResponse: TResponseAction = nil);
var
  Id: Integer;
  TaggedMsg: TTaggedMessage;
begin
  Id := FNextId;
  Inc(FNextId);
  if Assigned(OnResponse) then begin
    FResponseActions.Add(Id, OnResponse);
  end;

  TaggedMsg := TTaggedMessage.Create(Req, Id);
  try
    FConnection.SendMessage(TaggedMsg);
  finally
    FreeAndNil(TaggedMsg);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnReceivedMessage(Message: TLintMessage; Id: Integer);
begin
  try
    if FResponseActions.ContainsKey(Id) then begin
      try
        FResponseActions[Id](Message);
      except
        on E: Exception do begin
          Log.Warn(
            'Registered handler for incoming message (type %d) failed with exception %s %s',
            [Message.Category, E.ClassName, E.Message]);
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

function TLintServerThread.AcquireServerPossibleUninit: TLintServer;
begin
  FLock.Acquire;
  Result := FServer;
end;

//______________________________________________________________________________________________________________________

function TLintServerThread.AcquireServer: TLintServer;
begin
  if FServerDone then begin
    raise ELintServerError.Create('Server acquisition attempted from terminated thread');
  end;

  FLock.Acquire;
  try
    if not Assigned(FServer) then begin
      FServer := TLintServer.Create;
    end;
  except
    on E: Exception do begin
      FLock.Release;
      raise;
    end;
  end;

  Result := FServer;
end;

//______________________________________________________________________________________________________________________

procedure TLintServerThread.RefreshServer;
begin
  try
    FreeAndNil(FServer);
  except
    on E: Exception do begin
      Log.Warn('Refreshing server failed as an error was raised when freeing the old server: %s', [E.Message]);
    end;
  end;

  FServer := TLintServer.Create;
end;

//______________________________________________________________________________________________________________________

procedure TLintServerThread.ReleaseServer;
begin
  FLock.Release;
end;

//______________________________________________________________________________________________________________________

constructor TLintServerThread.Create;
begin
  inherited;
  FLock := TMutex.Create;
end;

//______________________________________________________________________________________________________________________

destructor TLintServerThread.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FLock);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintServerThread.DoTerminate;
begin
  // TThread.DoTerminate calls OnTerminate on the main thread using Synchronize.
  // This is a bizarre choice that has the potential for thread cycles.
  if Assigned(OnTerminate) then begin
    OnTerminate(Self);
  end;

  Log.Debug('Server thread terminated');
end;

//______________________________________________________________________________________________________________________

procedure TLintServerThread.Execute;
begin
  inherited;
  Log.Debug('Server thread started');

  while not Terminated do begin
    AcquireServerPossibleUninit;
    try
      if not Terminated then begin
        if Assigned(FServer) and not FServer.Process then begin
          RefreshServer;
        end;
      end;
    finally
      ReleaseServer;
    end;
  end;

  Log.Debug('Server thread terminating');

  AcquireServerPossibleUninit;
  try
    FreeAndNil(FServer);
    FServerDone := True;
  finally
    ReleaseServer;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TLintServerTcpConnection.Create(Host: string; Port: Integer);
begin
  inherited Create;

  FTcpClient := TIdTCPClient.Create;
  FTcpClient.Host := Host;
  FTcpClient.Port := Port;
  FTcpClient.ConnectTimeout := 2000;

  try
    FTcpClient.Connect;
  except
    on EIdSocketError do begin
      raise ELintPortRefused.Create('Connection refused for TCP client');
    end;
  end;
end;

//______________________________________________________________________________________________________________________

destructor TLintServerTcpConnection.Destroy;
begin
  FreeAndNil(FTcpClient);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TLintServerTcpConnection.DoReceiveMessage: TTaggedMessage;
var
  Id: SmallInt;
  Category: Byte;
  Length: Integer;
  DataBuffer: TBytes;
  IdDataBuffer: TIdBytes;
  DataJsonValue: TJSONValue;
begin
  Category := FTcpClient.IOHandler.ReadByte;
  Id := FTcpClient.IOHandler.ReadInt32;
  Length := FTcpClient.IOHandler.ReadInt32;
  FTcpClient.IOHandler.ReadBytes(IdDataBuffer, Length);

  DataBuffer := TBytes(IdDataBuffer);

  DataJsonValue := TJSONValue.ParseJSONValue(DataBuffer, 0, Length, True);
  Result := TTaggedMessage.Create(TLintMessage.Create(Category, DataJsonValue), Id);
end;

//______________________________________________________________________________________________________________________

procedure TLintServerTcpConnection.DoSendMessage(Msg: TTaggedMessage);
var
  DataBytes: TArray<Byte>;
  DataByte: Byte;
  Message: TLintMessage;
begin
  FTcpClient.IOHandler.Write(Msg.Message.Category);
  FTcpClient.IOHandler.Write(Msg.Id);

  Message := Msg.Extract;

  if Assigned(Message.Data) then begin
    DataBytes := TEncoding.UTF8.GetBytes(Message.Data.ToString);

    FTcpClient.IOHandler.Write(Length(DataBytes));
    for DataByte in DataBytes do begin
      FTcpClient.IOHandler.Write(DataByte);
    end;
  end
  else begin
    FTcpClient.IOHandler.Write(Integer(0));
  end;
end;

//______________________________________________________________________________________________________________________

function TLintServerTcpConnection.GetConnected: Boolean;
begin
  FTcpClient.CheckForGracefulDisconnect(False);
  Result := FTcpClient.Connected;
end;

//______________________________________________________________________________________________________________________

function TLintServerTcpConnection.ReceiveMessage: TTaggedMessage;
begin
  Result := ReceiveMessageWithTimeout(0);
end;

//______________________________________________________________________________________________________________________

procedure TLintServerTcpConnection.SendMessage(Msg: TTaggedMessage);
begin
  try
    DoSendMessage(Msg);
  except
    on E: EIdSocketError do begin
      Log.Warn('Socket error when sending message: ' + E.Message);

      FTcpClient.CheckForGracefulDisconnect(False);
      if not FTcpClient.Connected then begin
        Log.Warn('TCP connection was unexpectedly terminated');
      end;

      raise ELintServerFailed.Create('Server connection was unexpectedly terminated');
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintServerTcpConnection.ReceiveMessageWithTimeout(Timeout: Integer): TTaggedMessage;
begin
  Result := nil;

  try
    if (Timeout = 0) or FTcpClient.IOHandler.CheckForDataOnSource(Timeout) then begin
      Result := DoReceiveMessage;
    end;
  except
    on E: EIdSocketError do begin
      Log.Warn('Socket error in server thread: ' + E.Message);

      if not Connected then begin
        Log.Warn('TCP connection was unexpectedly terminated');
      end;

      raise ELintServerFailed.Create('Server connection was unexpectedly terminated');
    end;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TTaggedMessage.Create(Message: TLintMessage; Id: Integer);
begin
  inherited Create;

  FMessage := Message;
  FId := Id;
end;

//______________________________________________________________________________________________________________________

destructor TTaggedMessage.Destroy;
begin
  FreeAndNil(FMessage);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TTaggedMessage.Extract: TLintMessage;
begin
  Result := FMessage;
  FMessage := nil;
end;

end.
