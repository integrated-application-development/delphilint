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
  C_InitializeError = 26;
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
      DefaultSonarDelphiJarPath: string;
      SonarHostUrl: string = '';
  ApiToken: string = ''
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
    FExtProcessHandle: THandle;

    procedure SendMessage(Req: TLintMessage; OnResponse: TResponseAction = nil); overload;
    procedure SendMessage(Req: TLintMessage; Id: Integer); overload;
    procedure OnUnhandledMessage(Message: TLintMessage);
    procedure ReceiveMessage;

    function StartExtServer(
      Jar: string;
      JavaExe: string;
      WorkingDir: string;
      ShowConsole: Boolean): Integer;
    procedure StopExtServer;

    procedure OnInitializeResponse(
      const Response: TLintMessage;
      InitializeEvent: TEvent;
      var ErrorMsg: string
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

  protected
    procedure DoTerminate; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute; override;

    procedure Initialize(SonarHostUrl: string; ApiToken: string; DownloadPlugin: Boolean);
    procedure Analyze(
      BaseDir: string;
      DelphiFiles: TArray<string>;
      OnResult: TAnalyzeResultAction;
      OnError: TErrorAction;
      SonarHostUrl: string = '';
      ProjectKey: string = '';
      ApiToken: string = '';
      ProjectPropertiesPath: string = '';
      DownloadPlugin: Boolean = True);
    procedure RetrieveRules(
      SonarHostUrl: string;
      ProjectKey: string;
      OnResult: TRuleRetrieveResultAction;
      OnError: TErrorAction;
      ApiToken: string = '';
      DownloadPlugin: Boolean = True);
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    IdGlobal
  , Winapi.Windows
  , IdStack
  , System.IOUtils
  , DelphiLint.SetupForm
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

class function TLintMessage.Initialize(
  BdsPath: string;
  CompilerVersion: string;
  DefaultSonarDelphiJarPath: string;
  SonarHostUrl: string = '';
  ApiToken: string = ''
): TLintMessage;
var
  Json: TJSONObject;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestInitialize
  Json := TJSONObject.Create;
  Json.AddPair('bdsPath', BdsPath);
  Json.AddPair('compilerVersion', CompilerVersion);
  Json.AddPair('defaultSonarDelphiJarPath', DefaultSonarDelphiJarPath);
  Json.AddPair('sonarHostUrl', SonarHostUrl);
  Json.AddPair('apiToken', ApiToken);

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

constructor TLintServer.Create;
begin
  inherited Create(False);

  FNextId := 1;
  FTcpLock := TMutex.Create;

  FResponseActions := TDictionary<Integer, TResponseAction>.Create;

  FTcpClient := TIdTCPClient.Create;
  FTcpClient.Host := '127.0.0.1';

  if not LintContext.ValidateSetup then begin
    raise ELintServerMisconfigured.Create('DelphiLint external resources are misconfigured');
  end;

  if LintContext.Settings.DebugExternalServer then begin
    FTcpClient.Port := 14000;
  end
  else begin
    FTcpClient.Port := StartExtServer(
      LintContext.Settings.ServerJar,
      LintContext.Settings.JavaExe,
      LintContext.Settings.SettingsDirectory,
      LintContext.Settings.DebugShowConsole);
  end;

  try
    FTcpClient.Connect;
  except
    on EIdSocketError do begin
      raise ELintPortRefused.Create('Connection refused for TCP client');
    end;
  end;

  Log.Info('DelphiLint server connected');
end;

//______________________________________________________________________________________________________________________

destructor TLintServer.Destroy;
begin
  FreeAndNil(FTcpClient);
  FreeAndNil(FResponseActions);
  FreeAndNil(FTcpLock);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.DoTerminate;
begin
  Log.Info('Terminating server thread');
  StopExtServer;

  // TThread.DoTerminate calls OnTerminate on the main thread using Synchronize.
  // This is a bizarre choice that has the potential for thread cycles.
  if Assigned(OnTerminate) then begin
    OnTerminate(Self);
  end;

  Log.Info('Lint server thread terminated');
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Execute;
begin
  while not Terminated do begin
    try
      if FTcpClient.IOHandler.CheckForDataOnSource(50) then begin
        ReceiveMessage;
      end;
    except
      on E: EIdSocketError do begin
        Log.Info('Socket error in server thread: ' + E.Message);

        FTcpClient.CheckForGracefulDisconnect(False);
        if not FTcpClient.Connected then begin
          Log.Info('TCP connection was unexpectedly terminated, terminating server thread');
          Break;
        end;
      end;
      on E: Exception do begin
        Log.Info('Error occurred in server thread: ' + E.Message);
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Initialize(SonarHostUrl: string; ApiToken: string; DownloadPlugin: Boolean);
var
  InitializeMsg: TLintMessage;
  InitializeCompletedEvent: TEvent;
  DownloadUrl: string;
  ErrorMsg: string;
begin
  Log.Info('Requesting initialization...');

  if not LintContext.ValidateSetup then begin
    raise ELintServerMisconfigured.Create('DelphiLint external resources are misconfigured');
  end;

  try
    if DownloadPlugin then begin
      DownloadUrl := SonarHostUrl;
    end
    else begin
      DownloadUrl := '';
    end;

    InitializeMsg := TLintMessage.Initialize(
      LintContext.IDEServices.GetRootDirectory,
      GetDelphiVersion,
      LintContext.Settings.SonarDelphiJar,
      DownloadUrl,
      ApiToken);
    InitializeCompletedEvent := TEvent.Create;

    SendMessage(
      InitializeMsg,
      procedure(const Response: TLintMessage) begin
        OnInitializeResponse(Response, InitializeCompletedEvent, ErrorMsg);
      end);

    if InitializeCompletedEvent.WaitFor(C_Timeout) <> wrSignaled then begin
      raise ELintServerTimedOut.Create('Initialize timed out');
    end
    else if (ErrorMsg <> '') then begin
      Log.Info('Error during initialization');
      raise ELintServerFailed.Create(ErrorMsg);
    end;

    Log.Info('...initialized');
  finally
    FreeAndNil(InitializeMsg);
    FreeAndNil(InitializeCompletedEvent);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.OnInitializeResponse(const Response: TLintMessage; InitializeEvent: TEvent; var ErrorMsg: string);
begin
  if Assigned(InitializeEvent) then begin
    InitializeEvent.SetEvent;
  end;

  if Response.Category <> C_Initialized then begin
    ErrorMsg := Response.Data.Value;
    Log.Info('Initialize error (%d): %s', [Response.Category, ErrorMsg]);
  end
  else begin
    ErrorMsg := '';
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
  ProjectPropertiesPath: string = '';
  DownloadPlugin: Boolean = True);
var
  AnalyzeMsg: TLintMessage;
begin
  Log.Info('Requesting analysis - checking initialization status');
  try
    Initialize(SonarHostUrl, ApiToken, DownloadPlugin);
  except
    on E: ELintServerError do begin
      OnError(E.Message);
      Exit;
    end;
  end;

  Log.Info('Requesting analysis - sending request');
  AnalyzeMsg := TLintMessage.Analyze(BaseDir, DelphiFiles, SonarHostUrl, ProjectKey, ApiToken, ProjectPropertiesPath);
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

    Log.Info('Analysis returned error (%d): %s', [ErrorCat, ErrorMsg]);
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
  SonarHostUrl: string;
  ProjectKey: string;
  OnResult: TRuleRetrieveResultAction;
  OnError: TErrorAction;
  ApiToken: string = '';
  DownloadPlugin: Boolean = True
);
var RuleRetrieveMsg: TLintMessage;
begin
  try
    Initialize(SonarHostUrl, ApiToken, DownloadPlugin);
  except
    on E: ELintServerError do begin
      OnError(E.Message);
      Exit;
    end;
  end;

  RuleRetrieveMsg := TLintMessage.RuleRetrieve(SonarHostUrl, ProjectKey, ApiToken);

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

    Log.Info('Rule retrieve returned error (%d): %s', [ErrorCat, ErrorMsg]);
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
  Log.Info('Unhandled message (%d) received: <%s>', [Message.Category, Message.Data]);
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
  FTcpLock.Acquire;
  try
    FTcpClient.CheckForGracefulDisconnect(False);
    if FTcpClient.Connected then begin
      QuitMsg := TLintMessage.Quit;
      try
        SendMessage(QuitMsg);
      finally
        FreeAndNil(QuitMsg);
      end;
      FTcpClient.Disconnect;
    end;
  finally
    FTcpLock.Release;
  end;

  if FExtProcessHandle <> 0 then begin
    if WaitForSingleObject(FExtProcessHandle, 1000) <> WAIT_OBJECT_0 then begin
      Log.Info('Force terminating external process');
      TerminateProcess(FExtProcessHandle, 1);
    end;
    CloseHandle(FExtProcessHandle);
  end;

  Log.Info('External server terminated');
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
  DataBuffer: TBytes;
  IdDataBuffer: TIdBytes;
  DataJsonValue: TJSONValue;
  Message: TLintMessage;
begin
  FTcpLock.Acquire;
  try
    Category := FTcpClient.IOHandler.ReadByte;
    Id := FTcpClient.IOHandler.ReadInt32;
    Length := FTcpClient.IOHandler.ReadInt32;
    FTcpClient.IOHandler.ReadBytes(IdDataBuffer, Length);
  finally
    FTcpLock.Release;
  end;

  DataBuffer := TBytes(IdDataBuffer);

  DataJsonValue := TJSONValue.ParseJSONValue(DataBuffer, 0, Length, True);
  Message := TLintMessage.Create(Category, DataJsonValue);
  try
    if FResponseActions.ContainsKey(Id) then begin
      try
        FResponseActions[Id](Message);
      except
        on E: Exception do begin
          Log.Info(
            'Registered handler for incoming message (type %d) failed with exception %s %s',
            [Category, E.ClassName, E.Message]);
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
