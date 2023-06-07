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
    FData: TJsonValue;
  public
    constructor Create(Category: Byte); overload;
    constructor Create(Category: Byte; Data: TJsonValue); overload;
    destructor Destroy; override;
    class function Initialize(Data: TJsonObject): TLintMessage; static;
    class function Analyze(Data: TJsonObject): TLintMessage; static;
    class function Quit: TLintMessage; static;
    property Category: Byte read FCategory;
    property Data: TJsonValue read FData;
  end;

//______________________________________________________________________________________________________________________

  TLintResponseAction = reference to procedure (Message: TLintMessage);
  TLintAnalyzeAction = reference to procedure(Issues: TArray<TLintIssue>);
  TLintErrorAction = reference to procedure(Message: string);

  TLintServer = class(TThread)
  private
    FTcpClient: TIdTCPClient;
    FResponseActions: TDictionary<Integer, TLintResponseAction>;
    FNextId: Integer;
    FTcpLock: TMutex;

    procedure SendMessage(Req: TLintMessage; OnResponse: TLintResponseAction); overload;
    procedure SendMessage(Req: TLintMessage; Id: Integer); overload;
    procedure OnUnhandledMessage(Message: TLintMessage);
    // This method is NOT threadsafe and should only ever be called in the lint server thread.
    procedure ReceiveMessage;

    procedure StartExtServer(Jar: string; JavaExe: string; Port: Integer; ShowConsole: Boolean);
    procedure StopExtServer;

  public
    constructor Create(Port: Integer);
    destructor Destroy; override;

    procedure Execute; override;

    procedure Initialize;
    procedure Analyze(
      BaseDir: string;
      DelphiFiles: array of string;
      OnAnalyze: TLintAnalyzeAction;
      OnError: TLintErrorAction;
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
  , Winapi.ShellAPI
  , DelphiLint.Settings
  ;

//______________________________________________________________________________________________________________________

class function TLintMessage.Analyze(Data: TJsonObject): TLintMessage;
begin
  Result := TLintMessage.Create(C_Analyze, Data);
end;

//______________________________________________________________________________________________________________________

constructor TLintMessage.Create(Category: Byte; Data: TJsonValue);
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

class function TLintMessage.Initialize(Data: TJsonObject): TLintMessage;
begin
  Result := TLintMessage.Create(C_Initialize, Data);
end;

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

  FResponseActions := TDictionary<Integer, TLintResponseAction>.Create;

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

procedure TLintServer.Analyze(
  BaseDir: string;
  DelphiFiles: array of string;
  OnAnalyze: TLintAnalyzeAction;
  OnError: TLintErrorAction;
  SonarHostUrl: string = '';
  ProjectKey: string = '');
var
  Index: Integer;
  RequestJson: TJsonObject;
  InputFilesJson: TJsonArray;
begin
  Log.Info('Requesting analysis.');
  Initialize;

  InputFilesJson := TJsonArray.Create;

  for Index := 0 to Length(DelphiFiles) - 1 do begin
    InputFilesJson.Add(DelphiFiles[Index]);
  end;

  // JSON representation of au.com.integradev.delphilint.messaging.RequestAnalyze
  RequestJson := TJsonObject.Create;
  RequestJson.AddPair('baseDir', BaseDir);
  RequestJson.AddPair('inputFiles', InputFilesJson);
  RequestJson.AddPair('sonarHostUrl', SonarHostUrl);
  RequestJson.AddPair('projectKey', ProjectKey);

  SendMessage(
    TLintMessage.Analyze(RequestJson),
    procedure(Response: TLintMessage)
    var
      IssuesArrayJson: TJsonArray;
      Index: Integer;
      Issues: TArray<TLintIssue>;
    begin
      Synchronize(
        procedure begin
          Log.Info(Format('Analysis response received (%d)', [Response.Category]));
        end);

      if Response.Category <> C_AnalyzeResult then begin
        Synchronize(
          procedure begin
            Log.Info(Format('Analyze error (%d): %s', [Response.Category, Response.Data.AsType<string>]));
            OnError(Response.Data.AsType<string>);
          end);
      end
      else begin
        if Response.Data.TryGetValue<TJSONArray>('issues', IssuesArrayJson) then begin
          SetLength(Issues, IssuesArrayJson.Count);

          for Index := 0 to IssuesArrayJson.Count - 1 do begin
            Issues[Index] := TLintIssue.FromJson(IssuesArrayJson[Index] as TJsonObject);
          end;
        end;

        Synchronize(
          procedure begin
            Log.Info('Calling post-analyze action.');
            OnAnalyze(Issues);
          end);
      end;

      FreeAndNil(Response);
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.Initialize;
const
  // TODO: Expose compiler version as a setting
  C_CompilerVersion = 'VER350';
  C_LanguageKey = 'delph';
var
  DataJson: TJsonObject;
  InitializeCompletedEvent: TEvent;
begin
  Log.Info('Requesting initialization.');

  // JSON representation of au.com.integradev.delphilint.messaging.RequestInitialize
  DataJson := TJSONObject.Create;
  DataJson.AddPair('bdsPath', (BorlandIDEServices as IOTAServices).GetRootDirectory);
  DataJson.AddPair('compilerVersion', C_CompilerVersion);
  DataJson.AddPair('sonarDelphiJarPath', LintSettings.SonarDelphiJar);

  InitializeCompletedEvent := TEvent.Create;

  SendMessage(
    TLintMessage.Initialize(DataJson),
    procedure(Response: TLintMessage) begin
      InitializeCompletedEvent.SetEvent;

      if Response.Category <> C_Initialized then begin
        Queue(
          procedure begin
            Log.Info(Format('Initialize error (%d): %s', [Response.Category, Response.Data.ToString]));
          end);
      end;
    end);

  if InitializeCompletedEvent.WaitFor(C_Timeout) <> wrSignaled then begin
    raise Exception.Create('Initialize timed out');
  end;

  Log.Info('Initialization complete.');
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

  FTcpLock.Release;
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
  SendMessage(
    TLintMessage.Quit,
    procedure(Msg: TLintMessage)
    begin
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.SendMessage(Req: TLintMessage; OnResponse: TLintResponseAction);
var
  Id: SmallInt;
begin
  FTcpLock.Acquire;

  Id := FNextId;
  Inc(FNextId);
  FResponseActions.Add(Id, OnResponse);

  FTcpLock.Release;

  SendMessage(Req, Id);
end;

//______________________________________________________________________________________________________________________

procedure TLintServer.ReceiveMessage;
var
  Id: SmallInt;
  Category: Byte;
  Length: Integer;
  DataStr: string;
  DataJsonValue: TJsonValue;
  Message: TLintMessage;
begin
  Category := FTcpClient.IOHandler.ReadByte;
  Id := FTcpClient.IOHandler.ReadInt32;
  Length := FTcpClient.IOHandler.ReadInt32;
  DataStr := FTcpClient.IOHandler.ReadString(Length, IndyTextEncoding_UTF8);
  DataJsonValue := TJsonValue.ParseJSONValue(DataStr);

  Message := TLintMessage.Create(Category, DataJsonValue);

  if FResponseActions.ContainsKey(Id) then begin
    FResponseActions[Id](Message);
  end
  else begin
    OnUnhandledMessage(Message);
  end;
end;

//______________________________________________________________________________________________________________________

end.
