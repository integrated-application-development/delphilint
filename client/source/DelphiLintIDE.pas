unit DelphiLintIDE;

interface

uses
    System.SysUtils
  , System.Classes
  , IdHTTP
  , JSON
  , ToolsAPI
  ;

type
  TDelphiLintMessage = record
  public
    Category: String;
    Data: TJsonObject;

    constructor Create(Category: String; Data: TJsonObject);
    class function Initialize(Data: TJsonObject): TDelphiLintMessage; static;
    class function Analyze(Data: TJsonObject): TDelphiLintMessage; static;
  end;

  TDelphiLint = class(TObject)
  private
    FHttp: TIdHttp;
    FUrl: String;
    FSonarHostUrl: String;

    function Request(Req: TDelphiLintMessage): TDelphiLintMessage;

  public
    constructor Create(SonarHostUrl: String);
    destructor Destroy; override;

    procedure Initialize;
    procedure Analyze(DelphiFiles: array of String);
  end;

  TDelphiLintIDE = class(TObject)
  private
    FDelphiLint: TDelphiLint;
  public
    constructor Create;
    destructor Destroy; override;

    property DelphiLint: TDelphiLint read FDelphiLint;
  end;

  TDelphiLintMenuItem = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
    function GetIDString: String;
    function GetName: String;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: String;
  end;

procedure Register;

var
  DelphiLintIDE: TDelphiLintIDE;

implementation

procedure Register;
begin
  RegisterPackageWizard(TDelphiLintMenuItem.Create);
end;

constructor TDelphiLintIDE.Create;
begin
  inherited;
  FDelphiLint := TDelphiLint.Create('{URL REMOVED}');
end;

destructor TDelphiLintIDE.Destroy;
begin
  FreeAndNil(FDelphiLint);
  inherited;
end;

{ TDelphiLint }

constructor TDelphiLint.Create(SonarHostUrl: String);
begin
  FHttp := TIdHTTP.Create;
  FUrl := 'http://localhost:14000';
  FSonarHostUrl := SonarHostUrl;
end;

destructor TDelphiLint.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

procedure TDelphiLint.Analyze(DelphiFiles: array of String);
begin

end;

procedure TDelphiLint.Initialize;
const
  C_BdsPath = '{PATH REMOVED} Files (x86)/Embarcadero/Studio/22.0';
  C_CompilerVersion = 'VER350';
  C_LanguageKey = 'delph';
var
  DataJson: TJsonObject;
  Response: TDelphiLintMessage;
begin
  // See au.com.integradev.delphilint.messaging.RequestAnalyze
  DataJson := TJSONObject.Create;
  DataJson.AddPair('bdsPath', C_BdsPath);
  DataJson.AddPair('compilerVersion', C_CompilerVersion);
  DataJson.AddPair('sonarHostUrl', FSonarHostUrl);
  DataJson.AddPair('projectKey', '');
  DataJson.AddPair('languageKey', C_LanguageKey);

  Response := Request(TDelphiLintMessage.Initialize(DataJson));
  Assert(Response.Category = 'INITIALIZED');
end;

function TDelphiLint.Request(Req: TDelphiLintMessage): TDelphiLintMessage;
var
  ReqJson: TJsonObject;
  RequestStream: TStringStream;
  ResponseStream: TStringStream;
  RespStr: String;
  RespJson: TJsonObject;
  RespDataJson: TJsonValue;
begin
  ReqJson := TJSONObject.Create;
  ReqJson.AddPair('category', Req.Category);
  ReqJson.AddPair('data', Req.Data);

  RequestStream := TStringStream.Create(ReqJson.ToString);
  ResponseStream := TStringStream.Create;
  FHttp.Post(FUrl, RequestStream, ResponseStream);

  ResponseStream.Position := 0;
  RespStr := ResponseStream.ReadString(ResponseStream.Size);
  RespJson := TJsonObject.ParseJSONValue(RespStr) as TJsonObject;

  Result.Category := RespJson.GetValue<String>('category');
  if RespJson.TryGetValue<TJsonValue>('data', RespDataJson) then begin
    if RespDataJson is TJsonObject then begin
      Result.Data := RespDataJson as TJsonObject;
    end;
  end;
end;

{ TDelphiLintMessage }

class function TDelphiLintMessage.Analyze(Data: TJsonObject): TDelphiLintMessage;
begin
  Result := TDelphiLintMessage.Create('ANALYZE', Data);
end;

constructor TDelphiLintMessage.Create(Category: String; Data: TJsonObject);
begin
  Self.Category := Category;
  Self.Data := Data;
end;

class function TDelphiLintMessage.Initialize(Data: TJsonObject): TDelphiLintMessage;
begin
  Result := TDelphiLintMessage.Create('INITIALIZE', Data);
end;

{ TDelphiLintMenuItem }

procedure TDelphiLintMenuItem.Execute;
begin
  if not Assigned(DelphiLintIDE) then begin
    DelphiLintIDE := TDelphiLintIDE.Create;
  end;

  DelphiLintIDE.DelphiLint.Initialize;
end;

function TDelphiLintMenuItem.GetIDString: String;
begin
  Result := 'DelphiLint';
end;

function TDelphiLintMenuItem.GetMenuText: String;
begin
  Result := 'Analyze Active File with DelphiLint';
end;

function TDelphiLintMenuItem.GetName: String;
begin
  Result := 'DelphiLint';
end;

function TDelphiLintMenuItem.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

initialization

finalization
  FreeAndNil(DelphiLintIDE);

end.
