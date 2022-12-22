unit DelphiLintServer;

interface

uses
    IdHTTP
  , JSON
  , System.Classes
  , System.SysUtils
  ;

const
  C_Initialize = 'INITIALIZE';
  C_Analyze = 'ANALYZE';
  C_AnalyzeResult = 'ANALYZE_RESULT';
  C_AnalyzeError = 'ANALYZE_ERROR';
  C_Initialized = 'INITIALIZED';
  C_Uninitialized = 'UNINITIALIZED';
  C_InvalidRequest = 'INVALID_REQUEST';
  C_UnexpectedError = 'UNEXPECTED_ERROR';

type

//______________________________________________________________________________________________________________________

  TDelphiLintMessage = record
  public
    Category: String;
    Data: TJsonObject;

    constructor Create(Category: String; Data: TJsonObject);
    class function Initialize(Data: TJsonObject): TDelphiLintMessage; static;
    class function Analyze(Data: TJsonObject): TDelphiLintMessage; static;
  end;

//______________________________________________________________________________________________________________________

  TDelphiLintServer = class(TObject)
  private
    FHttp: TIdHttp;
    FServerUrl: String;
    FSonarHostUrl: String;

    function Request(Req: TDelphiLintMessage): TDelphiLintMessage;

  public
    constructor Create(SonarHostUrl: String);
    destructor Destroy; override;

    function Initialize: Boolean;
//    function Analyze(DelphiFiles: array of String): TArray<DelphiLintIssue>;
  end;

//______________________________________________________________________________________________________________________

implementation

//______________________________________________________________________________________________________________________

class function TDelphiLintMessage.Analyze(Data: TJsonObject): TDelphiLintMessage;
begin
  Result := TDelphiLintMessage.Create(C_Analyze, Data);
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintMessage.Create(Category: String; Data: TJsonObject);
begin
  Self.Category := Category;
  Self.Data := Data;
end;

//______________________________________________________________________________________________________________________

class function TDelphiLintMessage.Initialize(Data: TJsonObject): TDelphiLintMessage;
begin
  Result := TDelphiLintMessage.Create(C_Initialize, Data);
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintServer.Create(SonarHostUrl: String);
begin
  FHttp := TIdHTTP.Create;
  FServerUrl := 'http://localhost:14000';
  FSonarHostUrl := SonarHostUrl;
end;

//______________________________________________________________________________________________________________________

destructor TDelphiLintServer.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

//______________________________________________________________________________________________________________________
//
//procedure TDelphiLintServer.Analyze(DelphiFiles: array of String);
//begin
//
//end;
//
//______________________________________________________________________________________________________________________

function TDelphiLintServer.Initialize: Boolean;
const
  C_BdsPath = '{PATH REMOVED} Files (x86)/Embarcadero/Studio/22.0';
  C_CompilerVersion = 'VER350';
  C_LanguageKey = 'delph';
var
  DataJson: TJsonObject;
  Response: TDelphiLintMessage;
begin
  // JSON representation of au.com.integradev.delphilint.messaging.RequestAnalyze
  DataJson := TJSONObject.Create;
  DataJson.AddPair('bdsPath', C_BdsPath);
  DataJson.AddPair('compilerVersion', C_CompilerVersion);
  DataJson.AddPair('sonarHostUrl', FSonarHostUrl);
  DataJson.AddPair('projectKey', '');
  DataJson.AddPair('languageKey', C_LanguageKey);

  Response := Request(TDelphiLintMessage.Initialize(DataJson));
  Result := (Response.Category = C_Initialized);
end;

//______________________________________________________________________________________________________________________

function TDelphiLintServer.Request(Req: TDelphiLintMessage): TDelphiLintMessage;
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
  FHttp.Post(FServerUrl, RequestStream, ResponseStream);

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

end.
