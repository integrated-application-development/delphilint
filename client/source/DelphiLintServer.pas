unit DelphiLintServer;

interface

uses
    IdHTTP
  , JSON
  , System.Classes
  , System.SysUtils
  , DelphiLintData
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

  TDelphiLintMessage = class(TObject)
  private
    FCategory: String;
    FData: TJsonObject;
  public
    constructor Create(Category: String; Data: TJsonObject);
    destructor Destroy; override;
    class function Initialize(Data: TJsonObject): TDelphiLintMessage; static;
    class function Analyze(Data: TJsonObject): TDelphiLintMessage; static;
    property Category: String read FCategory;
    property Data: TJsonObject read FData;
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
    function Analyze(BaseDir: String; DelphiFiles: array of String): TArray<TDelphiLintIssue>;
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
  FCategory := Category;
  FData := Data;
end;

//______________________________________________________________________________________________________________________

destructor TDelphiLintMessage.Destroy;
begin
  FreeAndNil(FData);
  inherited;
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

function TDelphiLintServer.Analyze(BaseDir: String; DelphiFiles: array of String): TArray<TDelphiLintIssue>;
var
  RequestJson: TJsonObject;
  InputFilesJson: TJsonArray;
  Response: TDelphiLintMessage;
  IssuesJson: TJsonValue;
  IssuesArrayJson: TJsonArray;
  Index: Integer;
begin
  Assert(Initialize, 'Server could not be initialized'); // TODO: proper error handling

  InputFilesJson := TJsonArray.Create;

  for Index := 0 to Length(DelphiFiles) - 1 do begin
    InputFilesJson.Add(DelphiFiles[Index]);
  end;

  // JSON representation of au.com.integradev.delphilint.messaging.RequestAnalyze
  RequestJson := TJsonObject.Create;
  RequestJson.AddPair('baseDir', BaseDir);
  RequestJson.AddPair('inputFiles', InputFilesJson);

  Response := Request(TDelphiLintMessage.Analyze(RequestJson));
  Assert(Response.Category = C_AnalyzeResult); // TODO: proper error handling
  IssuesJson := Response.Data.GetValue('issues');

  if IssuesJson is TJsonArray then begin
    IssuesArrayJson := IssuesJson as TJsonArray;
    SetLength(Result, IssuesArrayJson.Count);

    for Index := 0 to IssuesArrayJson.Count - 1 do begin
      Result[Index] := TDelphiLintIssue.FromJson(IssuesArrayJson[Index] as TJsonObject);
    end;
  end;

  FreeAndNil(Response);
end;

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
  // JSON representation of au.com.integradev.delphilint.messaging.RequestInitialize
  DataJson := TJSONObject.Create;
  DataJson.AddPair('bdsPath', C_BdsPath);
  DataJson.AddPair('compilerVersion', C_CompilerVersion);
  DataJson.AddPair('sonarHostUrl', FSonarHostUrl);
  DataJson.AddPair('projectKey', '');
  DataJson.AddPair('languageKey', C_LanguageKey);

  Response := Request(TDelphiLintMessage.Initialize(DataJson));
  Result := (Response.Category = C_Initialized);

  FreeAndNil(Response);
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
  RespCategory: String;
begin
  ReqJson := TJSONObject.Create;
  ReqJson.AddPair('category', Req.Category);
  ReqJson.AddPair('data', Req.Data);

  RequestStream := TStringStream.Create(ReqJson.ToString);
  ResponseStream := TStringStream.Create;

  FreeAndNil(Req);

  FHttp.Post(FServerUrl, RequestStream, ResponseStream);

  ResponseStream.Position := 0;
  RespStr := ResponseStream.ReadString(ResponseStream.Size);
  RespJson := TJsonObject.ParseJSONValue(RespStr) as TJsonObject;

  RespCategory := RespJson.GetValue<String>('category');

  if RespJson.TryGetValue<TJsonValue>('data', RespDataJson) and (RespDataJson is TJsonObject) then begin
    Result := TDelphiLintMessage.Create(RespCategory, RespDataJson as TJsonObject);
  end
  else begin
    Result := TDelphiLintMessage.Create(RespCategory, nil);
  end;
end;

end.
