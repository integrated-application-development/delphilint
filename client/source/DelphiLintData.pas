unit DelphiLintData;

interface

uses
    JSON
  ;

type

//______________________________________________________________________________________________________________________

  TRange = record
    StartLine: Integer;
    StartLineOffset: Integer;
    EndLine: Integer;
    EndLineOffset: Integer;

    constructor FromJson(Json: TJsonObject);
  end;

//______________________________________________________________________________________________________________________

  TDelphiLintIssue = class(TObject)
  private
    FRuleKey: String;
    FMessage: String;
    FFilePath: String;
    FRange: TRange;

  public
    property RuleKey: String read FRuleKey;
    property Message: String read FMessage;
    property FilePath: String read FFilePath;
    property Range: TRange read FRange;

    constructor FromJson(Json: TJsonObject);
  end;

//______________________________________________________________________________________________________________________

implementation

//______________________________________________________________________________________________________________________

constructor TRange.FromJson(Json: TJsonObject);
begin
  StartLine := Json.GetValue<Integer>('startLine', 0);
  EndLine := Json.GetValue<Integer>('endLine', 0);
  StartLineOffset := Json.GetValue<Integer>('startLineOffset', 0);
  EndLineOffset := Json.GetValue<Integer>('endLineOffset', 0);
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintIssue.FromJson(Json: TJsonObject);
var
  RangeJson: TJsonValue;
begin
  FRuleKey := Json.GetValue<String>('ruleKey');
  FMessage := Json.GetValue<String>('message', FRuleKey);
  FFilePath := Json.GetValue<String>('file');

  RangeJson := Json.GetValue<TJsonValue>('range', nil);
  if Assigned(RangeJson) and (RangeJson is TJsonObject) then begin
    FRange := TRange.FromJson(RangeJson as TJsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

end.
