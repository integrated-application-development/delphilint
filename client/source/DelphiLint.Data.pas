unit DelphiLint.Data;

interface

uses
    System.JSON
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

  TLintIssue = class(TObject)
  private
    FRuleKey: string;
    FMessage: string;
    FFilePath: string;
    FRange: TRange;

  public
    property RuleKey: string read FRuleKey;
    property Message: string read FMessage;
    property FilePath: string read FFilePath;
    property Range: TRange read FRange write FRange;

    constructor FromJson(Json: TJsonObject);
  end;

//______________________________________________________________________________________________________________________

implementation

//______________________________________________________________________________________________________________________

constructor TRange.FromJson(Json: TJsonObject);
begin
  StartLine := Json.GetValue<Integer>('startLine', 0);
  EndLine := Json.GetValue<Integer>('endLine', 0);
  StartLineOffset := Json.GetValue<Integer>('startOffset', 0);
  EndLineOffset := Json.GetValue<Integer>('endOffset', 0);
end;

//______________________________________________________________________________________________________________________

constructor TLintIssue.FromJson(Json: TJsonObject);
var
  RangeJson: TJsonValue;
begin
  FRuleKey := Json.GetValue<string>('ruleKey');
  FMessage := Json.GetValue<string>('message', FRuleKey);
  FFilePath := Json.GetValue<string>('file');

  RangeJson := Json.GetValue<TJsonValue>('range', nil);
  if Assigned(RangeJson) and (RangeJson is TJsonObject) then begin
    FRange := TRange.FromJson(RangeJson as TJsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

end.
