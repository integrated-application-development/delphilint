unit DelphiLintTest.Data;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.Data
  , System.JSON
  ;

type
  [TestFixture]
  TDataJsonParseTest = class(TObject)
  private
    function Parse<T: TJSONValue>(Text: string): T;
    procedure TestParseRuleType(Str: string; Value: TRuleType);
    procedure TestParseRuleSeverity(Str: string; Value: TRuleSeverity);
  public
    [TestCase]
    procedure TestCreateRule;
    [TestCase]
    procedure TestParseRuleTypes;
    [TestCase]
    procedure TestParseRuleSeverities;
  end;

implementation

uses
    System.SysUtils
  ;

//______________________________________________________________________________________________________________________

function TDataJsonParseTest.Parse<T>(Text: string): T;
begin
  Result := TJSONObject.ParseJSONValue(Text) as T;
end;

//______________________________________________________________________________________________________________________

procedure TDataJsonParseTest.TestCreateRule;
const
  CRuleJsonStr: string = '{"key":"myrulekey","name":"My Rule","desc":"My description for the rule",'
    + '"severity":"MAJOR","type":"CODE_SMELL"}';
var
  JsonObject: TJSONObject;
  Rule: TRule;
begin
  JsonObject := Parse<TJSONObject>(CRuleJsonStr);
  try
    Rule := TRule.CreateFromJson(JsonObject);
    Assert.AreEqual('myrulekey', Rule.RuleKey);
    Assert.AreEqual('My Rule', Rule.Name);
    Assert.AreEqual('My description for the rule', Rule.Desc);
    Assert.AreEqual(rsMajor, Rule.Severity);
    Assert.AreEqual(rtCodeSmell, Rule.RuleType);
  finally
    FreeAndNil(Rule);
    FreeAndNil(JsonObject);
  end;
end;

//______________________________________________________________________________________________________________________


procedure TDataJsonParseTest.TestParseRuleSeverity(Str: string; Value: TRuleSeverity);
const
  CRuleJsonFormatStr: string = '{"key":"","name":"","desc":"","severity":"%s","type":"CODE_SMELL"}';
var
  JsonObject: TJSONObject;
  Rule: TRule;
begin
  JsonObject := Parse<TJSONObject>(Format(CRuleJsonFormatStr, [Str]));
  try
    Rule := TRule.CreateFromJson(JsonObject);
    Assert.AreEqual(Value, Rule.Severity);
  finally
    FreeAndNil(Rule);
    FreeAndNil(JsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TDataJsonParseTest.TestParseRuleType(Str: string; Value: TRuleType);
const
  CRuleJsonFormatStr: string = '{"key":"","name":"","desc":"","severity":"MAJOR","type":"%s"}';
var
  JsonObject: TJSONObject;
  Rule: TRule;
begin
  JsonObject := Parse<TJSONObject>(Format(CRuleJsonFormatStr, [Str]));
  try
    Rule := TRule.CreateFromJson(JsonObject);
    Assert.AreEqual(Value, Rule.RuleType);
  finally
    FreeAndNil(Rule);
    FreeAndNil(JsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TDataJsonParseTest.TestParseRuleTypes;
begin
  TestParseRuleType('CODE_SMELL', rtCodeSmell);
  TestParseRuleType('VULNERABILITY', rtVulnerability);
  TestParseRuleType('SECURITY_HOTSPOT', rtSecurityHotspot);
  TestParseRuleType('BUG', rtBug);
end;

//______________________________________________________________________________________________________________________

procedure TDataJsonParseTest.TestParseRuleSeverities;
begin
  TestParseRuleSeverity('INFO', rsInfo);
  TestParseRuleSeverity('MINOR', rsMinor);
  TestParseRuleSeverity('MAJOR', rsMajor);
  TestParseRuleSeverity('CRITICAL', rsCritical);
  TestParseRuleSeverity('BLOCKER', rsBlocker);
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TDataJsonParseTest);

end.
