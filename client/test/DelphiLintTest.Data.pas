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
    procedure TestCreateRuleNoCleanCode;
    [TestCase]
    procedure TestCreateRuleCleanCode;
    [TestCase]
    procedure TestParseRuleTypes;
    [TestCase]
    procedure TestParseRuleSeverities;
  end;

  [TestFixture]
  TLiveIssueTest = class(TObject)
  public
    [TestCase]
    procedure TestUpdateTetherOnUnchangedLineStaysTethered;
    [TestCase]
    procedure TestUpdateTetherOnChangedLineIsUntethered;
    [TestCase]
    procedure TestUpdateTetherOnChangedRangeIsUntethered;
    [TestCase]
    procedure TestUpdateTetherOnUnchangedLineStaysTetheredMultiline;
    [TestCase]
    procedure TestUpdateTetherOnChangedLineIsUntetheredMultiline;
    [TestCase]
    procedure TestUpdateTetherOnChangedRangeIsUntetheredMultiline;
    [TestCase]
    procedure TestCannotRetether;
    [TestCase]
    procedure TestUpdateTetherOnChangedWhitespaceIsUntethered;
    [TestCase('Down', '3')]
    [TestCase('Up', '-5')]
    [TestCase('Zero', '0')]
    procedure TestMoveLine(Delta: Integer);
    [TestCase]
    procedure TestNewLineMoveSession;
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

procedure TDataJsonParseTest.TestCreateRuleNoCleanCode;
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
    Assert.IsNull(Rule.CleanCode);
  finally
    FreeAndNil(Rule);
    FreeAndNil(JsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TDataJsonParseTest.TestCreateRuleCleanCode;
const
  CRuleJsonStr: string = '{' +
      '"key":"myrulekey",' +
      '"name":"My Rule",' +
      '"desc":"My description for the rule",' +
      '"severity":"MAJOR",' +
      '"type":"CODE_SMELL",' +
      '"cleanCode":{"attribute":"COMPLETE","category":"INTENTIONAL","impacts":{"SECURITY":"HIGH"}}' +
    '}';
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
    Assert.IsNotNull(Rule.CleanCode);
    Assert.AreEqual(ccaComplete, Rule.CleanCode.Attribute);
    Assert.AreEqual(cccIntentional, Rule.CleanCode.Category);
    Assert.AreEqual(1, Rule.CleanCode.Impacts.Count);
    Assert.AreEqual(imsHigh, Rule.CleanCode.Impacts[sqaSecurity]);
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

procedure TLiveIssueTest.TestCannotRetether;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQr__u');
    Assert.IsFalse(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQrstu');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedLineIsUntethered;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQr__u');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedLineIsUntetheredMultiline;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssue.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(7, 'QRst_____');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedRangeIsUntethered;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGH_KLMNOPQrstu');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedRangeIsUntetheredMultiline;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssue.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6, 'I  LM__P');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedWhitespaceIsUntethered;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abcd  GHJ LMN PQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcd    GHJ  LMNPQrstu');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnUnchangedLineStaysTethered;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQrstu');
    Assert.IsTrue(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnUnchangedLineStaysTetheredMultiline;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssue.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6, 'IJKLMNOP');
    Assert.IsTrue(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestMoveLine(Delta: Integer);
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssue.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );

    Assert.AreEqual(LiveIssue.StartLine, 5);
    Assert.AreEqual(LiveIssue.EndLine, 7);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 5);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 7);
    LiveIssue.LinesMoved := Delta;
    Assert.AreEqual(LiveIssue.StartLine, 5 + Delta);
    Assert.AreEqual(LiveIssue.EndLine, 7 + Delta);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 5);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 7);

    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6 + Delta, 'IJKLMNOP');
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6 + Delta, 'abc');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestNewLineMoveSession;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssue.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );

    Assert.AreEqual(LiveIssue.StartLine, 5);
    Assert.AreEqual(LiveIssue.EndLine, 7);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 5);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 7);
    LiveIssue.LinesMoved := 5;
    LiveIssue.NewLineMoveSession;
    Assert.AreEqual(LiveIssue.StartLine, 10);
    Assert.AreEqual(LiveIssue.EndLine, 12);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 10);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 12);

    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(11, 'IJKLMNOP');
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(11, 'abc');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TDataJsonParseTest);
  TDUnitX.RegisterTestFixture(TLiveIssueTest);

end.
