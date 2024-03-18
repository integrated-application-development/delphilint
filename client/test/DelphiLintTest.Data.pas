{
DelphiLint Client
Copyright (C) 2024 Integrated Application Development

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
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
    procedure TestParseIssueStatus(Str: string; Value: TIssueStatus);
  public
    [Test]
    procedure TestCreateRuleNoCleanCode;
    [Test]
    procedure TestCreateRuleCleanCode;
    [Test]
    procedure TestParseRuleTypes;
    [Test]
    procedure TestParseRuleSeverities;
    [Test]
    procedure TestParseIssueStatuses;
    [Test]
    procedure TestCreateIssue;
  end;

  [TestFixture]
  TLiveIssueTest = class(TObject)
  public
    [Test]
    procedure TestIssueWithNoRangeIsSetToEntireFirstLine;
    [Test]
    procedure TestWrongNumberOfAssociatedLinesRaisesRangeError;
    [Test]
    procedure TestTransfersMetadataWhenHasMetadata;
    [Test]
    procedure TestTransfersMetadataWhenNotHasMetadata;
    [Test]
    procedure TestUpdateTetherOnUnchangedLineStaysTethered;
    [Test]
    procedure TestUpdateTetherOnChangedLineIsUntethered;
    [Test]
    procedure TestUpdateTetherOnChangedRangeIsUntethered;
    [Test]
    procedure TestUpdateTetherOnUnchangedLineStaysTetheredMultiline;
    [Test]
    procedure TestUpdateTetherOnChangedLineIsUntetheredMultiline;
    [Test]
    procedure TestUpdateTetherOnChangedRangeIsUntetheredMultiline;
    [Test]
    procedure TestCannotRetether;
    [Test]
    procedure TestUpdateTetherOnChangedWhitespaceIsUntethered;
    [Test]
    [TestCase('Down', '3')]
    [TestCase('Up', '-5')]
    [TestCase('Zero', '0')]
    procedure TestMoveLine(Delta: Integer);
    [Test]
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

procedure TDataJsonParseTest.TestCreateIssue;
const
  CIssueJsonStr: string = '{' +
    '"ruleKey":"rk1",' +
    '"message":"msg1",' +
    '"file":"myfile1",' +
    '"range":{"startLine":5,"startOffset":4,"endLine":6,"endOffset":16},' +
    '"metadata":{"assignee":"myassignee","creationDate":"myDate","status":"REOPENED"}' +
  '}';
var
  JsonObject: TJSONObject;
  Issue: TLintIssue;
begin
  JsonObject := Parse<TJSONObject>(CIssueJsonStr);
  try
    Issue := TLintIssue.CreateFromJson(JsonObject);

    Assert.AreEqual(Issue.RuleKey, 'rk1');
    Assert.AreEqual(Issue.Message, 'msg1');
    Assert.AreEqual(Issue.FilePath, 'myfile1');
    Assert.AreEqual(Issue.Range.StartLine, 5);
    Assert.AreEqual(Issue.Range.StartLineOffset, 4);
    Assert.AreEqual(Issue.Range.EndLine, 6);
    Assert.AreEqual(Issue.Range.EndLineOffset, 16);
    Assert.AreEqual(Issue.Metadata.Assignee, 'myassignee');
    Assert.AreEqual(Issue.Metadata.CreationDate, 'myDate');
    Assert.AreEqual(Issue.Metadata.Status, issReopened);
  finally
    FreeAndNil(Issue);
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

procedure TDataJsonParseTest.TestParseIssueStatus(Str: string; Value: TIssueStatus);
const
  CIssueMetadataJson: string = '{"assignee":"","creationDate":"","status":"%s"}';
var
  JsonObject: TJSONObject;
  Metadata: TIssueMetadata;
begin
  JsonObject := Parse<TJSONObject>(Format(CIssueMetadataJson, [Str]));
  try
    Metadata := TIssueMetadata.CreateFromJson(JsonObject);
    Assert.AreEqual(Value, Metadata.Status);
  finally
    FreeAndNil(Metadata);
    FreeAndNil(JsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TDataJsonParseTest.TestParseIssueStatuses;
begin
  TestParseIssueStatus('OPEN', issOpen);
  TestParseIssueStatus('CONFIRMED', issConfirmed);
  TestParseIssueStatus('REOPENED', issReopened);
  TestParseIssueStatus('RESOLVED', issResolved);
  TestParseIssueStatus('CLOSED', issClosed);
  TestParseIssueStatus('ACCEPTED', issAccepted);
  TestParseIssueStatus('TO_REVIEW', issToReview);
  TestParseIssueStatus('REVIEWED', issReviewed);
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

procedure TLiveIssueTest.TestTransfersMetadataWhenHasMetadata;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create(
    'rk1',
    'msg',
    'abc.pas',
    nil,
    TIssueMetadata.Create(
      'myassignee',
      issReopened,
      'creationdate'
    ));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abc'], True);
    Assert.IsTrue(LiveIssue.HasMetadata);
    Assert.AreEqual(LiveIssue.Assignee, 'myassignee');
    Assert.AreEqual(LiveIssue.Status, issReopened);
    Assert.AreEqual(LiveIssue.CreationDate, 'creationdate');
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestTransfersMetadataWhenNotHasMetadata;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create(
    'rk1',
    'msg',
    'abc.pas',
    nil,
    TIssueMetadata.Create(
      'myassignee',
      issReopened,
      'creationdate'
    ));
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abc'], False);
    Assert.IsFalse(LiveIssue.HasMetadata);
    Assert.AreEqual(LiveIssue.Assignee, 'myassignee');
    Assert.AreEqual(LiveIssue.Status, issReopened);
    Assert.AreEqual(LiveIssue.CreationDate, 'creationdate');
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
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

procedure TLiveIssueTest.TestWrongNumberOfAssociatedLinesRaisesRangeError;
var
  IssueData: TLintIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(4, 0, 4, 26));
  try
    Assert.WillRaise(
      procedure begin
        FreeAndNil(TLiveIssue.Create(IssueData, ['abc', 'def'], False));
      end
    );
  finally
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestIssueWithNoRangeIsSetToEntireFirstLine;
var
  IssueData: TLintIssue;
  LiveIssue: TLiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', nil);
  try
    LiveIssue := TLiveIssue.Create(IssueData, ['abc'], False);
    Assert.AreEqual(LiveIssue.StartLine, 1);
    Assert.AreEqual(LiveIssue.EndLine, 1);
    Assert.AreEqual(LiveIssue.StartLineOffset, 0);
    Assert.AreEqual(LiveIssue.EndLineOffset, 0);
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
