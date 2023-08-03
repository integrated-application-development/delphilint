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
unit DelphiLint.Data;

interface

uses
    System.JSON
  ;

type

//______________________________________________________________________________________________________________________

  TRange = class(TObject)
  private
    FStartLine: Integer;
    FStartLineOffset: Integer;
    FEndLine: Integer;
    FEndLineOffset: Integer;
  public
    constructor FromJson(Json: TJsonObject);

    property StartLine: Integer read FStartLine;
    property StartLineOffset: Integer read FStartLineOffset;
    property EndLine: Integer read FEndLine;
    property EndLineOffset: Integer read FEndLineOffset;
  end;

//______________________________________________________________________________________________________________________

  TIssueStatus = (
    issOpen,
    issConfirmed,
    issReopened,
    issResolved,
    issClosed,
    issToReview,
    issReviewed
  );

  TIssueMetadata = class(TObject)
  private
    FAssignee: string;
    FCreationDate: string;
    FStatus: TIssueStatus;
  public
    constructor FromJson(Json: TJSONObject);

    property Assignee: string read FAssignee;
    property CreationDate: string read FCreationDate;
    property Status: TIssueStatus read FStatus;
  end;

//______________________________________________________________________________________________________________________

  TLintIssue = class(TObject)
  private
    FRuleKey: string;
    FMessage: string;
    FFilePath: string;
    FRange: TRange;
    FMetadata: TIssueMetadata;

  public
    property RuleKey: string read FRuleKey;
    property Message: string read FMessage;
    property FilePath: string read FFilePath;
    property Range: TRange read FRange write FRange;
    property Metadata: TIssueMetadata read FMetadata write FMetadata;

    constructor FromJson(Json: TJsonObject);
    destructor Destroy; override;
  end;

//______________________________________________________________________________________________________________________

  TRuleType = (
    rtCodeSmell,
    rtBug,
    rtVulnerability,
    rtSecurityHotspot
  );

  TRuleSeverity = (
    rsInfo,
    rsMinor,
    rsMajor,
    rsCritical,
    rsBlocker
  );

  TRule = class(TObject)
  private
    FRuleKey: string;
    FName: string;
    FDesc: string;
    FSeverity: TRuleSeverity;
    FType: TRuleType;

  public
    property RuleKey: string read FRuleKey;
    property Name: string read FName;
    property Desc: string read FDesc;
    property Severity: TRuleSeverity read FSeverity;
    property RuleType: TRuleType read FType;

    constructor FromJson(Json: TJSONObject);
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    System.StrUtils
  , System.SysUtils
  ;

//______________________________________________________________________________________________________________________

constructor TRange.FromJson(Json: TJsonObject);
begin
  FStartLine := Json.GetValue<Integer>('startLine', 0);
  FEndLine := Json.GetValue<Integer>('endLine', 0);
  FStartLineOffset := Json.GetValue<Integer>('startOffset', 0);
  FEndLineOffset := Json.GetValue<Integer>('endOffset', 0);
end;

//______________________________________________________________________________________________________________________

constructor TLintIssue.FromJson(Json: TJsonObject);
var
  RangeJson: TJsonValue;
  MetadataJson: TJsonValue;
begin
  FRuleKey := Json.GetValue<string>('ruleKey');
  FMessage := Json.GetValue<string>('message', FRuleKey);
  FFilePath := Json.GetValue<string>('file');
  FRange := nil;

  RangeJson := Json.GetValue<TJsonValue>('range', nil);
  if Assigned(RangeJson) and (RangeJson is TJsonObject) then begin
    FRange := TRange.FromJson(RangeJson as TJsonObject);
  end;

  MetadataJson := Json.GetValue<TJSONValue>('metadata', nil);
  if Assigned(MetadataJson) and (MetadataJson is TJSONObject) then begin
    FMetadata := TIssueMetadata.FromJson(MetadataJson as TJSONObject);
  end;
end;

//______________________________________________________________________________________________________________________

destructor TLintIssue.Destroy;
begin
  FreeAndNil(FRange);
  FreeAndNil(FMetadata);
  inherited;
end;

//______________________________________________________________________________________________________________________

constructor TRule.FromJson(Json: TJSONObject);
const
  C_Severities: array of string = ['INFO', 'MINOR', 'MAJOR', 'CRITICAL', 'BLOCKER'];
  C_RuleTypes: array of string = ['CODE_SMELL', 'BUG', 'VULNERABILITY', 'SECURITY_HOTSPOT'];
begin
  FRuleKey := Json.GetValue<string>('key');
  FName := Json.GetValue<string>('name');
  FDesc := Json.GetValue<string>('desc');
  FSeverity := TRuleSeverity(IndexStr(Json.GetValue<string>('severity'), C_Severities));
  FType := TRuleType(IndexStr(Json.GetValue<string>('type'), C_RuleTypes));
end;

//______________________________________________________________________________________________________________________

constructor TIssueMetadata.FromJson(Json: TJSONObject);
const
  C_Statuses: array of string = ['OPEN', 'CONFIRMED', 'REOPENED', 'RESOLVED', 'CLOSED', 'TO_REVIEW', 'REVIEWED'];
begin
  inherited;
  FAssignee := Json.GetValue<string>('assignee');
  FCreationDate := Json.GetValue<string>('creationDate');
  FStatus := TIssueStatus(IndexStr(Json.GetValue<string>('status'), C_Statuses));
end;

end.
