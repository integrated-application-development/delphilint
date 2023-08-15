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
  , System.Generics.Collections
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
    constructor CreateFromJson(Json: TJSONObject);

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
    constructor CreateFromJson(Json: TJSONObject);

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
    constructor CreateFromJson(Json: TJSONObject);
    destructor Destroy; override;

    property RuleKey: string read FRuleKey;
    property Message: string read FMessage;
    property FilePath: string read FFilePath;
    property Range: TRange read FRange write FRange;
    property Metadata: TIssueMetadata read FMetadata write FMetadata;
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
    constructor CreateFromJson(Json: TJSONObject);

    property RuleKey: string read FRuleKey;
    property Name: string read FName;
    property Desc: string read FDesc;
    property Severity: TRuleSeverity read FSeverity;
    property RuleType: TRuleType read FType;
  end;

//______________________________________________________________________________________________________________________


  TLiveIssue = class(TObject)
  private
    FRuleKey: string;
    FMessage: string;
    FFilePath: string;
    FAssignee: string;
    FCreationDate: string;
    FStatus: TIssueStatus;
    FHasMetadata: Boolean;
    FStartLine: Integer;
    FEndLine: Integer;
    FStartLineOffset: Integer;
    FEndLineOffset: Integer;
    FLinesMoved: Integer;
    FTethered: Boolean;
    FLines: TList<string>;

    function GetStartLine: Integer;
    function GetEndLine: Integer;
    procedure PopulateLines(const FileLines: TArray<string>);
  public
    constructor Create(Issue: TLintIssue; FileLines: TArray<string>; HasMetadata: Boolean = False);
    destructor Destroy; override;

    procedure NewLineMoveSession;
    procedure UpdateTether(LineNum: Integer; LineText: string);
    procedure Untether;

    property RuleKey: string read FRuleKey;
    property Message: string read FMessage;
    property FilePath: string read FFilePath write FFilePath;
    property Assignee: string read FAssignee;
    property CreationDate: string read FCreationDate;
    property Status: TIssueStatus read FStatus;
    property HasMetadata: Boolean read FHasMetadata;

    property OriginalStartLine: Integer read FStartLine;
    property OriginalEndLine: Integer read FEndLine;
    property StartLine: Integer read GetStartLine;
    property EndLine: Integer read GetEndLine;
    property StartLineOffset: Integer read FStartLineOffset;
    property EndLineOffset: Integer read FEndLineOffset;
    property LinesMoved: Integer read FLinesMoved write FLinesMoved;
    property Tethered: Boolean read FTethered;
  end;

//______________________________________________________________________________________________________________________

  TFileAnalysisHistory = record
    AnalysisTime: TDateTime;
    Success: Boolean;
    IssuesFound: Integer;
    FileHash: string;
  end;

//______________________________________________________________________________________________________________________

  TCurrentAnalysis = class(TObject)
  private
    FPaths: TArray<string>;
  public
    constructor Create(Paths: TArray<string>);
    function IncludesFile(const Path: string): Boolean;

    property Paths: TArray<string> read FPaths;
  end;

//______________________________________________________________________________________________________________________

  TFileAnalysisStatus = (
    fasNeverAnalyzed,
    fasOutdatedAnalysis,
    fasUpToDateAnalysis
  );

//______________________________________________________________________________________________________________________

implementation

uses
    System.StrUtils
  , System.SysUtils
  , DelphiLint.Utils
  ;

//______________________________________________________________________________________________________________________

constructor TRange.CreateFromJson(Json: TJSONObject);
begin
  inherited Create;
  FStartLine := Json.GetValue<Integer>('startLine', 0);
  FEndLine := Json.GetValue<Integer>('endLine', 0);
  FStartLineOffset := Json.GetValue<Integer>('startOffset', 0);
  FEndLineOffset := Json.GetValue<Integer>('endOffset', 0);
end;

//______________________________________________________________________________________________________________________

constructor TLintIssue.CreateFromJson(Json: TJSONObject);
var
  RangeJson: TJSONValue;
  MetadataJson: TJSONValue;
begin
  inherited Create;
  FRuleKey := Json.GetValue<string>('ruleKey');
  FMessage := Json.GetValue<string>('message', FRuleKey);
  FFilePath := Json.GetValue<string>('file');
  FRange := nil;

  RangeJson := Json.GetValue<TJSONValue>('range', nil);
  if Assigned(RangeJson) and (RangeJson is TJSONObject) then begin
    FRange := TRange.CreateFromJson(RangeJson as TJSONObject);
  end;

  MetadataJson := Json.GetValue<TJSONValue>('metadata', nil);
  if Assigned(MetadataJson) and (MetadataJson is TJSONObject) then begin
    FMetadata := TIssueMetadata.CreateFromJson(MetadataJson as TJSONObject);
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

constructor TRule.CreateFromJson(Json: TJSONObject);
const
  C_Severities: array of string = ['INFO', 'MINOR', 'MAJOR', 'CRITICAL', 'BLOCKER'];
  C_RuleTypes: array of string = ['CODE_SMELL', 'BUG', 'VULNERABILITY', 'SECURITY_HOTSPOT'];
begin
  inherited;

  FRuleKey := Json.GetValue<string>('key');
  FName := Json.GetValue<string>('name');
  FDesc := Json.GetValue<string>('desc');
  FSeverity := TRuleSeverity(IndexStr(Json.GetValue<string>('severity'), C_Severities));
  FType := TRuleType(IndexStr(Json.GetValue<string>('type'), C_RuleTypes));
end;

//______________________________________________________________________________________________________________________

constructor TIssueMetadata.CreateFromJson(Json: TJSONObject);
const
  C_Statuses: array of string = ['OPEN', 'CONFIRMED', 'REOPENED', 'RESOLVED', 'CLOSED', 'TO_REVIEW', 'REVIEWED'];
begin
  inherited;

  FAssignee := Json.GetValue<string>('assignee');
  FCreationDate := Json.GetValue<string>('creationDate');
  FStatus := TIssueStatus(IndexStr(Json.GetValue<string>('status'), C_Statuses));
end;

//______________________________________________________________________________________________________________________

constructor TLiveIssue.Create(Issue: TLintIssue; FileLines: TArray<string>; HasMetadata: Boolean = False);
begin
  inherited Create;

  FRuleKey := Issue.RuleKey;
  FMessage := Issue.Message;
  FFilePath := Issue.FilePath;

  if Assigned(Issue.Range) then begin
    FStartLine := Issue.Range.StartLine;
    FEndLine := Issue.Range.EndLine;
    FStartLineOffset := Issue.Range.StartLineOffset;
    FEndLineOffset := Issue.Range.EndLineOffset;
  end
  else begin
    FStartLine := 1;
    FEndLine := 2;
    FStartLineOffset := 0;
    FEndLineOffset := 0;
  end;

  FHasMetadata := HasMetadata;
  if Assigned(Issue.Metadata) then begin
    FAssignee := Issue.Metadata.Assignee;
    FCreationDate := Issue.Metadata.CreationDate;
    FStatus := Issue.Metadata.Status;
  end;

  FLinesMoved := 0;
  FLines := TList<string>.Create;
  FTethered := True;

  PopulateLines(FileLines);
end;

//______________________________________________________________________________________________________________________

destructor TLiveIssue.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.PopulateLines(const FileLines: TArray<string>);
var
  LineNum: Integer;
begin
  if FileExists(FFilePath) and (Length(FileLines) > StartLine) then begin
    LineNum := StartLine;

    while (LineNum <= EndLine) and (LineNum < Length(FileLines)) do begin
      FLines.Add(FileLines[LineNum - 1]);
      Inc(LineNum);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.Untether;
begin
  FTethered := False;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.UpdateTether(LineNum: Integer; LineText: string);
var
  Delta: Integer;
begin
  if not Tethered then begin
    Exit;
  end;

  Delta := LineNum - StartLine;
  if (Delta >= 0) and (Delta < FLines.Count) then begin
    if (FLines[Delta] <> LineText) then begin
      Untether;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLiveIssue.GetStartLine: Integer;
begin
  Result := FStartLine + LinesMoved;
end;

//______________________________________________________________________________________________________________________

function TLiveIssue.GetEndLine: Integer;
begin
  Result := FEndLine + LinesMoved;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.NewLineMoveSession;
begin
  FStartLine := StartLine;
  FEndLine := EndLine;
  FLinesMoved := 0;
end;

//______________________________________________________________________________________________________________________

constructor TCurrentAnalysis.Create(Paths: TArray<string>);
begin
  inherited Create;
  FPaths := Paths;
end;

//______________________________________________________________________________________________________________________

function TCurrentAnalysis.IncludesFile(const Path: string): Boolean;
var
  PathEl: string;
begin
  Result := False;

  for PathEl in FPaths do begin
    if NormalizePath(PathEl) = NormalizePath(Path) then begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
