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
unit DelphiLint.LiveData;

interface

uses
    DelphiLint.Data
  , DelphiLint.Events
  ;

type

  ILiveIssue = interface
    ['{AC60181F-D4C3-46B2-8B02-02FDD1D511D6}']
    function RuleKey: string;
    function Message: string;
    function FilePath: string;
    function Assignee: string;
    function CreationDate: string;
    function Status: TIssueStatus;
    function HasMetadata: Boolean;
    function StartLine: Integer;
    function EndLine: Integer;
    function OriginalStartLine: Integer;
    function OriginalEndLine: Integer;
    function StartLineOffset: Integer;
    function EndLineOffset: Integer;
    function IsTethered: Boolean;
    function GetLinesMoved: Integer;
    procedure SetLinesMoved(NewStartLine: Integer);

    procedure NewLineMoveSession;
    procedure UpdateTether(LineNum: Integer; LineText: string);
    procedure Untether;

    function GetOnUntethered: TEventNotifier<Integer>;

    property LinesMoved: Integer read GetLinesMoved write SetLinesMoved;
    property OnUntethered: TEventNotifier<Integer> read GetOnUntethered;
  end;

//______________________________________________________________________________________________________________________

  TLiveIssueImpl = class(TInterfacedObject, ILiveIssue)
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
    FOnUntethered: TEventNotifier<Integer>;
    FLines: TArray<string>;
  public
    constructor Create(Issue: TLintIssue; IssueLines: TArray<string>; HasMetadata: Boolean = False);
    destructor Destroy; override;

    procedure NewLineMoveSession;
    procedure UpdateTether(LineNum: Integer; LineText: string);
    procedure Untether;

    function RuleKey: string;
    function Message: string;
    function FilePath: string;
    function Assignee: string;
    function CreationDate: string;
    function Status: TIssueStatus;
    function HasMetadata: Boolean;

    function StartLine: Integer;
    function EndLine: Integer;
    function OriginalStartLine: Integer;
    function OriginalEndLine: Integer;
    function StartLineOffset: Integer;
    function EndLineOffset: Integer;

    function GetOnUntethered: TEventNotifier<Integer>;

    function GetLinesMoved: Integer;
    procedure SetLinesMoved(NewStartLine: Integer);

    function IsTethered: Boolean;

  end;

//______________________________________________________________________________________________________________________

implementation

uses
    System.SysUtils
  ;


//______________________________________________________________________________________________________________________

constructor TLiveIssueImpl.Create(Issue: TLintIssue; IssueLines: TArray<string>; HasMetadata: Boolean = False);
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
    FEndLine := 1;
    FStartLineOffset := 0;
    FEndLineOffset := 0;
  end;

  if Length(IssueLines) <> (FEndLine - FStartLine + 1) then begin
    raise ERangeError.CreateFmt(
      'Issue spans %d lines (line %d to line %d), but %d lines were associated',
      [
        FEndLine - FStartLine + 1,
        FStartLine,
        FEndLine,
        Length(IssueLines)
      ]);
  end;

  FHasMetadata := HasMetadata;
  if Assigned(Issue.Metadata) then begin
    FAssignee := Issue.Metadata.Assignee;
    FCreationDate := Issue.Metadata.CreationDate;
    FStatus := Issue.Metadata.Status;
  end;

  FOnUntethered := TEventNotifier<Integer>.Create;

  FLinesMoved := 0;
  FLines := IssueLines;
  FTethered := True;
end;

//______________________________________________________________________________________________________________________

destructor TLiveIssueImpl.Destroy;
begin
  FreeAndNil(FOnUntethered);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueImpl.Untether;
begin
  if FTethered then begin
    FTethered := False;
    FOnUntethered.Notify(StartLine);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueImpl.UpdateTether(LineNum: Integer; LineText: string);
var
  Delta: Integer;
begin
  if not FTethered then begin
    Exit;
  end;

  Delta := LineNum - StartLine;
  if (Delta >= 0) and (Delta < Length(FLines)) then begin
    if (FLines[Delta] <> LineText) then begin
      Untether;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.Assignee: string;
begin
  Result := FAssignee;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.StartLine: Integer;
begin
  Result := FStartLine + FLinesMoved;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.StartLineOffset: Integer;
begin
  Result := FStartLineOffset;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.Status: TIssueStatus;
begin
  Result := FStatus;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.CreationDate: string;
begin
  Result := FCreationDate;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.EndLine: Integer;
begin
  Result := FEndLine + FLinesMoved;
end;

function TLiveIssueImpl.EndLineOffset: Integer;
begin
  Result := FEndLineOffset;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.FilePath: string;
begin
  Result := FFilePath;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueImpl.SetLinesMoved(NewStartLine: Integer);
begin
  FLinesMoved := NewStartLine;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.GetLinesMoved: Integer;
begin
  Result := FLinesMoved;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.GetOnUntethered: TEventNotifier<Integer>;
begin
  Result := FOnUntethered;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.HasMetadata: Boolean;
begin
  Result := FHasMetadata;
end;

//______________________________________________________________________________________________________________________

function TLiveIssueImpl.IsTethered: Boolean;
begin
  Result := FTethered;
end;

function TLiveIssueImpl.Message: string;
begin
  Result := FMessage;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueImpl.NewLineMoveSession;
begin
  FStartLine := StartLine;
  FEndLine := EndLine;
  FLinesMoved := 0;
end;

function TLiveIssueImpl.OriginalEndLine: Integer;
begin
  Result := FEndLine;
end;

function TLiveIssueImpl.OriginalStartLine: Integer;
begin
  Result := FStartLine;
end;

function TLiveIssueImpl.RuleKey: string;
begin
  Result := FRuleKey;
end;

end.
