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
unit DelphiLint.IssueActions;

interface

uses
    Vcl.Menus
  , System.Classes
  , System.Generics.Collections
  , DelphiLint.LiveData
  ;

type

//______________________________________________________________________________________________________________________

  TIssueMenuItemFactory = class(TObject)
  private
    FIssue: ILiveIssue;

  public
    constructor Create(Issue: ILiveIssue);
    destructor Destroy; override;

    function HideIssue(Owner: TComponent): TMenuItem;
    function ApplyQuickFix(Owner: TComponent): TMenuItem;
  end;

//______________________________________________________________________________________________________________________

  TIssueLinkedMenuItem = class abstract(TMenuItem)
  private
    FIssue: ILiveIssue;
  protected
    procedure DoOnClick(Sender: TObject); virtual; abstract;
  public
    constructor CreateLinked(AOwner: TComponent; Issue: ILiveIssue);

    property LinkedIssue: ILiveIssue read FIssue;
  end;

//______________________________________________________________________________________________________________________

  TUntetherMenuItem = class(TIssueLinkedMenuItem)
  protected
    procedure DoOnClick(Sender: TObject); override;
  end;

//______________________________________________________________________________________________________________________

  TQuickFixMenuItem = class(TIssueLinkedMenuItem)
  private
    FIndex: Integer;

    function GetQuickFix: TLiveQuickFix;
  protected
    procedure DoOnClick(Sender: TObject); override;
  public
    constructor CreateLinked(AOwner: TComponent; Issue: ILiveIssue; Index: Integer);
    property QuickFix: TLiveQuickFix read GetQuickFix;
  end;

//______________________________________________________________________________________________________________________

procedure ReflowQuickFixEdits(TextEdits: TList<TLiveTextEdit>; EditIndex: Integer);
procedure CalculateQuickFixReflowMetrics(
  TextEdit: TLiveTextEdit;
  out AddedLines: Integer;
  out AddedColumns: Integer
);

//______________________________________________________________________________________________________________________

implementation

uses
    System.SysUtils
  , DelphiLint.Context
  ;

//______________________________________________________________________________________________________________________

constructor TIssueMenuItemFactory.Create(Issue: ILiveIssue);
begin
  inherited Create;
  FIssue := Issue;
end;

//______________________________________________________________________________________________________________________

destructor TIssueMenuItemFactory.Destroy;
begin
  FIssue := nil;
  inherited;
end;

//______________________________________________________________________________________________________________________

function TIssueMenuItemFactory.HideIssue(Owner: TComponent): TMenuItem;
begin
  Result := TUntetherMenuItem.CreateLinked(Owner, FIssue);
  Result.Name := 'HideIssue';
  Result.Caption := 'Hide Issue';
  Result.Enabled := FIssue.IsTethered;
end;

//______________________________________________________________________________________________________________________

function TIssueMenuItemFactory.ApplyQuickFix(Owner: TComponent): TMenuItem;

  function QuickFixItem(Index: Integer; IndexInName: Boolean): TQuickFixMenuItem;
  begin
    Result := TQuickFixMenuItem.CreateLinked(Owner, FIssue, Index);
    Result.Name := Format('ApplyQuickFix_%d', [Index]);
    Result.Enabled := Result.QuickFix.Tethered;

    if IndexInName then begin
      Result.Caption := Format('&%d: %s', [Index + 1, Result.QuickFix.Message]);
    end
    else begin
      Result.Caption := Format('&Quick Fix: %s', [Result.QuickFix.Message]);
    end;
  end;

var
  I: Integer;
  FixCount: Integer;
begin
  FixCount := FIssue.QuickFixes.Count;

  if FixCount = 0 then begin
    Result := nil;
  end
  else if FixCount = 1 then begin
    Result := QuickFixItem(0, False);
  end
  else begin
    Result := TMenuItem.Create(Owner);
    Result.Name := 'DelphiLintApplyQuickFixGroup';
    Result.Caption := Format('&Quick Fix (%d available)', [FixCount]);

    for I := 0 to FixCount - 1 do begin
      Result.Add(QuickFixItem(I, True));
    end;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TIssueLinkedMenuItem.CreateLinked(AOwner: TComponent; Issue: ILiveIssue);
begin
  inherited Create(AOwner);

  FIssue := Issue;
  OnClick := DoOnClick;
end;

//______________________________________________________________________________________________________________________

procedure TUntetherMenuItem.DoOnClick(Sender: TObject);
begin
  LinkedIssue.Untether;
end;

//______________________________________________________________________________________________________________________

constructor TQuickFixMenuItem.CreateLinked(AOwner: TComponent; Issue: ILiveIssue; Index: Integer);
begin
  inherited CreateLinked(AOwner, Issue);
  FIndex := Index;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixMenuItem.DoOnClick(Sender: TObject);
var
  QuickFix: TLiveQuickFix;
  TextEdit: TLiveTextEdit;
  View: IIDEEditView;
  I: Integer;
begin
  if FIssue.QuickFixes.Count < FIndex then begin
    Log.Warn('Quick fix menu item wanted nonexistent quick fix at index %d', [FIndex]);
    Exit;
  end;

  QuickFix := FIssue.QuickFixes[FIndex];
  View := LintContext.IDEServices.GetTopView;

  if not QuickFix.Tethered then begin
    Log.Warn('Attempted to apply an untethered quick fix');
    Exit;
  end
  else if not Assigned(View) then begin
    Log.Warn('No top view found when applying quick fix');
    Exit;
  end;

  for I := 0 to QuickFix.TextEdits.Count - 1 do begin
    TextEdit := QuickFix.TextEdits[I];

    View.ReplaceText(
      TextEdit.Replacement,
      QuickFix.Issue.StartLine + TextEdit.RelativeStartLine,
      TextEdit.StartLineOffset,
      QuickFix.Issue.StartLine + TextEdit.RelativeEndLine,
      TextEdit.EndLineOffset
    );

    ReflowQuickFixEdits(QuickFix.TextEdits, I);
  end;

  QuickFix.Untether;
  View.Paint;
end;

//______________________________________________________________________________________________________________________

function TQuickFixMenuItem.GetQuickFix: TLiveQuickFix;
begin
  Result := nil;
  if (FIndex >= 0) and (FIndex < FIssue.QuickFixes.Count) then begin
    Result := FIssue.QuickFixes[FIndex];
  end;
end;

//______________________________________________________________________________________________________________________

procedure CalculateQuickFixReflowMetrics(
  TextEdit: TLiveTextEdit;
  out AddedLines: Integer;
  out AddedColumns: Integer
);
var
  OldEndOffset: Integer;
  NewEndOffset: Integer;
  OldLineHeight: Integer;
  NewLineHeight: Integer;
  ReplacementLines: TStringList;
begin
  // Determines the delta from the original range end to the new range end after the replacement is applied.
  // This allows us to apply the delta to future text edits so they continue to work as expected.

  // Line height is always at least 1 - a zero-length range is modelled as height 1, width 0
  OldLineHeight := 1 + TextEdit.RelativeEndLine - TextEdit.RelativeStartLine;
  OldEndOffset := TextEdit.EndLineOffset;

  ReplacementLines := TStringList.Create;
  try
    ReplacementLines.Text := TextEdit.Replacement;
    NewLineHeight := ReplacementLines.Count;

    if NewLineHeight = 0 then begin
      // If this edit is just a deletion, then range end is the original range start
      NewEndOffset := TextEdit.StartLineOffset;
      NewLineHeight := 1;
    end
    else begin
      // Otherwise, the range end offset is the length from the left margin to the last replacement line
      NewEndOffset := Length(ReplacementLines[ReplacementLines.Count - 1]);

      if NewLineHeight = 1 then begin
        // If there was no \n in the replacement, the length from the left margin must also include the start col
        NewEndOffset := TextEdit.StartLineOffset + NewEndOffset;
      end;
    end;

    AddedColumns := NewEndOffset - OldEndOffset;
    AddedLines := NewLineHeight - OldLineHeight;
  finally
    FreeAndNil(ReplacementLines);
  end;
end;

//______________________________________________________________________________________________________________________

procedure ReflowQuickFixEdits(TextEdits: TList<TLiveTextEdit>; EditIndex: Integer);
var
  AppliedLine: Integer;
  AppliedLineOffset: Integer;
  LinesAdded: Integer;
  ColumnsAdded: Integer;

  procedure OffsetPosition(var TargetLine: Integer; var TargetOffset: Integer);
  var
    LineAfter: Boolean;
    PositionAfter: Boolean;
  begin
    LineAfter := TargetLine > AppliedLine;
    PositionAfter := LineAfter or ((TargetLine = AppliedLine) and (TargetOffset >= AppliedLineOffset));

    if PositionAfter then begin
      if not LineAfter then begin
        // This pos starts after the applied edit on the same line, offset column
        TargetOffset := TargetOffset + ColumnsAdded;
      end;

      // This pos starts after the applied edit, offset line
      TargetLine := TargetLine + LinesAdded;
    end;
  end;

var
  AppliedEdit: TLiveTextEdit;
  CurrentEdit: TLiveTextEdit;
  I: Integer;
  StartLine: Integer;
  StartLineOffset: Integer;
  EndLine: Integer;
  EndLineOffset: Integer;
begin
  AppliedEdit := TextEdits[EditIndex];
  AppliedLine := AppliedEdit.RelativeEndLine;
  AppliedLineOffset := AppliedEdit.EndLineOffset;
  CalculateQuickFixReflowMetrics(AppliedEdit, LinesAdded, ColumnsAdded);

  if (LinesAdded = 0) and (ColumnsAdded = 0) then begin
    // Replacement is the same size as the original string - no action needed
    Exit;
  end;

  for I := EditIndex + 1 to TextEdits.Count - 1 do begin
    CurrentEdit := TextEdits[I];

    if CurrentEdit.RelativeEndLine < AppliedLine then begin
      // Higher edits don't need to be offset
      Continue;
    end;

    StartLine := CurrentEdit.RelativeStartLine;
    StartLineOffset := CurrentEdit.StartLineOffset;
    EndLine := CurrentEdit.RelativeEndLine;
    EndLineOffset := CurrentEdit.EndLineOffset;

    OffsetPosition(StartLine, StartLineOffset);
    OffsetPosition(EndLine, EndLineOffset);

    CurrentEdit.RelativeStartLine := StartLine;
    CurrentEdit.StartLineOffset := StartLineOffset;
    CurrentEdit.RelativeEndLine := EndLine;
    CurrentEdit.EndLineOffset := EndLineOffset;
  end;
end;

end.
