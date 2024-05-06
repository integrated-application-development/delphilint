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

    procedure CalculateReplacementMetrics(
      TextEdit: TLiveTextEdit;
      out AddedLines: Integer;
      out AddedColumns: Integer
    );
    procedure OffsetTextEdits(QuickFix: TLiveQuickFix; EditIndex: Integer);
    function GetQuickFix: TLiveQuickFix;
  protected
    procedure DoOnClick(Sender: TObject); override;
  public
    constructor CreateLinked(AOwner: TComponent; Issue: ILiveIssue; Index: Integer);
    property QuickFix: TLiveQuickFix read GetQuickFix;
  end;

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

    OffsetTextEdits(QuickFix, I);
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

procedure TQuickFixMenuItem.CalculateReplacementMetrics(
  TextEdit: TLiveTextEdit;
  out AddedLines: Integer;
  out AddedColumns: Integer
);
var
  RangeLength: Integer;
  RangeHeight: Integer;
  ReplacementLastLineLength: Integer;
  Lines: TStringList;
begin
  if TextEdit.RelativeStartLine = TextEdit.RelativeEndLine then begin
    RangeHeight := 1;
    RangeLength := TextEdit.EndLineOffset - TextEdit.StartLineOffset;
  end
  else begin
    RangeHeight := TextEdit.RelativeEndLine - TextEdit.RelativeStartLine;
    RangeLength := TextEdit.EndLineOffset;
  end;

  Lines := TStringList.Create;
  try
    Lines.Text := TextEdit.Replacement;

    if Lines.Count <> 0 then begin
      ReplacementLastLineLength := Length(Lines[Lines.Count - 1]);
      AddedColumns := ReplacementLastLineLength - RangeLength;
      AddedLines := Lines.Count - RangeHeight;
    end
    else begin
      AddedColumns := -RangeLength;
      AddedLines := 0;
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixMenuItem.OffsetTextEdits(QuickFix: TLiveQuickFix; EditIndex: Integer);
var
  AppliedEdit: TLiveTextEdit;
  I: Integer;
  LinesAdded: Integer;
  ColumnsAdded: Integer;
  SuccessorEdit: TLiveTextEdit;
begin
  AppliedEdit := QuickFix.TextEdits[EditIndex];
  CalculateReplacementMetrics(AppliedEdit, LinesAdded, ColumnsAdded);

  for I := EditIndex + 1 to QuickFix.TextEdits.Count - 1 do begin
    SuccessorEdit := QuickFix.TextEdits[I];

    if SuccessorEdit.RelativeStartLine >= AppliedEdit.RelativeEndLine then begin
      if (SuccessorEdit.RelativeStartLine = AppliedEdit.RelativeEndLine)
        and(SuccessorEdit.StartLineOffset > AppliedEdit.EndLineOffset)
      then begin
        SuccessorEdit.StartLineOffset := SuccessorEdit.StartLineOffset + ColumnsAdded;
      end;

      if (SuccessorEdit.RelativeEndLine = AppliedEdit.RelativeEndLine)
        and (SuccessorEdit.EndLineOffset > AppliedEdit.EndLineOffset)
      then begin
        SuccessorEdit.EndLineOffset := SuccessorEdit.EndLineOffset + ColumnsAdded;
      end;

      SuccessorEdit.RelativeStartLine := SuccessorEdit.RelativeStartLine + LinesAdded;
      SuccessorEdit.RelativeEndLine := SuccessorEdit.RelativeEndLine + LinesAdded;
    end;
  end;
end;

end.
