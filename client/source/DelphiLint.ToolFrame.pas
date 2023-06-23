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
unit DelphiLint.ToolFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus,
  Vcl.ToolWin, System.RegularExpressions, DelphiLint.Data, DelphiLint.Context;

type
  THtmlRemover = class(TObject)
  private
    FBrRegex: TRegEx;
    FTagRegex: TRegEx;
    FConsecutiveSpaceRegex: TRegEx;
    FConsecutiveNewlineRegex: TRegEx;
  public
    constructor Create;
    function Process(Text: string): string;
  end;

  TCurrentFileStatus = (
    cfsNotAnalyzable,
    cfsNotAnalyzed,
    cfsInAnalysis,
    cfsFailed,
    cfsNoIssues,
    cfsNoIssuesOutdated,
    cfsIssues,
    cfsIssuesOutdated
  );

  TLintToolFrame = class(TFrame)
    FileHeadingPanel: TPanel;
    FileStatusLabel: TLabel;
    ProgBar: TProgressBar;
    FileNameLabel: TLabel;
    ProgImage: TImage;
    IssueListBox: TListBox;
    LintButtonPanel: TPanel;
    RulePanel: TPanel;
    RuleNameLabel: TLabel;
    RuleTypeLabel: TLabel;
    RuleDescLabel: TLabel;
    ContentPanel: TPanel;
    SplitPanel: TPanel;
    RuleHeading: TPanel;
    TopPanel: TPanel;
    LintToolBar: TToolBar;
    AnalyzeShortButton: TToolButton;
    AnalyzePopupMenu: TPopupMenu;
    AnalyzeCurrentFile1: TMenuItem;
    AnalyzeOpenFiles1: TMenuItem;
    StatusPanel: TPanel;
    ProgLabel: TLabel;
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure OnIssueSelected(Sender: TObject);
    procedure OnIssueDoubleClicked(Sender: TObject);
    procedure OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnMeasureIssueItem(Control: TWinControl; Index: Integer; var Height: Integer);
  private const
    C_RuleSeverityStrs: array[TRuleSeverity] of string = (
      'Info',
      'Minor',
      'Major',
      'Critical',
      'Blocker'
    );
    C_RuleTypeStrs: array[TRuleType] of string = (
      'Code smell',
      'Bug',
      'Vulnerability',
      'Security hotspot'
    );
  private
    FResizing: Boolean;
    FDragStartX: Integer;
    FCurrentPath: string;
    FHtmlRemover: THtmlRemover;
    FIssues: TArray<TLiveIssue>;

    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    procedure RepaintIssueView;
    procedure GetIssueItemText(ListBox: TListBox; Issue: TLiveIssue; out LocationText: string; out MessageText: string);


    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);

    procedure RefreshRuleView;
    procedure SetRuleView(Name: string; RuleKey: string; RuleType: TRuleType; Severity: TRuleSeverity; Desc: string);

    function GetStatusImage(Status: TCurrentFileStatus): Integer;
    function GetStatusCaption(Status: TCurrentFileStatus; NumIssues: Integer): string;
    procedure UpdateFileStatus(Status: TCurrentFileStatus; NumIssues: Integer = -1);
    procedure UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean = False);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure ChangeActiveFile(const Path: string);
    procedure RefreshActiveFile;
  end;

implementation

uses
    ToolsAPI
  , DelphiLint.Utils
  , System.StrUtils
  , System.IOUtils
  , DelphiLint.Logger
  , DelphiLint.Plugin
  ;

{$R *.dfm}

constructor TLintToolFrame.Create(Owner: TComponent);
var
  Editor: IOTASourceEditor;
begin
  inherited Create(Owner);
  FResizing := False;

  FHtmlRemover := THtmlRemover.Create;
  FCurrentPath := '';

  LintContext.OnAnalysisStarted.AddListener(OnAnalysisStarted);

  LintContext.OnAnalysisComplete.AddListener(
    procedure(const Paths: TArray<string>) begin
      OnAnalysisFinished(Paths, True);
    end);

  LintContext.OnAnalysisFailed.AddListener(
    procedure(const Paths: TArray<string>) begin
      OnAnalysisFinished(Paths, False);
    end);

  if TryGetCurrentSourceEditor(Editor) then begin
    ChangeActiveFile(Editor.FileName);
  end
  else begin
    ChangeActiveFile('');
  end;

  if LintContext.InAnalysis then begin
    OnAnalysisStarted(LintContext.CurrentAnalysis.Paths);
  end
  else begin
    UpdateAnalysisStatus('Idle');
  end;

  Plugin.RegisterToolFrame(Self);
end;

//______________________________________________________________________________________________________________________

destructor TLintToolFrame.Destroy;
begin
  FreeAndNil(FHtmlRemover);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.FrameResize(Sender: TObject);
begin
  RepaintIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean);
begin
  if ShowProgress then begin
    ProgBar.Style := TProgressBarStyle.pbstMarquee;
  end
  else begin
    ProgBar.Style := TProgressBarStyle.pbstNormal;
    ProgBar.Position := 0;
  end;
  ProgLabel.Caption := Msg;
  Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnAnalysisStarted(const Paths: TArray<string>);
var
  SourceFile: string;
begin
  if Length(Paths) = 2 then begin
    SourceFile := Paths[0];
    if IsDelphiSource(Paths[1]) then begin
      SourceFile := Paths[1];
    end;

    UpdateAnalysisStatus(Format('Analyzing %s...', [TPath.GetFileName(SourceFile)]), True);
  end
  else begin
    for SourceFile in Paths do begin
      if IsDelphiSource(SourceFile) then begin
        Break;
      end;
    end;

    UpdateAnalysisStatus(
      Format(
        'Analyzing %s + %d more...',
        [TPath.GetFileName(SourceFile), Length(Paths) - 2]),
      True);
  end;

  RefreshActiveFile;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);
begin
    UpdateAnalysisStatus(
      Format(
        'Idle (last analysis %s at %s)',
        [IfThen(Succeeded, 'succeeded', 'failed'), FormatDateTime('h:nnam/pm', Now)]));
    RefreshActiveFile;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FResizing := True;
  FDragStartX := X;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  // TODO: Implement visual feedback
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewWidth: Integer;
begin
  FResizing := False;
  NewWidth := RulePanel.Width - (X - FDragStartX);

  if (NewWidth < ContentPanel.Width - 10) then begin
    RulePanel.Width := NewWidth;
  end;

  RepaintIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.UpdateFileNameLabel(NewText: string = '');
begin
  if NewText = '' then begin
    FileNameLabel.Caption := TPath.GetFileName(FCurrentPath);
  end
  else begin
    FileNameLabel.Caption := NewText;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.ChangeActiveFile(const Path: string);
var
  History: TFileAnalysisHistory;
  FileScannable: Boolean;
begin
  FileScannable := IsFileInProject(Path);
  FCurrentPath := IfThen(FileScannable, Path, '');

  if FileScannable then begin
    if LintContext.InAnalysis and LintContext.CurrentAnalysis.IncludesFile(Path) then begin
      UpdateFileStatus(cfsInAnalysis);
      Exit;
    end;

    case LintContext.GetAnalysisStatus(Path) of
      fasNeverAnalyzed:
          UpdateFileStatus(cfsNotAnalyzed);
      fasOutdatedAnalysis:
        if LintContext.TryGetAnalysisHistory(Path, History) then begin
          if History.Success then begin
            if History.IssuesFound = 0 then begin
              UpdateFileStatus(cfsNoIssuesOutdated);
            end
            else begin
              UpdateFileStatus(cfsIssuesOutdated, History.IssuesFound);
            end;
          end
          else begin
            UpdateFileStatus(cfsNotAnalyzed);
          end;
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently outdated analysis.');
          UpdateFileStatus(cfsNotAnalyzed);
        end;
      fasUpToDateAnalysis:
        if LintContext.TryGetAnalysisHistory(Path, History) then begin
          if History.Success then begin
            if History.IssuesFound = 0 then begin
              UpdateFileStatus(cfsNoIssues);
            end
            else begin
              UpdateFileStatus(cfsIssues, History.IssuesFound);
            end;
          end
          else begin
            UpdateFileStatus(cfsFailed);
          end;
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently up-to-date analysis.');
          UpdateFileStatus(cfsNotAnalyzed);
        end;
    end;
  end
  else begin
    UpdateFileStatus(cfsNotAnalyzable);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.UpdateFileStatus(Status: TCurrentFileStatus; NumIssues: Integer = -1);
begin
  if Status = TCurrentFileStatus.cfsNotAnalyzable then begin
    UpdateFileNameLabel('Non-project file');
  end
  else begin
    UpdateFileNameLabel;
  end;

  Plugin.LintImages.GetIcon(GetStatusImage(Status), ProgImage.Picture.Icon);
  FileStatusLabel.Caption := GetStatusCaption(Status, NumIssues);
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

function TLintToolFrame.GetStatusImage(Status: TCurrentFileStatus): Integer;
begin
  case Status of
    cfsNotAnalyzable, cfsNotAnalyzed: Result := C_ImgDefault;
    cfsInAnalysis: Result := C_ImgWorking;
    cfsFailed: Result := C_ImgError;
    cfsNoIssues: Result := C_ImgSuccess;
    cfsNoIssuesOutdated: Result := C_ImgSuccessWarn;
    cfsIssues: Result := C_ImgIssues;
    cfsIssuesOutdated: Result := C_ImgIssuesWarn;
  else
    Result := C_ImgDefault;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintToolFrame.GetStatusCaption(Status: TCurrentFileStatus; NumIssues: Integer): string;
begin
  case Status of
    cfsNotAnalyzable: Result := 'Not analyzable';
    cfsNotAnalyzed: Result := 'Not analyzed';
    cfsInAnalysis: Result := 'Analyzing';
    cfsFailed: Result := 'Failed';
    cfsNoIssues: Result := 'No issues';
    cfsNoIssuesOutdated: Result := 'No issues (outdated)';
    cfsIssues: Result := Format('%d issues', [NumIssues]);
    cfsIssuesOutdated: Result := Format('%d issues (outdated)', [NumIssues]);
  else
    Result := 'Not analyzable';
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.GetIssueItemText(ListBox: TListBox; Issue: TLiveIssue; out LocationText, MessageText: string);
begin
  if Issue.Tethered then begin
    LocationText := Format('(%d, %d) ', [Issue.StartLine, Issue.StartLineOffset]);
  end
  else begin
    LocationText := '(removed) ';
  end;

  MessageText := Issue.Message;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnMeasureIssueItem(Control: TWinControl; Index: Integer; var Height: Integer);
var
  ListBox: TListBox;
  LocationText: string;
  MessageText: string;
  Issue: TLiveIssue;
  Rect: TRect;
begin
  ListBox := Control as TListBox;
  // For some reason, OnMeasureItem is called after a string is added to the listbox, but before its corresponding
  // object is added. This means we have to maintain a separate list to be able to retrieve them by index here.
  Issue := FIssues[Index];

  GetIssueItemText(ListBox, Issue, LocationText, MessageText);

  Rect := TRect.Empty;
  Rect.Left := Rect.Left + ListBox.Canvas.TextWidth(LocationText) + 4;
  Rect.Right := ListBox.ClientRect.Right - 4;
  Rect.Top := Rect.Top + 4;
  Rect.Height := 0;

  DrawText(
    ListBox.Canvas.Handle,
    PChar(MessageText),
    Length(MessageText),
    Rect,
    DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

  Height := Rect.Height + 8;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  Issue: TLiveIssue;
  ListBox: TListBox;
  LocationText: string;
  MessageText: string;
  LocationWidth: Integer;
  MessageRect: TRect;
begin
  ListBox := Control as TListBox;

  if (Index < 0) or (Index >= Length(FIssues)) then begin
    Exit;
  end;

  Issue := FIssues[Index];
  GetIssueItemText(ListBox, Issue, LocationText, MessageText);

  Canvas := ListBox.Canvas;
  Canvas.FillRect(Rect);

  if not Issue.Tethered then begin
    Canvas.Font.Color := clGrayText;
  end;

  LocationWidth := Canvas.TextWidth(LocationText);
  Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, LocationText);

  Canvas.Font.Style := [fsBold];

  MessageRect := TRect.Empty;
  MessageRect.Left := Rect.Left + LocationWidth + 4;
  MessageRect.Right := Rect.Right - 4;
  MessageRect.Top := Rect.Top + 4;
  MessageRect.Bottom := Rect.Bottom - 4;

  DrawText(
    ListBox.Canvas.Handle,
    PChar(MessageText),
    Length(MessageText),
    MessageRect,
    DT_LEFT or DT_WORDBREAK);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnIssueDoubleClicked(Sender: TObject);
var
  SelectedIndex: Integer;
  SelectedIssue: TLiveIssue;
  Editor: IOTASourceEditor;
  Buffer: IOTAEditBuffer;
begin
  SelectedIndex := IssueListBox.ItemIndex;

  // No item selected
  if SelectedIndex = -1 then begin
    Exit;
  end;

  SelectedIssue := TLiveIssue(IssueListBox.Items.Objects[SelectedIndex]);

  // Issue line has been removed
  if SelectedIssue.StartLine = -1 then begin
    Exit;
  end;

  if TryGetCurrentSourceEditor(Editor) and (Editor.EditViewCount <> 0) then begin
    Buffer := Editor.EditViews[0].Buffer;
    Buffer.EditPosition.GotoLine(SelectedIssue.StartLine);
    Buffer.EditPosition.Move(0, SelectedIssue.StartLineOffset);
    Buffer.TopView.Paint;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnIssueSelected(Sender: TObject);
begin
  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshActiveFile;
begin
  ChangeActiveFile(FCurrentPath);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshIssueView;
begin
  if FCurrentPath <> '' then begin
    FIssues := LintContext.GetIssues(FCurrentPath);
  end;

  RepaintIssueView;
  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RepaintIssueView;
var
  Issue: TLiveIssue;
  Selected: Integer;
begin
  Selected := IssueListBox.ItemIndex;
  IssueListBox.ClearSelection;
  IssueListBox.Clear;

  for Issue in FIssues do begin
    IssueListBox.AddItem(Format('%d: %s', [Issue.StartLine, Issue.Message]), Issue);
  end;

  if IssueListBox.Items.Count > Selected then begin
    IssueListBox.ItemIndex := Selected;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshRuleView;
var
  SelectedIndex: Integer;
  SelectedIssue: TLiveIssue;
  Rule: TRule;
  WasVisible: Boolean;
begin
  WasVisible := RulePanel.Visible;

  SelectedIndex := IssueListBox.ItemIndex;

  if SelectedIndex <> -1 then begin
    SelectedIssue := TLiveIssue(IssueListBox.Items.Objects[SelectedIndex]);
    Rule := LintContext.GetRule(SelectedIssue.RuleKey);
    RulePanel.Visible := True;
    SplitPanel.Visible := True;
    if Assigned(Rule) then begin
      SetRuleView(Rule.Name, Rule.RuleKey, Rule.RuleType, Rule.Severity, Rule.Desc);
    end
    else begin
      SetRuleView(
        SelectedIssue.RuleKey,
        SelectedIssue.RuleKey,
        TRuleType.rtCodeSmell,
        TRuleSeverity.rsMinor,
        'Metadata for this rule could not be retrieved.');
    end;
  end
  else begin
    SplitPanel.Visible := False;
    RulePanel.Visible := False;
  end;

  if WasVisible <> RulePanel.Visible then begin
    RepaintIssueView;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SetRuleView(
  Name: string;
  RuleKey: string;
  RuleType: TRuleType;
  Severity: TRuleSeverity;
  Desc: string
);
begin
  RuleNameLabel.Caption := Name;
  RuleTypeLabel.Caption := C_RuleTypeStrs[RuleType] + ' - ' + C_RuleSeverityStrs[Severity];
  RuleDescLabel.Caption := FHtmlRemover.Process(Desc);
end;

//______________________________________________________________________________________________________________________

constructor THtmlRemover.Create;
begin
  FBrRegex := TRegEx.Create('<br[^>]*\/?[^>]*>', [roCompiled]);
  FTagRegex := TRegEx.Create('<[^>]*>', [roCompiled]);
  FConsecutiveSpaceRegex := TRegEx.Create('[ \t](?=[ \t])', [roCompiled]);
  FConsecutiveNewlineRegex := TRegEx.Create('\n', [roCompiled]);
end;

function THtmlRemover.Process(Text: string): string;
begin
  Result := Text;
  Result := FConsecutiveNewlineRegex.Replace(Result, '');
  Result := FBrRegex.Replace(Result, #13#10);
  Result := FTagRegex.Replace(Result, '');
  Result := FConsecutiveSpaceRegex.Replace(Result, '');
end;

//______________________________________________________________________________________________________________________


end.
