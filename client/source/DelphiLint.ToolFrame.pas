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
  Vcl.ToolWin, System.RegularExpressions, DelphiLint.Data;

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

  TLintToolFrame = class(TFrame)
    FileHeadingPanel: TPanel;
    ProgLabel: TLabel;
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
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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
    FCurrentPath: string;
    FHtmlRemover: THtmlRemover;

    function IsFileScannable(const Path: string): Boolean;
    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    procedure OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnIssueSelected(Sender: TObject);
    procedure OnIssueDoubleClicked(Sender: TObject);

    procedure RefreshRuleView;
    procedure SetRuleView(Name: string; RuleKey: string; RuleType: TRuleType; Severity: TRuleSeverity; Desc: string);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure AnalysisStarted;
    procedure AnalysisFailed;
    procedure AnalysisCleared;
    procedure AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean = False);
    procedure ChangeActiveFile(const Path: string);
    procedure RefreshActiveFile;
  end;

implementation

uses
    DelphiLint.Context
  , ToolsAPI
  , DelphiLint.Utils
  , System.StrUtils
  , System.IOUtils
  , DelphiLint.Logger
  , System.Math
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

  IssueListBox.OnDrawItem := OnDrawIssueItem;
  IssueListBox.OnClick := OnIssueSelected;
  IssueListBox.OnDblClick := OnIssueDoubleClicked;

  LintContext.OnAnalysisStarted.AddListener(
    procedure(const Paths: TArray<string>) begin
      AnalysisStarted;
    end);

  LintContext.OnAnalysisComplete.AddListener(
    procedure(const Paths: TArray<string>)
    var
      History: TFileAnalysisHistory;
    begin
      if LintContext.TryGetAnalysisHistory(Paths[0], History) then begin
        RefreshActiveFile;
      end;
    end);

  LintContext.OnAnalysisFailed.AddListener(
    procedure(const Paths: TArray<string>) begin
      AnalysisFailed;
    end);

  Editor := GetCurrentSourceEditor;
  if Assigned(Editor) then begin
    ChangeActiveFile(Editor.FileName);
  end
  else begin
    AnalysisCleared;
    UpdateFileNameLabel('No file selected');
  end;

  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(Self.Owner);

  Plugin.RegisterToolFrame(Self);
end;

//______________________________________________________________________________________________________________________

destructor TLintToolFrame.Destroy;
begin
  FreeAndNil(FHtmlRemover);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FResizing := True;
end;

procedure TLintToolFrame.SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewWidth: Integer;
begin
  if FResizing then begin
    NewWidth := RulePanel.Width - X;

    if (NewWidth < ContentPanel.Width - 10) then begin
      RulePanel.Width := NewWidth;
    end;
  end;
end;

procedure TLintToolFrame.SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FResizing := False;
end;

//______________________________________________________________________________________________________________________

function TLintToolFrame.IsFileScannable(const Path: string): Boolean;

  function IsProjectFile: Boolean;
  var
    ProjectDir: string;
  begin
    ProjectDir := NormalizePath(DelphiLint.Utils.GetProjectDirectory);
    Result := StartsText(ProjectDir, NormalizePath(Path));
  end;

begin
  Result := (Path <> '') and IsPasFile(Path) and IsProjectFile and FileExists(Path);
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
  FileScannable := IsFileScannable(Path);
  Plugin.AnalysisActionsEnabled := FileScannable;

  if FileScannable then begin
    FCurrentPath := Path;
  end
  else begin
    FCurrentPath := '';
  end;

  if LintContext.InAnalysis then begin
    Exit;
  end;

  if FileScannable then begin
    UpdateFileNameLabel;

    case LintContext.GetAnalysisStatus(Path) of
      fasNeverAnalyzed:
          AnalysisCleared;
      fasOutdatedAnalysis:
        if LintContext.TryGetAnalysisHistory(Path, History) then begin
          if History.Success then begin
            AnalysisSucceeded(History.IssuesFound, True);
          end
          else begin
            AnalysisFailed;
          end;
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently outdated analysis.');
          AnalysisCleared;
        end;
      fasUpToDateAnalysis:
        if LintContext.TryGetAnalysisHistory(Path, History) then begin
          if History.Success then begin
            AnalysisSucceeded(History.IssuesFound, False);
          end
          else begin
            AnalysisFailed;
          end;
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently up-to-date analysis.');
          AnalysisCleared;
        end;
    end;
  end
  else begin
    AnalysisCleared;
    UpdateFileNameLabel('File not analyzable');
    RefreshIssueView;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.AnalysisCleared;
begin
  Plugin.LintImages.GetIcon(C_ImgDefault, ProgImage.Picture.Icon);
  ProgLabel.Caption := 'Not analyzed';
  ProgBar.Hide;
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.AnalysisFailed;
begin
  Plugin.LintImages.GetIcon(C_ImgError, ProgImage.Picture.Icon);
  ProgLabel.Caption := 'Failed';
  ProgBar.Hide;
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.AnalysisStarted;
begin
  Plugin.LintImages.GetIcon(C_ImgWorking, ProgImage.Picture.Icon);
  ProgLabel.Caption := 'Analyzing';
  ProgBar.Show;
  ProgBar.Style := TProgressBarStyle.pbstNormal;
  ProgBar.Style := TProgressBarStyle.pbstMarquee;
  RefreshIssueView;
  Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean);
var
  ImageIndex: Integer;
begin
  if IssueCount = 0 then begin
    ImageIndex := IfThen(Outdated, C_ImgSuccessWarn, C_ImgSuccess);
    ProgLabel.Caption := Format('No issues%s', [IfThen(Outdated, ' (outdated)', '')]);
  end
  else begin
    ImageIndex := IfThen(Outdated, C_ImgIssuesWarn, C_ImgIssues);
    ProgLabel.Caption := Format('%d issues%s', [IssueCount,IfThen(Outdated, ' (outdated)', '')]);
  end;
  Plugin.LintImages.GetIcon(ImageIndex, ProgImage.Picture.Icon);
  ProgBar.Hide;
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  Issue: TLiveIssue;
  ListBox: TListBox;
  LocationText: string;
  LocationWidth: Integer;
begin
  ListBox := Control as TListBox;

  Canvas := ListBox.Canvas;
  Issue := TLiveIssue(ListBox.Items.Objects[Index]);
  Canvas.FillRect(Rect);

  if Issue.StartLine <> -1 then begin
    LocationText := Format('(%d, %d) ', [Issue.StartLine, Issue.StartLineOffset]);
  end
  else begin
    LocationText := '(deleted) ';
    Canvas.Font.Color := clGrayText;
  end;

  LocationWidth := Canvas.TextWidth(LocationText);
  Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, LocationText);

  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(Rect.Left + 4 + LocationWidth, Rect.Top + 4, Issue.Message);
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

  Editor := GetCurrentSourceEditor;
  if Assigned(Editor) and (Editor.EditViewCount <> 0) then begin
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
var
  Issues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
begin
  IssueListBox.Clear;
  IssueListBox.ClearSelection;

  if FCurrentPath <> '' then begin
    Issues := LintContext.GetIssues(FCurrentPath);
    for Issue in Issues do begin
      IssueListBox.AddItem(Format('%d: %s', [Issue.StartLine, Issue.Message]), Issue);
    end;
  end;

  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshRuleView;
var
  SelectedIndex: Integer;
  SelectedIssue: TLiveIssue;
  Rule: TRule;
begin
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
    RulePanel.Visible := False;
    SplitPanel.Visible := False;
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
