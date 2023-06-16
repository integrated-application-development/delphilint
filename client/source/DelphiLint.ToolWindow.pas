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
unit DelphiLint.ToolWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus,
  DelphiLint.ToolFrame, System.RegularExpressions, DelphiLint.Data;

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

  TLintToolWindow = class(TDockableForm)
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
    FCurrentPath: string;
    FFrame: TLintToolFrame;
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
    class procedure CreateInstance;
    class procedure RemoveInstance;
    class procedure ShowInstance;
    class function Instance: TLintToolWindow;

    procedure Focus;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure AnalysisStarted;
    procedure AnalysisFailed;
    procedure AnalysisCleared;
    procedure AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean = False);
    procedure ChangeActiveFile(const Path: string);
  end;

implementation

{$R *.dfm}

uses
    DeskUtil
  , ToolsAPI
  , DelphiLint.Context
  , DelphiLint.Logger
  , System.Math
  , System.StrUtils
  , System.IOUtils
  , DelphiLint.Utils
  , DelphiLint.Plugin
  ;

var
  GToolWindow: TLintToolWindow;

//______________________________________________________________________________________________________________________

procedure RegisterDockableForm(var FormInstance: TLintToolWindow);
begin
  if @RegisterFieldAddress <> nil then begin
    RegisterFieldAddress(FormInstance.Name, @FormInstance);
  end;

  RegisterDesktopFormClass(TLintToolWindow, FormInstance.Name, FormInstance.Name);
  (BorlandIDEServices as IOTAIDEThemingServices).RegisterFormClass(TLintToolWindow);
end;

//______________________________________________________________________________________________________________________

procedure UnregisterDockableForm(var FormInstance: TLintToolWindow);
begin
  if (@UnregisterFieldAddress <> nil) and Assigned(FormInstance) then begin
    UnregisterFieldAddress(@FormInstance);
  end;
end;

//______________________________________________________________________________________________________________________

procedure CreateDockableForm(var FormInstance: TLintToolWindow);
begin
  FormInstance := TLintToolWindow.Create(nil);
  RegisterDockableForm(FormInstance);
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(FormInstance);
end;

//______________________________________________________________________________________________________________________

procedure FreeDockableForm(var FormInstance: TLintToolWindow);
begin
  UnregisterDockableForm(FormInstance);
  FreeAndNil(FormInstance);
end;

//______________________________________________________________________________________________________________________

procedure ShowDockableForm(var FormInstance: TLintToolWindow);
begin
  if Assigned(FormInstance) then begin
    if FormInstance.Floating then begin
      FormInstance.Show;
      FormInstance.Focus;
    end
    else begin
      FormInstance.ForceShow;
      FocusWindow(FormInstance);
      FormInstance.Focus;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

class procedure TLintToolWindow.CreateInstance;
begin
  if not Assigned(GToolWindow) then begin
    CreateDockableForm(GToolWindow);
  end;
end;

//______________________________________________________________________________________________________________________

class procedure TLintToolWindow.RemoveInstance;
begin
  if Assigned(GToolWindow) then begin
    FreeDockableForm(GToolWindow);
  end;
end;

//______________________________________________________________________________________________________________________

class procedure TLintToolWindow.ShowInstance;
begin
  CreateInstance;
  ShowDockableForm(GToolWindow);
end;

//______________________________________________________________________________________________________________________

class function TLintToolWindow.Instance: TLintToolWindow;
begin
  Result := GToolWindow;
end;

//______________________________________________________________________________________________________________________

function TLintToolWindow.IsFileScannable(const Path: string): Boolean;

  function IsProjectFile: Boolean;
  var
    Project: IOTAProject;
    FileList: TStringList;
  begin
    Project := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
    if Assigned(Project) then begin
      FileList := TStringList.Create;
      try
        Project.GetCompleteFileList(FileList);
        Result := FileList.IndexOf(Path) <> -1;
      finally
        FreeAndNil(FileList);
      end;
    end
    else begin
      Result := False;
    end;
  end;

begin
  Result := (Path <> '') and IsPasFile(Path) and IsProjectFile;
end;

//______________________________________________________________________________________________________________________

constructor TLintToolWindow.Create(Owner: TComponent);
var
  Editor: IOTASourceEditor;
begin
  inherited;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;

  FHtmlRemover := THtmlRemover.Create;

  FFrame := TLintToolFrame.Create(Self);
  FFrame.Parent := Self;
  FFrame.Align := alClient;

  FFrame.IssueListBox.OnDrawItem := OnDrawIssueItem;
  FFrame.IssueListBox.OnClick := OnIssueSelected;
  FFrame.IssueListBox.OnDblClick := OnIssueDoubleClicked;

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
        AnalysisSucceeded(History.IssuesFound, False);
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
end;

//______________________________________________________________________________________________________________________

destructor TLintToolWindow.Destroy;
begin
  FreeAndNil(FHtmlRemover);

  SaveStateNecessary := True;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.Focus;
begin
  SetFocus;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.UpdateFileNameLabel(NewText: string = '');
begin
  if NewText = '' then begin
    FFrame.FileNameLabel.Caption := TPath.GetFileName(FCurrentPath);
  end
  else begin
    FFrame.FileNameLabel.Caption := NewText;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.ChangeActiveFile(const Path: string);
var
  History: TFileAnalysisHistory;
begin
  if LintContext.InAnalysis then begin
    Exit;
  end;

  if IsFileScannable(Path) then begin
    FCurrentPath := Path;
    UpdateFileNameLabel;
    FFrame.LintButton.Enabled := True;

    case LintContext.GetAnalysisStatus(Path) of
      fasNeverAnalyzed:
          AnalysisCleared;
      fasOutdatedAnalysis:
        if LintContext.TryGetAnalysisHistory(Path, History) then begin
          AnalysisSucceeded(History.IssuesFound, True);
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently outdated analysis.');
          AnalysisCleared;
        end;
      fasUpToDateAnalysis:
        if LintContext.TryGetAnalysisHistory(Path, History) then begin
          AnalysisSucceeded(History.IssuesFound, False);
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently up-to-date analysis.');
          AnalysisCleared;
        end;
    end;
  end
  else begin
    FCurrentPath := '';
    AnalysisCleared;
    UpdateFileNameLabel('File not analyzable');
    FFrame.LintButton.Enabled := False;
    RefreshIssueView;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisCleared;
begin
  Plugin.LintImages.GetIcon(C_ImgDefault, FFrame.ProgImage.Picture.Icon);
  FFrame.LintButton.Hint := 'Scan current file';
  FFrame.ProgLabel.Caption := 'Not analyzed';
  FFrame.ProgBar.Hide;
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisFailed;
begin
  Plugin.LintImages.GetIcon(C_ImgError, FFrame.ProgImage.Picture.Icon);
  FFrame.LintButton.Hint := 'Error occurred during analysis';
  FFrame.ProgLabel.Caption := 'Failed';
  FFrame.ProgBar.Hide;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisStarted;
begin
  Plugin.LintImages.GetIcon(C_ImgWorking, FFrame.ProgImage.Picture.Icon);
  FFrame.LintButton.Hint := 'Analysis in progress';
  FFrame.ProgLabel.Caption := 'Analyzing';
  FFrame.ProgBar.Show;
  FFrame.ProgBar.Style := TProgressBarStyle.pbstNormal;
  FFrame.ProgBar.Style := TProgressBarStyle.pbstMarquee;
  Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean);
var
  ImageIndex: Integer;
begin
  if IssueCount = 0 then begin
    ImageIndex := IfThen(Outdated, C_ImgSuccessWarn, C_ImgSuccess);
    FFrame.ProgLabel.Caption := Format('No issues%s', [IfThen(Outdated, ' (outdated)', '')]);
  end
  else begin
    ImageIndex := IfThen(Outdated, C_ImgIssuesWarn, C_ImgIssues);
    FFrame.ProgLabel.Caption := Format('%d issues%s', [IssueCount,IfThen(Outdated, ' (outdated)', '')]);
  end;
  Plugin.LintImages.GetIcon(ImageIndex, FFrame.ProgImage.Picture.Icon);
  FFrame.LintButton.Hint := 'Analysis succeeded';
  FFrame.ProgBar.Hide;
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
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

procedure TLintToolWindow.OnIssueDoubleClicked(Sender: TObject);
var
  SelectedIndex: Integer;
  SelectedIssue: TLiveIssue;
  Editor: IOTASourceEditor;
  Buffer: IOTAEditBuffer;
begin
  SelectedIndex := FFrame.IssueListBox.ItemIndex;

  // No item selected
  if SelectedIndex = -1 then begin
    Exit;
  end;

  SelectedIssue := TLiveIssue(FFrame.IssueListBox.Items.Objects[SelectedIndex]);

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

procedure TLintToolWindow.OnIssueSelected(Sender: TObject);
begin
  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.RefreshIssueView;
var
  Issues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
begin
  FFrame.IssueListBox.Clear;
  FFrame.IssueListBox.ClearSelection;

  if FCurrentPath <> '' then begin
    Issues := LintContext.GetIssues(FCurrentPath);
    for Issue in Issues do begin
      FFrame.IssueListBox.AddItem(Format('%d: %s', [Issue.StartLine, Issue.Message]), Issue);
    end;
  end;

  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.RefreshRuleView;
var
  SelectedIndex: Integer;
  SelectedIssue: TLiveIssue;
  Rule: TRule;
begin
  SelectedIndex := FFrame.IssueListBox.ItemIndex;

  if SelectedIndex <> -1 then begin
    SelectedIssue := TLiveIssue(FFrame.IssueListBox.Items.Objects[SelectedIndex]);
    Rule := LintContext.GetRule(SelectedIssue.RuleKey);
    FFrame.RulePanel.Visible := True;
    FFrame.SplitPanel.Visible := True;
    SetRuleView(Rule.Name, Rule.RuleKey, Rule.RuleType, Rule.Severity, Rule.Desc);
  end
  else begin
    FFrame.RulePanel.Visible := False;
    FFrame.SplitPanel.Visible := False;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.SetRuleView(
  Name: string;
  RuleKey: string;
  RuleType: TRuleType;
  Severity: TRuleSeverity;
  Desc: string
);
begin
  FFrame.RuleNameLabel.Caption := Name;
  FFrame.RuleTypeLabel.Caption := C_RuleTypeStrs[RuleType] + ' - ' + C_RuleSeverityStrs[Severity];
  FFrame.RuleDescLabel.Caption := FHtmlRemover.Process(Desc);
end;

//______________________________________________________________________________________________________________________

constructor THtmlRemover.Create;
begin
  FBrRegex := TRegEx.Create('<br[^>]*\/?[^>]*>', [roCompiled]);
  FTagRegex := TRegEx.Create('<[^>]*>', [roCompiled]);
  FConsecutiveSpaceRegex := TRegEx.Create('[ \t](?=[ \t])', [roCompiled]);
  FConsecutiveNewlineRegex := TRegEx.Create('\n', [roCompiled]);
end;

//______________________________________________________________________________________________________________________

function THtmlRemover.Process(Text: string): string;
begin
  Result := Text;
  Result := FConsecutiveNewlineRegex.Replace(Result, '');
  Result := FBrRegex.Replace(Result, #13#10);
  Result := FTagRegex.Replace(Result, '');
  Result := FConsecutiveSpaceRegex.Replace(Result, '');
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  if Assigned(GToolWindow) then begin
    GToolWindow.RemoveInstance;
  end;

end.
