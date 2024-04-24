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
unit DelphiLint.ToolFrame;

interface

uses
    System.Classes
  , System.Generics.Collections
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.ComCtrls
  , Vcl.ExtCtrls
  , Vcl.StdCtrls
  , Vcl.Graphics
  , Vcl.Menus
  , Vcl.ToolWin
  , Winapi.Windows
  , DelphiLint.Data
  , DelphiLint.IDEBaseTypes
  , DelphiLint.HtmlGen
  , DelphiLint.Utils
  , DelphiLint.Context
  , Vcl.Edge
  , Winapi.WebView2
  , Winapi.ActiveX
  , DelphiLint.LiveData, Vcl.ControlList
  ;

type
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
    LintButtonPanel: TPanel;
    RulePanel: TPanel;
    ContentPanel: TPanel;
    SplitPanel: TPanel;
    TopPanel: TPanel;
    LintToolBar: TToolBar;
    AnalyzeShortButton: TToolButton;
    AnalyzePopupMenu: TPopupMenu;
    AnalyzeCurrentFile1: TMenuItem;
    AnalyzeOpenFiles1: TMenuItem;
    StatusPanel: TPanel;
    ProgLabel: TLabel;
    ResizeIndicatorPanel: TPanel;
    RuleBrowser: TEdgeBrowser;
    ActionClearActiveFile1: TMenuItem;
    Separator1: TMenuItem;
    IssueControlList: TControlList;
    IssueMessageLabel: TLabel;
    IssueImage: TImage;
    IssueMetaLabel: TLabel;
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
    procedure RuleBrowserNavigationStarting(Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
    procedure RuleBrowserNewWindowRequested(Sender: TCustomEdgeBrowser; Args: TNewWindowRequestedEventArgs);
    procedure RuleBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
    procedure IssueControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
    procedure FrameResize(Sender: TObject);
    procedure IssueControlListItemClick(Sender: TObject);
    procedure IssueControlListItemDblClick(Sender: TObject);
  private const
    CIssueStatusStrs: array[TIssueStatus] of string = (
      'Open',
      'Confirmed',
      'Reopened',
      'Resolved',
      'Closed',
      'Accepted',
      'To review',
      'Reviewed (acknowledged)'
    );
    CIssueIconWidth = 16;
    CIssuePadding = 4;
  private
    FResizing: Boolean;
    FDragStartX: Integer;
    FCurrentPath: string;
    FIssues: TObjectList<TWrapper<ILiveIssue>>;
    FRuleHtmls: TDictionary<string, string>;
    FVisibleRule: string;
    FNavigationAllowed: Boolean;
    FRuleHtmlGenerator: TRuleHtmlGenerator;

    function GetSelectedIssue: ILiveIssue;
    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    function GetIssueMetadataText(Issue: ILiveIssue): string;

    procedure SetRuleView(Rule: TRule);

    procedure OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);

    procedure RefreshRuleView;

    function GetStatusCaption(Status: TCurrentFileStatus; NumIssues: Integer): string;
    procedure UpdateFileStatus(Status: TCurrentFileStatus; NumIssues: Integer = -1);
    procedure UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean = False);
    procedure UpdateAnalysisStatusForFile(const Path: string);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure ChangeActiveFile(const Path: string);
    procedure RefreshActiveFile;
  end;

  TLintToolFormInfo = class(TCustomDockableFormBase)
  public
    function GetCaption: string; override;
    function GetIdentifier: string; override;
    function GetFrameClass: TCustomFrameClass; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
  end;

implementation

uses
    System.SysUtils
  , System.DateUtils
  , System.TimeSpan
  , System.StrUtils
  , System.IOUtils
  , System.Types
  , System.UITypes
  , System.Win.ComObj
  , Winapi.ShellAPI
  , DelphiLint.Resources
  , DelphiLint.ExtWebView2
  ;

{$R *.dfm}


constructor TLintToolFrame.Create(Owner: TComponent);
var
  Editor: IIDESourceEditor;
begin
  inherited Create(Owner);
  FResizing := False;
  FCurrentPath := '';
  FNavigationAllowed := False;
  FRuleHtmls := TDictionary<string, string>.Create;
  FRuleHtmlGenerator := TRuleHtmlGenerator.Create;
  FIssues := TObjectList<TWrapper<ILiveIssue>>.Create;

  Analyzer.OnAnalysisStateChanged.AddListener(OnAnalysisStateChanged);

  if TryGetCurrentSourceEditor(Editor) then begin
    ChangeActiveFile(Editor.FileName);
  end
  else begin
    ChangeActiveFile('');
  end;

  if Analyzer.InAnalysis then begin
    OnAnalysisStarted(Analyzer.CurrentAnalysis.Paths);
  end
  else begin
    UpdateAnalysisStatus('Idle');
  end;

  LintContext.Plugin.OnActiveFileChanged.AddListener(ChangeActiveFile);

  TThread.ForceQueue(
    TThread.Current,
    procedure begin
      RuleBrowser.CreateWebView;
    end);
end;

//______________________________________________________________________________________________________________________

destructor TLintToolFrame.Destroy;
begin
  FreeAndNil(FRuleHtmls);
  FreeAndNil(FRuleHtmlGenerator);
  FreeAndNil(FIssues);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.FrameResize(Sender: TObject);
begin
  if RulePanel.Left < 10 then begin
    RulePanel.Width := Width div 2;
  end;
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

procedure TLintToolFrame.OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
begin
  case StateChange.Change of
    ascStarted: begin
      OnAnalysisStarted(StateChange.Files);
    end;
    ascSucceeded: begin
      OnAnalysisFinished(StateChange.Files, True);
    end;
    ascFailed: begin
      OnAnalysisFinished(StateChange.Files, False);
    end;
    ascCleared: begin
      ChangeActiveFile(FCurrentPath);
    end;
  end;
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

procedure TLintToolFrame.SplitPanelMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X: Integer;
  Y: Integer);
begin
  FResizing := True;
  FDragStartX := X;
  ResizeIndicatorPanel.Visible := True;
  ResizeIndicatorPanel.BoundsRect := SplitPanel.BoundsRect;
  ResizeIndicatorPanel.BringToFront;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
begin
  ResizeIndicatorPanel.Left := SplitPanel.Left + X;
  IssueControlList.Invalidate;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X: Integer;
  Y: Integer);
var
  NewWidth: Integer;
begin
  FResizing := False;
  ResizeIndicatorPanel.Visible := False;
  NewWidth := RulePanel.Width - (X - FDragStartX);

  if (NewWidth < ContentPanel.Width - 10) then begin
    RulePanel.Width := NewWidth;
  end;
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
  FileScannable: Boolean;
begin
  FileScannable := IsFileInProject(Path);
  FCurrentPath := IfThen(FileScannable, Path, '');

  if FileScannable then begin
    if Analyzer.InAnalysis and Analyzer.CurrentAnalysis.IncludesFile(Path) then begin
      UpdateFileStatus(cfsInAnalysis);
      Exit;
    end;

    UpdateAnalysisStatusForFile(Path);
  end
  else begin
    UpdateFileStatus(cfsNotAnalyzable);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.UpdateAnalysisStatusForFile(const Path: string);

  procedure UpdateAnalyzedFileStatus(const Path: string; Outdated: Boolean);
  var
    History: TFileAnalysisHistory;
  begin
    if not Analyzer.TryGetAnalysisHistory(Path, History) then begin
      Log.Warn('Could not get analysis history for file %s with apparently outdated analysis', [Path]);
      UpdateFileStatus(cfsNotAnalyzed);
      Exit;
    end;

    if not History.Success then begin
      UpdateFileStatus(cfsNotAnalyzed);
    end;

    if Outdated then begin
      if History.IssuesFound = 0 then begin
        UpdateFileStatus(cfsNoIssuesOutdated);
      end
      else begin
        UpdateFileStatus(cfsIssuesOutdated, History.IssuesFound);
      end;
    end
    else begin
      if History.IssuesFound = 0 then begin
        UpdateFileStatus(cfsNoIssues);
      end
      else begin
        UpdateFileStatus(cfsIssues, History.IssuesFound);
      end;
    end;
  end;

begin
  case Analyzer.GetAnalysisStatus(Path) of
  fasOutdatedAnalysis:
    UpdateAnalyzedFileStatus(Path, True);
  fasUpToDateAnalysis:
    UpdateAnalyzedFileStatus(Path, False);
  else
    UpdateFileStatus(cfsNotAnalyzed);
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

  ProgImage.Picture.Graphic := LintResources.LintStatusIcon(Status);
  FileStatusLabel.Caption := GetStatusCaption(Status, NumIssues);
  RefreshIssueView;
end;

//______________________________________________________________________________________________________________________

function TLintToolFrame.GetSelectedIssue: ILiveIssue;
var
  SelectedIndex: Integer;
begin

  SelectedIndex := IssueControlList.ItemIndex;
  if SelectedIndex = -1 then begin
    Result := nil;
  end
  else if (SelectedIndex >= 0) and (SelectedIndex < FIssues.Count) then begin
    Result := FIssues[SelectedIndex].Get;
  end
  else begin
    Log.Warn('Issue %d was selected in control list, but there were only %d issues', [SelectedIndex, FIssues.Count]);
    Result := nil;
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
    cfsIssues: begin
      if NumIssues = 1 then begin
        Result := '1 issue';
      end
      else begin
        Result := Format('%d issues', [NumIssues]);
      end;
    end;
    cfsIssuesOutdated:
      if NumIssues = 1 then begin
        Result := '1 issue (outdated)';
      end
      else begin
        Result := Format('%d issues (outdated)', [NumIssues]);
      end;
  else
    Result := 'Not analyzable';
  end;
end;

//______________________________________________________________________________________________________________________

function TLintToolFrame.GetIssueMetadataText(Issue: ILiveIssue): string;
var
  CreationDateTime: TDateTime;
  TimeSinceCreation: TTimeSpan;
  ExtraInfo: string;
begin
  Result := Format('(%d, %d)', [Issue.StartLine, Issue.StartLineOffset]);

  if Issue.HasMetadata then begin
    if Issue.CreationDate <> '' then begin
      CreationDateTime := ISO8601ToDate(Issue.CreationDate, False);
      TimeSinceCreation := TTimeSpan.Subtract(Now, CreationDateTime);

      ExtraInfo := Format('%s • %s • %s', [
        TimeSpanToAgoString(TimeSinceCreation),
        CIssueStatusStrs[Issue.Status],
        IfThen(Issue.Assignee <> '', 'Assigned to ' + Issue.Assignee, 'Unassigned')
      ]);
    end
    else begin
      ExtraInfo := 'New issue';
    end;
  end
  else begin
    ExtraInfo := 'New issue';
  end;

  Result := Format('%s • %s', [Result, ExtraInfo]);

  if not Issue.IsTethered then begin
    Result := Format('%s • %s', [Result, 'Potentially resolved']);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.IssueControlListBeforeDrawItem(
  AIndex: Integer;
  ACanvas: TCanvas;
  ARect: TRect;
  AState: TOwnerDrawState
);
var
  Issue: ILiveIssue;
  Rule: TRule;
  MaxImpactSeverity: TImpactSeverity;
begin
  if AIndex >= FIssues.Count then begin
    Exit;
  end;

  Issue := FIssues[AIndex].Get;
  IssueMessageLabel.Caption := Issue.Message;
  IssueMessageLabel.Enabled := Issue.IsTethered;

  IssueMetaLabel.Caption := GetIssueMetadataText(Issue);
  IssueMetaLabel.Enabled := Issue.IsTethered;

  Rule := Analyzer.GetRule(Issue.RuleKey);
  if not Assigned(Rule) then begin
    Log.Warn('Rule "%s" could not be drawn', [Issue.RuleKey]);
    IssueImage.Picture.Graphic := nil;
    Exit;
  end;

  if Assigned(Rule.CleanCode) then begin
    MaxImpactSeverity := TArrayUtils.Max<TImpactSeverity>(Rule.CleanCode.Impacts.Values.ToArray, imsMedium);
    IssueImage.Picture.Graphic := LintResources.ImpactSeverityIcon(MaxImpactSeverity);
  end
  else begin
    IssueImage.Picture.Graphic := LintResources.RuleTypeIcon(Rule.RuleType);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.IssueControlListItemClick(Sender: TObject);
begin
  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.IssueControlListItemDblClick(Sender: TObject);
var
  SelectedIssue: ILiveIssue;
  Editor: IIDESourceEditor;
begin
  SelectedIssue := GetSelectedIssue;
  if not Assigned(SelectedIssue) then begin
    Exit;
  end;

  // Issue line has been removed
  if SelectedIssue.StartLine = -1 then begin
    Exit;
  end;

  if TryGetCurrentSourceEditor(Editor) and (Editor.EditViewCount <> 0) then begin
    Editor.EditViews[0].GoToPosition(SelectedIssue.StartLine, SelectedIssue.StartLineOffset);
    Editor.EditViews[0].Paint;
  end;
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
    FIssues.Clear;
    FIssues.AddRange(TWrapper<ILiveIssue>.WrapArray(Analyzer.GetIssues(FCurrentPath)));
    IssueControlList.ItemCount := FIssues.Count;
    IssueControlList.Repaint;
  end;

  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
var
  Controller2: ICoreWebView2Controller2;
  Color: COREWEBVIEW2_COLOR;
  DelphiColor: Integer;
begin
  if not RuleBrowser.WebViewCreated then begin
    Exit;
  end;

  RuleBrowser.SettingsInterface.Set_AreDevToolsEnabled({$IFDEF DEBUG}1{$ELSE}0{$ENDIF});
  RuleBrowser.SettingsInterface.Set_AreDefaultContextMenusEnabled(0);
  RuleBrowser.SettingsInterface.Set_IsZoomControlEnabled(0);

  if Supports(RuleBrowser.ControllerInterface, ICoreWebView2Controller2, Controller2) then begin
    DelphiColor := ColorToRGB(LintContext.IDEServices.GetSystemColor(clWindow));
    Color.R := GetRValue(DelphiColor);
    Color.G := GetGValue(DelphiColor);
    Color.B := GetBValue(DelphiColor);
    Color.A := 255;
    Controller2.Put_DefaultBackgroundColor(Color);

    Controller2.Get_DefaultBackgroundColor(Color);
    Log.Info('Default background is now rgba(%d, %d, %d, %d)', [Color.R, Color.G, Color.B, Color.A]);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserNavigationStarting(Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
var
  ArgsInterface3: ICoreWebView2NavigationStartingEventArgs3;
  NavigationKind: COREWEBVIEW2_NAVIGATION_KIND;
  Uri: PWideChar;
begin
  if Supports(Args.ArgsInterface, ICoreWebView2NavigationStartingEventArgs3, ArgsInterface3) then begin
    ArgsInterface3.Get_NavigationKind(NavigationKind);
    if NavigationKind = COREWEBVIEW2_NAVIGATION_KIND_BACK_OR_FORWARD then begin
      Log.Info('Back/forward requested in rule webview, cancelling');
      Args.ArgsInterface.Set_Cancel(1);
    end;
  end;

  Args.ArgsInterface.Get_uri(Uri);
  if not StartsText('file:', Uri) then begin
    Log.Info('New URL requested in rule webview, intercepting and showing externally');
    Args.ArgsInterface.Set_Cancel(1);
    ShellExecute(0, 'open', Uri, nil, nil, SW_SHOWNORMAL);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserNewWindowRequested(Sender: TCustomEdgeBrowser; Args: TNewWindowRequestedEventArgs);
var
  Uri: PWideChar;
begin
  Log.Info('New window requested in rule webview, intercepting and showing externally');
  Args.ArgsInterface.Set_Handled(1);
  Args.ArgsInterface.Get_uri(Uri);
  ShellExecute(0, 'open', Uri, nil, nil, SW_SHOWNORMAL);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshRuleView;
var
  SelectedIssue: ILiveIssue;
  Rule: TRule;
begin
  SelectedIssue := GetSelectedIssue;

  if Assigned(SelectedIssue) then begin
    Rule := Analyzer.GetRule(SelectedIssue.RuleKey);
    RulePanel.Visible := True;
    SplitPanel.Visible := True;
    if Assigned(Rule) then begin
      SetRuleView(Rule);
    end;
  end
  else begin
    SplitPanel.Visible := False;
    RulePanel.Visible := False;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SetRuleView(Rule: TRule);
begin
  if (not FRuleHtmls.ContainsKey(Rule.RuleKey)) or (not FileExists(FRuleHtmls[Rule.RuleKey])) then begin
    FRuleHtmls.AddOrSetValue(Rule.RuleKey, FRuleHtmlGenerator.GenerateHtmlFile(Rule));
  end;

  try
    FNavigationAllowed := True;
    if FVisibleRule <> Rule.RuleKey then begin
      try
        RuleBrowser.Navigate('file:///' + NormalizePath(FRuleHtmls[Rule.RuleKey]));
      except
        on E: EOleException do begin
          Log.Warn('OLE exception occurred during navigation: %s', [E.Message]);
          if E.Message <> 'Unspecified error' then begin
            raise;
          end;
        end;
      end;
      FVisibleRule := Rule.RuleKey;
    end;
  finally
    FNavigationAllowed := False;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintToolFormInfo.GetIdentifier: string;
begin
  Result := 'DelphiLintToolForm';
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFormInfo.FrameCreated(AFrame: TCustomFrame);
begin
  LintContext.IDEServices.ApplyTheme(AFrame);
end;

//______________________________________________________________________________________________________________________

function TLintToolFormInfo.GetCaption: string;
begin
  Result := 'DelphiLint';
end;

//______________________________________________________________________________________________________________________

function TLintToolFormInfo.GetFrameClass: TCustomFrameClass;
begin
  Result := TLintToolFrame;
end;

end.
