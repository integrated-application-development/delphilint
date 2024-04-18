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
  , DelphiLint.LiveData
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
    IssueListBox: TListBox;
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
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure OnIssueSelected(Sender: TObject);
    procedure OnIssueDoubleClicked(Sender: TObject);
    procedure OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnMeasureIssueItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure RuleBrowserNavigationStarting(Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
    procedure RuleBrowserNewWindowRequested(Sender: TCustomEdgeBrowser; Args: TNewWindowRequestedEventArgs);
    procedure RuleBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
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
    procedure RepaintIssueView;
    procedure GetIssueItemText(
      ListBox: TListBox;
      Issue: ILiveIssue;
      out LocationText: string;
      out MessageText: string;
      out MetadataText: string);

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
  , Vcl.Graphics
  , System.Win.ComObj
  , Winapi.ShellAPI
  , Winapi.Messages
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

  SendMessage(IssueListBox.Handle, WM_SETREDRAW, 0, 0);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
begin
  ResizeIndicatorPanel.Left := SplitPanel.Left + X;
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
  SendMessage(IssueListBox.Handle, WM_SETREDRAW, 1, 0);
  ResizeIndicatorPanel.Visible := False;
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
  Result := nil;
  SelectedIndex := IssueListBox.ItemIndex;

  if SelectedIndex <> -1 then begin
    Result := TWrapper<ILiveIssue>(IssueListBox.Items.Objects[SelectedIndex]).Get;
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

procedure TLintToolFrame.GetIssueItemText(
  ListBox: TListBox;
  Issue: ILiveIssue;
  out LocationText: string;
  out MessageText: string;
  out MetadataText: string);
var
  CreationDateTime: TDateTime;
  TimeSinceCreation: TTimeSpan;
begin
  if Issue.IsTethered then begin
    LocationText := Format('(%d, %d) ', [Issue.StartLine, Issue.StartLineOffset]);
  end
  else begin
    LocationText := '';
  end;

  MessageText := Issue.Message;

  if Issue.HasMetadata then begin
    if Issue.CreationDate <> '' then begin
      CreationDateTime := ISO8601ToDate(Issue.CreationDate, False);
      TimeSinceCreation := TTimeSpan.Subtract(Now, CreationDateTime);

      MetadataText := Format('%s • %s • %s', [
        TimeSpanToAgoString(TimeSinceCreation),
        CIssueStatusStrs[Issue.Status],
        IfThen(Issue.Assignee <> '', 'Assigned to ' + Issue.Assignee, 'Unassigned')
      ]);
    end
    else begin
      MetadataText := 'New issue';
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnMeasureIssueItem(Control: TWinControl; Index: Integer; var Height: Integer);
var
  ListBox: TListBox;
  LocationText: string;
  MessageText: string;
  MetadataText: string;
  Issue: ILiveIssue;
  Rect: TRect;
begin
  ListBox := Control as TListBox;
  // For some reason, OnMeasureItem is called after a string is added to the listbox, but before its corresponding
  // object is added. This means we have to maintain a separate list to be able to retrieve them by index here.
  Issue := FIssues[Index].Get;

  GetIssueItemText(ListBox, Issue, LocationText, MessageText, MetadataText);

  Rect := TRect.Empty;
  Rect.Left := Rect.Left
    + CIssuePadding
    + 2 * CIssueIconWidth
    + ListBox.Canvas.TextWidth(LocationText);
  Rect.Right := ListBox.ClientRect.Right - CIssuePadding;
  Rect.Top := Rect.Top + CIssuePadding;
  Rect.Height := 0;

  DrawText(
    ListBox.Canvas.Handle,
    PChar(MessageText),
    Length(MessageText),
    Rect,
    DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

  Height := Rect.Height + CIssuePadding + ListBox.Canvas.TextHeight(MetadataText) + CIssuePadding;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  Issue: ILiveIssue;
  ListBox: TListBox;
  LocationText: string;
  MessageText: string;
  MetadataText: string;
  LocationWidth: Integer;
  MessageRect: TRect;
  TextLeft: Integer;
  Rule: TRule;
  IssueType: TRuleType;
  IssueSeverity: TRuleSeverity;
  MaxImpactSeverity: TImpactSeverity;
  HasCleanCode: Boolean;
begin
  ListBox := Control as TListBox;

  if (Index < 0) or (Index >= FIssues.Count) then begin
    Exit;
  end;

  Issue := FIssues[Index].Get;
  GetIssueItemText(ListBox, Issue, LocationText, MessageText, MetadataText);

  Canvas := ListBox.Canvas;
  Canvas.FillRect(Rect);

  Rule := Analyzer.GetRule(Issue.RuleKey);
  if not Assigned(Rule) then begin
    Log.Warn('Rule "%s" could not be drawn', [Issue.RuleKey]);
    Exit;
  end;

  HasCleanCode := Assigned(Rule.CleanCode);
  if HasCleanCode then begin
    MaxImpactSeverity := TArrayUtils.Max<TImpactSeverity>(Rule.CleanCode.Impacts.Values.ToArray, imsMedium);

    Canvas.Draw(
      Rect.Left + CIssuePadding,
      Rect.Top + CIssuePadding + 1,
      LintResources.ImpactSeverityIcon(MaxImpactSeverity));

    TextLeft := Rect.Left + CIssuePadding + CIssueIconWidth;
  end
  else begin
    IssueType := Rule.RuleType;
    IssueSeverity := Rule.Severity;

    Canvas.Draw(
      Rect.Left + CIssuePadding,
      Rect.Top + CIssuePadding + 1,
      LintResources.RuleTypeIcon(IssueType));
    TextLeft := Rect.Left + CIssuePadding + CIssueIconWidth;

    if IssueType <> rtSecurityHotspot then begin
      Canvas.Draw(
        Rect.Left + CIssuePadding + CIssueIconWidth,
        Rect.Top + CIssuePadding + 1,
        LintResources.RuleSeverityIcon(IssueSeverity));
      TextLeft := TextLeft + CIssueIconWidth;
    end;
  end;

  if not Issue.IsTethered then begin
    Canvas.Font.Color := clGrayText;
  end;

  LocationWidth := Canvas.TextWidth(LocationText);
  Canvas.TextOut(
    TextLeft,
    Rect.Top + CIssuePadding,
    LocationText);

  Canvas.Font.Style := [fsBold];

  MessageRect := TRect.Empty;
  MessageRect.Left := TextLeft + LocationWidth;
  MessageRect.Right := Rect.Right - CIssuePadding;
  MessageRect.Top := Rect.Top + CIssuePadding;
  MessageRect.Bottom := Rect.Bottom - CIssuePadding - Canvas.TextHeight(MetadataText);

  DrawText(
    ListBox.Canvas.Handle,
    PChar(MessageText),
    Length(MessageText),
    MessageRect,
    DT_LEFT or DT_WORDBREAK);

  Canvas.Font.Style := [];

  Canvas.TextOut(Rect.Left + CIssuePadding, MessageRect.Bottom, MetadataText);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnIssueDoubleClicked(Sender: TObject);
var
  SelectedIndex: Integer;
  SelectedIssue: ILiveIssue;
  Editor: IIDESourceEditor;
begin
  SelectedIndex := IssueListBox.ItemIndex;

  // No item selected
  if SelectedIndex = -1 then begin
    Exit;
  end;

  SelectedIssue := GetSelectedIssue;
  if not Assigned(SelectedIssue) then begin
    Log.Warn('Nonexistent issue double clicked');
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
    FIssues.Clear;
    FIssues.AddRange(TWrapper<ILiveIssue>.WrapArray(Analyzer.GetIssues(FCurrentPath)));
  end;

  RepaintIssueView;
  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RepaintIssueView;
var
  IssueWrapper: TWrapper<ILiveIssue>;
  Issue: ILiveIssue;
  Selected: Integer;
begin
  Selected := IssueListBox.ItemIndex;
  IssueListBox.ClearSelection;
  IssueListBox.Clear;

  for IssueWrapper in FIssues do begin
    Issue := IssueWrapper.Get;
    IssueListBox.AddItem(Format('%d: %s', [Issue.StartLine, Issue.Message]), IssueWrapper);
  end;

  if IssueListBox.Items.Count > Selected then begin
    IssueListBox.ItemIndex := Selected;
  end;
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
  WasVisible: Boolean;
begin
  WasVisible := RulePanel.Visible;

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

  if WasVisible <> RulePanel.Visible then begin
    RepaintIssueView;
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
