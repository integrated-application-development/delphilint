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
unit DelphiLint.ToolFrame.Legacy;

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
  , Vcl.OleCtrls
  , SHDocVw
  , Winapi.Windows
  , DelphiLint.Data
  , DelphiLint.IDEBaseTypes
  , DelphiLint.HtmlGen
  , DelphiLint.Utils
  , DelphiLint.Context
  , DelphiLint.LiveData
  , Vcl.Buttons
  , System.ImageList
  , Vcl.ImgList
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
    ContentPanel: TPanel;
    RulePanel: TPanel;
    StatusPanel: TPanel;
    ProgBar: TProgressBar;
    ProgLabel: TLabel;
    ResizeIndicatorPanel: TPanel;
    SplitPanel: TPanel;
    TopPanel: TPanel;
    WebViewInitTimer: TTimer;
    ErrorImageList: TImageList;
    ErrorButtonPanel: TPanel;
    WarningButtonPanel: TPanel;
    WarningButton: TSpeedButton;
    ErrorButton: TSpeedButton;
    AnalyzePopupMenu: TPopupMenu;
    AnalyzeCurrentFile1: TMenuItem;
    AnalyzeOpenFiles1: TMenuItem;
    Separator1: TMenuItem;
    ActionClearActiveFile1: TMenuItem;
    Separator2: TMenuItem;
    ViewLogItem: TMenuItem;
    IssueContextMenu: TPopupMenu;
    ListView: TListView;
    IssueImage: TImage;
    IssueMessageLabel: TLabel;
    IssueMetaLabel: TLabel;
    FileHeadingPanel: TPanel;
    FileNameLabel: TLabel;
    ProgImage: TImage;
    FileStatusLabel: TLabel;
    LintButtonPanel: TPanel;
    LintToolBar: TToolBar;
    AnalyzeShortButton: TToolButton;
    RuleBrowser: TWebBrowser;
    MarqueeTimer: TTimer;
    procedure BtnNavigateClick(Sender: TObject);
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
    procedure RuleBrowserBeforeNavigate2(Sender: TObject; const pDisp: IDispatch;
      const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure RuleBrowserNewWindow2(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
    procedure RuleBrowserDocumentComplete(Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure FrameResize(Sender: TObject);
    procedure IssueControlListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure IssueControlListItemDblClick(Sender: TObject);
    procedure MarqueeTimerTimer(Sender: TObject);
    procedure ViewLogClick(Sender: TObject);
    procedure WebViewInitTimerTimer(Sender: TObject);
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
  private
    FResizing: Boolean;
    FDragStartX: Integer;
    FCurrentPath: string;
    FIssues: TObjectList<TWrapper<ILiveIssue>>;
    FRuleHtmls: TDictionary<string, string>;
    FLastAnalysisLogs: TArray<string>;
    FLastAnalysisTime: TDateTime;
    FVisibleRule: string;
    FNavigationAllowed: Boolean;
    FRuleHtmlGenerator: TRuleHtmlGenerator;
    FBrowserHwnd: HWND;

    function GetSelectedIssue: ILiveIssue;
    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    function GetIssueMetadataText(Issue: ILiveIssue): string;

    procedure SetRuleView(Rule: TRule);

    procedure OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);

    procedure RefreshRuleView;
    procedure DirtyWebView;

    function GetStatusCaption(Status: TCurrentFileStatus; NumIssues: Integer): string;
    procedure UpdateFileStatus(Status: TCurrentFileStatus; NumIssues: Integer = -1);
    procedure UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean = False);
    procedure UpdateAnalysisStatusForFile(const Path: string);

    procedure SetLogMessages(LogMessages: TArray<string>);

    function CreateIssuePopup(Index: Integer): TPopupMenu;
    procedure OpenRuleInBrowser(Sender: TObject);
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
  , Vcl.Dialogs
  , Vcl.Graphics
  , Winapi.ShellAPI
  , DelphiLint.Resources
  , DelphiLint.IssueActions
  , DelphiLint.LogViewer
  , System.Variants
  , System.RegularExpressions
  ;

{$R *.dfm}

procedure AssignCleanImageToButtonGlyph(Button: TSpeedButton; ImageList: TCustomImageList; Index: Integer);
var
  Bmp: Vcl.Graphics.TBitmap;
  R: TRect;
begin
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(ImageList.Width, ImageList.Height);

    Bmp.Canvas.Brush.Color := clFuchsia;
    R := Rect(0, 0, Bmp.Width, Bmp.Height);
    Bmp.Canvas.FillRect(R);

    ImageList.Draw(Bmp.Canvas, 0, 0, Index, True);

    Button.Glyph.Assign(Bmp);
    Button.Glyph.Transparent := True;
    Button.Glyph.TransparentColor := clFuchsia;
    Button.NumGlyphs := 1;
  finally
    Bmp.Free;
  end;
end;

constructor TLintToolFrame.Create(Owner: TComponent);
var
  Editor: IIDESourceEditor;
begin
  inherited Create(Owner);

  ListView.SmallImages := TImageList.Create(Self);
  ListView.SmallImages.Height := 40;

  AssignCleanImageToButtonGlyph(WarningButton, ErrorImageList, 2);
  AssignCleanImageToButtonGlyph(ErrorButton, ErrorImageList, 0);

  FResizing := False;
  FCurrentPath := ExtractFilePath(ParamStr(0));
  FNavigationAllowed := False;
  FRuleHtmls := TDictionary<string, string>.Create;

  FRuleHtmlGenerator := TRuleHtmlGenerator.Create;

  FIssues := TObjectList<TWrapper<ILiveIssue>>.Create;

  FBrowserHwnd := RuleBrowser.Handle;
  RuleBrowser.Navigate('about:blank');


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

  DirtyWebView;

  SetLogMessages([]);

  LintContext.Plugin.OnActiveFileChanged.AddListener(ChangeActiveFile);
end;

//______________________________________________________________________________________________________________________

function TLintToolFrame.CreateIssuePopup(Index: Integer): TPopupMenu;

  function DummyMenuItem(Owner: TComponent): TMenuItem;
  begin
    Result := TMenuItem.Create(Owner);
    Result.Visible := False;
  end;

var
  Issue: ILiveIssue;
  MenuItemFactory: TIssueMenuItemFactory;
  Item: TMenuItem;
  I: Integer;
begin
  if (Index < 0) or (Index >= FIssues.Count) then begin
    Result := nil;
    Exit;
  end;

  Result := IssueContextMenu;

  for I := Result.Items.Count - 1 downto 0 do begin
    Result.Items[I].Free;
  end;

  Item := TMenuItem.Create(Self);
  Item.Caption := 'Open Rule in Browser';
  Item.OnClick := OpenRuleInBrowser;
  Result.Items.Add(Item);
  Item := TMenuItem.Create(Self);
  Item.Caption := '-';
  Result.Items.Add(Item);

  Issue := FIssues[Index].Get;
  MenuItemFactory := TIssueMenuItemFactory.Create(Issue);
  try
    Result.Items.Add(DummyMenuItem(Result));
    Result.Items.Add(MenuItemFactory.HideIssue(Result));

    Item := MenuItemFactory.ApplyQuickFix(Result);
    if Assigned(Item) then begin
      Result.Items.Add(Item);
    end;
  finally
    FreeAndNil(MenuItemFactory);
  end;
end;

procedure TLintToolFrame.OpenRuleInBrowser(Sender: TObject);
var
  SelectedIssue: ILiveIssue;
  Rule: TRule;
  HtmlFilePath: string;
begin
  SelectedIssue := GetSelectedIssue;

  if Assigned(SelectedIssue) then begin
    Rule := Analyzer.GetRule(SelectedIssue.RuleKey);
    if Assigned(Rule) then
    begin
      if (not FRuleHtmls.ContainsKey(Rule.RuleKey)) or (not FileExists(FRuleHtmls[Rule.RuleKey])) then begin
        FRuleHtmls.AddOrSetValue(Rule.RuleKey, FRuleHtmlGenerator.GenerateHtmlFile(Rule));
      end;

      HtmlFilePath := FRuleHtmls[Rule.RuleKey];
      ShellExecute(0, 'open', PChar(NormalizePath(HtmlFilePath)), nil, nil, SW_SHOWNORMAL);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

destructor TLintToolFrame.Destroy;
begin
  FreeAndNil(FRuleHtmls);
  FreeAndNil(FRuleHtmlGenerator);
  FreeAndNil(FIssues);
  inherited;
end;

procedure TLintToolFrame.BtnNavigateClick(Sender: TObject);
var
  LURL: string;
begin
  LURL := InputBox('URL: ', 'Navegar', 'file:///');
  FNavigationAllowed := True;
  RuleBrowser.Navigate(LURL);
  SplitPanel.Visible := True;
  RulePanel.Visible := True;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.ViewLogClick(Sender: TObject);
var
  Form: TLogViewerForm;
begin
  Form := TLogViewerForm.Create(nil, FLastAnalysisTime, FLastAnalysisLogs);
  try
    LintContext.IDEServices.ApplyTheme(Form);
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.FrameResize(Sender: TObject);
begin
  if RulePanel.Left < 10 then begin
    RulePanel.Width := Width div 2;
  end;

  DirtyWebView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean);
begin
  MarqueeTimer.Enabled := ShowProgress;
  ProgBar.Position     := 0;
  ProgLabel.Caption    := Msg;
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
      SetLogMessages(StateChange.LogMessages);
      OnAnalysisFinished(StateChange.Files, True);
    end;
    ascFailed: begin
      OnAnalysisFinished(StateChange.Files, False);
    end;
    ascCleared: begin
      ChangeActiveFile(FCurrentPath);
    end;
    ascUpdated: begin
      ChangeActiveFile(FCurrentPath);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);
begin
    UpdateAnalysisStatus(
      Format(
        'Idle (last analysis%s at %s)',
        [IfThen(Succeeded, '', 'failed'), FormatDateTime('h:nnam/pm', Now)]));
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
  if Button <> mbLeft then begin
    Exit;
  end;

  FResizing := True;
  FDragStartX := X;
  ResizeIndicatorPanel.Visible := True;
  ResizeIndicatorPanel.BoundsRect := SplitPanel.BoundsRect;
  ResizeIndicatorPanel.BringToFront;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
begin
  if not FResizing then begin
    Exit;
  end;

  ResizeIndicatorPanel.Left := SplitPanel.Left + X;
  ListView.Invalidate;
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
  if (Button <> mbLeft) or (not FResizing) then begin
    Exit;
  end;

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
  SelectedIndex := ListView.ItemIndex;
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
  CreationTimeString: string;
  CreationDateTime: TDateTime;
  TimeSinceCreation: TTimeSpan;
  ExtraInfo: string;
begin
  Result := Format('(%d, %d)', [Issue.StartLine, Issue.StartLineOffset]);

  if Issue.HasMetadata then begin
    if Issue.CreationDate <> '' then begin
      CreationTimeString := TRegEx.Replace(Issue.CreationDate, '([+-]\d{2})(\d{2})$', '$1:$2');
      CreationDateTime := ISO8601ToDate(CreationTimeString, False);
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

procedure TLintToolFrame.WebViewInitTimerTimer(Sender: TObject);
begin
  WebViewInitTimer.Enabled := False;

  if FBrowserHwnd <> RuleBrowser.Handle then begin
    // Docking or undocking the DelphiLint frame sometimes "detaches" the web view from the control, resulting in
    // a blank white control. The browser handle changes when the frame is docker or undocked, and it seems like
    // the only reliable indicator that this could have happened. There are many false positives with this approach,
    // but since the effect isn't too disruptive it's an acceptable solution.

    Log.Debug('Dirty check found change in handle (%x to %x) - initializing', [FBrowserHwnd, RuleBrowser.Handle]);
    FBrowserHwnd := RuleBrowser.Handle;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.DirtyWebView;
begin
  // Starting the IDE with a window layout including a docked DelphiLint can sometimes cause the web view to fail
  // to initialize (likely because the windows are not properly registered with Windows yet). This initializes the
  // web view after a short delay managed by the VCL event loop instead, circumventing this issue.
  //
  // Also, since DirtyWebView is called on frame resize, delaying a short period helps prevent excessive web view
  // reinitializations.

  WebViewInitTimer.Enabled := True;
end;

procedure TLintToolFrame.ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Issue: ILiveIssue;
  Rule: TRule;
  MaxImpactSeverity: TImpactSeverity;
  ImageRect: TRect;
  MessageRect: TRect;
  MessageTextHeight: Integer;
  MetaRect: TRect;
  MetaTextHeight: Integer;
  TextStartX: Integer;
  CurrentY: Integer;
  IssueMessageText: string;
  IssueMetaText: string;
begin
  DefaultDraw := False;

  Issue := FIssues[Item.Index].Get;

  if Item.Index >= FIssues.Count then
    Exit;

  if Item.Selected then
  begin
    Sender.Canvas.Brush.Color := clHighlight;
    Sender.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    Sender.Canvas.Brush.Color := TListView(Sender).Color;
    Sender.Canvas.Font.Color := TListView(Sender).Font.Color;
  end;
  Sender.Canvas.FillRect(Item.DisplayRect(drBounds));

  Rule := Analyzer.GetRule(Issue.RuleKey);
  if not Assigned(Rule) then begin
    Log.Warn('Rule "%s" could not be drawn', [Issue.RuleKey]);
    IssueImage.Picture.Graphic := nil;
  end
  else
  begin
    if Assigned(Rule.CleanCode) then
    begin
      MaxImpactSeverity := TArrayUtils.Max<TImpactSeverity>(Rule.CleanCode.Impacts.Values.ToArray, imsMedium);
      IssueImage.Picture.Graphic := LintResources.ImpactSeverityIcon(MaxImpactSeverity);
    end
    else
    begin
      IssueImage.Picture.Graphic := LintResources.RuleTypeIcon(Rule.RuleType, Rule.Severity);
    end;

    if Assigned(IssueImage.Picture.Graphic) then
    begin
      ImageRect := Rect(Item.DisplayRect(drBounds).Left + 2, Item.DisplayRect(drBounds).Top + 2,
                        Item.DisplayRect(drBounds).Left + IssueImage.Width + 2,
                        Item.DisplayRect(drBounds).Top + IssueImage.Height + 2);
      Sender.Canvas.Draw(ImageRect.Left, ImageRect.Top, IssueImage.Picture.Graphic);
    end;
  end;

  TextStartX := Item.DisplayRect(drBounds).Left + IssueImage.Width + 8;
  CurrentY := Item.DisplayRect(drBounds).Top + 2;

  Sender.Canvas.Font.Assign(IssueMessageLabel.Font);
  IssueMessageText := Issue.Message;
  MessageTextHeight := Sender.Canvas.TextHeight(IssueMessageText);

  MessageRect := Rect(TextStartX, CurrentY,
                      Item.DisplayRect(drBounds).Right - 4, CurrentY + MessageTextHeight);
  Sender.Canvas.TextRect(MessageRect, IssueMessageText, [tfLeft, tfSingleLine]);

  CurrentY := CurrentY + MessageTextHeight + 2;

  Sender.Canvas.Font.Assign(IssueMetaLabel.Font);
  IssueMetaText := GetIssueMetadataText(Issue);
  MetaTextHeight := Sender.Canvas.TextHeight(IssueMetaText);

  MetaRect := Rect(TextStartX, CurrentY,
                   Item.DisplayRect(drBounds).Right - 4, CurrentY + MetaTextHeight);
  Sender.Canvas.TextRect(MetaRect, IssueMetaText, [tfLeft, tfSingleLine]);

end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.IssueControlListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  Item: TListItem;
  Popup: TPopupMenu;
  Point: TPoint;
begin
  Handled := False;

  Item := ListView.GetItemAt(MousePos.X, MousePos.Y);
  if Assigned(Item) then
  begin
    Popup := CreateIssuePopup(Item.Index);
    if Assigned(Popup) then begin
      Point := ListView.ClientToScreen(MousePos);
      Popup.Popup(Point.X, Point.Y);
      Handled := True;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
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

procedure TLintToolFrame.MarqueeTimerTimer(Sender: TObject);
begin
  ProgBar.Position := ProgBar.Position + 1;
  if ProgBar.Position >= ProgBar.Max then
    ProgBar.Position := ProgBar.Min;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshActiveFile;
begin
  ChangeActiveFile(FCurrentPath);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RefreshIssueView;
begin
  FIssues.Clear;

  if FCurrentPath <> '' then begin
    FIssues.AddRange(TWrapper<ILiveIssue>.WrapArray(Analyzer.GetIssues(FCurrentPath)));
  end;

  ListView.Selected := nil;
  ListView.ItemIndex := -1;
  ListView.Items.Count := FIssues.Count;
  ListView.Repaint;

  RefreshRuleView;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserBeforeNavigate2(Sender: TObject; const pDisp: IDispatch;
  const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  UrlStr: string;
begin
  UrlStr := VarToStr(URL);

  Log.Debug('RuleBrowser navigating to: %s (NavigationAllowed: %s)', [UrlStr, BoolToStr(FNavigationAllowed, True)]);

  // Always allow navigation initiated by DelphiLint
  if FNavigationAllowed then begin
    Log.Debug('Navigation allowed by DelphiLint: %s', [UrlStr]);
    // Reset flag after allowing navigation to prevent interference with subsequent navigations
    FNavigationAllowed := False;
    Cancel := False;
    Exit;
  end;

  // For navigations not controlled by DelphiLint:
  // Allow navigation for:
  // - Local files (file:///)
  // - about: pages (about:blank, etc.)
  if StartsText('file:', UrlStr) or StartsText('about:', UrlStr) then begin
    Log.Debug('Allowing local/about navigation in RuleBrowser: %s', [UrlStr]);
    Cancel := False;
  end
  else begin
    // Open other URLs (http, https, etc.) in external browser
    Log.Info('External URL requested in rule webview, opening externally: %s', [UrlStr]);
    Cancel := True;
    ShellExecute(0, 'open', PChar(UrlStr), nil, nil, SW_SHOWNORMAL);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserNewWindow2(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
begin
  Log.Info('New window requested in rule webview, intercepting and cancelling');
  Cancel := True;
  ppDisp := nil;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserDocumentComplete(Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
begin
  Log.Debug('Rule browser document completed successfully for URL: %s', [VarToStr(URL)]);
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

procedure TLintToolFrame.SetLogMessages(LogMessages: TArray<string>);
var
  Log: string;
  ErrorCount: Integer;
  WarningCount: Integer;
begin
  FLastAnalysisLogs := LogMessages;
  FLastAnalysisTime := Now;

  ErrorCount := 0;
  WarningCount := 0;
  for Log in LogMessages do begin
    if StartsStr('[ERROR]', Log) then begin
      Inc(ErrorCount);
    end
    else if StartsStr('[WARN]', Log) then begin
      Inc(WarningCount);
    end;
  end;

  if ErrorCount > 0 then begin
    ErrorButton.Caption := Format('%d error%s', [ErrorCount, IfThen(ErrorCount = 1, '', 's')]);
  end
  else if WarningCount > 0 then begin
    WarningButton.Caption := Format('%d warning%s', [WarningCount, IfThen(WarningCount = 1, '', 's')]);
  end;

  ErrorButtonPanel.Visible := ErrorCount > 0;
  WarningButtonPanel.Visible := (ErrorCount = 0) and (WarningCount > 0);
  ViewLogItem.Enabled := Length(LogMessages) > 0;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.SetRuleView(Rule: TRule);
var
  HtmlFilePath: string;
begin
  if (not FRuleHtmls.ContainsKey(Rule.RuleKey)) or (not FileExists(FRuleHtmls[Rule.RuleKey])) then begin
    FRuleHtmls.AddOrSetValue(Rule.RuleKey, FRuleHtmlGenerator.GenerateHtmlFile(Rule));
  end;

  if FVisibleRule <> Rule.RuleKey then begin
    HtmlFilePath := FRuleHtmls[Rule.RuleKey];
    Log.Debug('SetRuleView: Navigating to %s for rule %s', [HtmlFilePath, Rule.RuleKey]);

    // Set flag before navigation so it is respected in BeforeNavigate2
    FNavigationAllowed := True;

    try
      RuleBrowser.Navigate('file:///' + NormalizePath(HtmlFilePath));
    except
      on E: EOleException do begin
        Log.Warn('OLE exception occurred during navigation: %s', [E.Message]);
        FNavigationAllowed := False; // Reset flag on error
        if E.Message <> 'Unspecified error' then begin
          raise;
        end;
      end;
    end;
    FVisibleRule := Rule.RuleKey;
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
