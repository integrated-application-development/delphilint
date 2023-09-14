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
  , Vcl.Graphics
  , Winapi.Windows
  , SHDocVw
  , DelphiLint.Data
  , DelphiLint.IDEBaseTypes
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

  TRuleHtmlGenerator = class(TObject)
  private
    FTextColor: string;
    FBgColor: string;
    FCodeBgColor: string;
    FLinkColor: string;

    function ColorToHex(Color: TColor): string;
    function WrapHtml(Html: string; WrappingTag: string): string;
    function ImageToBase64(Image: TGraphic): string;

    function GetRuleTypeStr(RuleType: TRuleType): string;
    function GetRuleSeverityStr(Severity: TRuleSeverity): string;
    function GetCleanCodeAttributeStr(Attribute: TCleanCodeAttribute): string;
    function GetCleanCodeCategoryStr(Category: TCleanCodeAttributeCategory): string;
    function GetSoftwareQualityStr(Quality: TSoftwareQuality): string;
    function GetImpactSeverityClassName(Severity: TImpactSeverity): string;
    function GetImpactTooltip(Quality: TSoftwareQuality; Severity: TImpactSeverity): string;
    function GetAttributeTooltip(Attribute: TCleanCodeAttribute): string;

    function GenerateCss: string;
    function BuildHtmlPage(BodyHtml: string; BodyClass: string = ''): string;
    function BuildTooltip(Html: string; Text: string = ''): string;
  public
    constructor Create;
    procedure UpdateColors;

    function GenerateHtmlText(Rule: TRule): string;
    function GenerateHtmlFile(Rule: TRule): string;
  end;

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
    RuleBrowser: TWebBrowser;
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure OnIssueSelected(Sender: TObject);
    procedure OnIssueDoubleClicked(Sender: TObject);
    procedure OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnMeasureIssueItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure RuleBrowserBeforeNavigate2(
      ASender: TObject;
      const PDisp: IDispatch;
      const URL: OleVariant;
      const Flags: OleVariant;
      const TargetFrameName: OleVariant;
      const PostData: OleVariant;
      const Headers: OleVariant;
      var Cancel: WordBool);
    procedure RuleBrowserNewWindow3(
      ASender: TObject;
      var PDisp: IDispatch;
      var Cancel: WordBool;
      Flags: Cardinal;
      const UrlContext: WideString;
      const Url: WideString);
  private const
    C_IssueStatusStrs: array[TIssueStatus] of string = (
      'Open',
      'Confirmed',
      'Reopened',
      'Resolved',
      'Closed',
      'To review',
      'Reviewed (acknowledged)'
    );
    C_IssueIconWidth = 16;
    C_IssuePadding = 4;
  private
    FResizing: Boolean;
    FDragStartX: Integer;
    FCurrentPath: string;
    FIssues: TArray<TLiveIssue>;
    FRuleHtmls: TDictionary<string, string>;
    FVisibleRule: string;
    FNavigationAllowed: Boolean;
    FRuleHtmlGenerator: TRuleHtmlGenerator;

    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    procedure RepaintIssueView;
    procedure GetIssueItemText(
      ListBox: TListBox;
      Issue: TLiveIssue;
      out LocationText: string;
      out MessageText: string;
      out MetadataText: string);

    procedure SetRuleView(Rule: TRule);

    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);

    procedure RefreshRuleView;

    function GetStatusCaption(Status: TCurrentFileStatus; NumIssues: Integer): string;
    procedure UpdateFileStatus(Status: TCurrentFileStatus; NumIssues: Integer = -1);
    procedure UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean = False);
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
  , System.NetEncoding
  , System.Win.ComObj
  , Vcl.Imaging.pngimage
  , Winapi.ShellAPI
  , Winapi.Messages
  , DelphiLint.Utils
  , DelphiLint.Resources
  , DelphiLint.Context
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

  Analyzer.OnAnalysisStarted.AddListener(OnAnalysisStarted);

  Analyzer.OnAnalysisComplete.AddListener(
    procedure(const Paths: TArray<string>) begin
      OnAnalysisFinished(Paths, True);
    end);

  Analyzer.OnAnalysisFailed.AddListener(
    procedure(const Paths: TArray<string>) begin
      OnAnalysisFinished(Paths, False);
    end);

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
end;

//______________________________________________________________________________________________________________________

destructor TLintToolFrame.Destroy;
var
  RuleKey: string;
begin
  for RuleKey in FRuleHtmls.Keys do begin
    TFile.Delete(FRuleHtmls[RuleKey]);
  end;

  FreeAndNil(FRuleHtmls);
  FreeAndNil(FRuleHtmlGenerator);
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
  History: TFileAnalysisHistory;
  FileScannable: Boolean;
begin
  FileScannable := IsFileInProject(Path);
  FCurrentPath := IfThen(FileScannable, Path, '');

  if FileScannable then begin
    if Analyzer.InAnalysis and Analyzer.CurrentAnalysis.IncludesFile(Path) then begin
      UpdateFileStatus(cfsInAnalysis);
      Exit;
    end;

    case Analyzer.GetAnalysisStatus(Path) of
      fasNeverAnalyzed:
          UpdateFileStatus(cfsNotAnalyzed);
      fasOutdatedAnalysis:
        if Analyzer.TryGetAnalysisHistory(Path, History) then begin
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
          Log.Warn('Could not get analysis history for file %s with apparently outdated analysis', [Path]);
          UpdateFileStatus(cfsNotAnalyzed);
        end;
      fasUpToDateAnalysis:
        if Analyzer.TryGetAnalysisHistory(Path, History) then begin
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
          Log.Warn('Could not get analysis history for file %s with apparently up-to-date analysis', [Path]);
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

  ProgImage.Picture.Graphic := LintResources.LintStatusIcon(Status);
  FileStatusLabel.Caption := GetStatusCaption(Status, NumIssues);
  RefreshIssueView;
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
  Issue: TLiveIssue;
  out LocationText: string;
  out MessageText: string;
  out MetadataText: string);
var
  CreationDateTime: TDateTime;
  TimeSinceCreation: TTimeSpan;
begin
  if Issue.Tethered then begin
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
        C_IssueStatusStrs[Issue.Status],
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
  Issue: TLiveIssue;
  Rect: TRect;
begin
  ListBox := Control as TListBox;
  // For some reason, OnMeasureItem is called after a string is added to the listbox, but before its corresponding
  // object is added. This means we have to maintain a separate list to be able to retrieve them by index here.
  Issue := FIssues[Index];

  GetIssueItemText(ListBox, Issue, LocationText, MessageText, MetadataText);

  Rect := TRect.Empty;
  Rect.Left := Rect.Left
    + C_IssuePadding
    + 2 * C_IssueIconWidth
    + ListBox.Canvas.TextWidth(LocationText);
  Rect.Right := ListBox.ClientRect.Right - C_IssuePadding;
  Rect.Top := Rect.Top + C_IssuePadding;
  Rect.Height := 0;

  DrawText(
    ListBox.Canvas.Handle,
    PChar(MessageText),
    Length(MessageText),
    Rect,
    DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

  Height := Rect.Height + C_IssuePadding + ListBox.Canvas.TextHeight(MetadataText) + C_IssuePadding;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  Issue: TLiveIssue;
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

  if (Index < 0) or (Index >= Length(FIssues)) then begin
    Exit;
  end;

  Issue := FIssues[Index];
  GetIssueItemText(ListBox, Issue, LocationText, MessageText, MetadataText);

  Canvas := ListBox.Canvas;
  Canvas.FillRect(Rect);

  Rule := Analyzer.GetRule(Issue.RuleKey);
  HasCleanCode := Assigned(Rule.CleanCode);
  if HasCleanCode then begin
    MaxImpactSeverity := TArrayUtils.Max<TImpactSeverity>(Rule.CleanCode.Impacts.Values.ToArray, imsMedium);

    Canvas.Draw(
      Rect.Left + C_IssuePadding,
      Rect.Top + C_IssuePadding + 1,
      LintResources.ImpactSeverityIcon(MaxImpactSeverity));

    TextLeft := Rect.Left + C_IssuePadding + C_IssueIconWidth;
  end
  else begin
    IssueType := Rule.RuleType;
    IssueSeverity := Rule.Severity;

    Canvas.Draw(
      Rect.Left + C_IssuePadding,
      Rect.Top + C_IssuePadding + 1,
      LintResources.RuleTypeIcon(IssueType));
    Canvas.Draw(
      Rect.Left + C_IssuePadding + C_IssueIconWidth,
      Rect.Top + C_IssuePadding + 1,
      LintResources.RuleSeverityIcon(IssueSeverity));

    TextLeft := Rect.Left + C_IssuePadding + 2 * C_IssueIconWidth;
  end;

  if not Issue.Tethered then begin
    Canvas.Font.Color := clGrayText;
  end;

  LocationWidth := Canvas.TextWidth(LocationText);
  Canvas.TextOut(
    TextLeft,
    Rect.Top + C_IssuePadding,
    LocationText);

  Canvas.Font.Style := [fsBold];

  MessageRect := TRect.Empty;
  MessageRect.Left := TextLeft + LocationWidth;
  MessageRect.Right := Rect.Right - C_IssuePadding;
  MessageRect.Top := Rect.Top + C_IssuePadding;
  MessageRect.Bottom := Rect.Bottom - C_IssuePadding - Canvas.TextHeight(MetadataText);

  DrawText(
    ListBox.Canvas.Handle,
    PChar(MessageText),
    Length(MessageText),
    MessageRect,
    DT_LEFT or DT_WORDBREAK);

  Canvas.Font.Style := [];

  Canvas.TextOut(Rect.Left + C_IssuePadding, MessageRect.Bottom, MetadataText);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.OnIssueDoubleClicked(Sender: TObject);
var
  SelectedIndex: Integer;
  SelectedIssue: TLiveIssue;
  Editor: IIDESourceEditor;
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
    FIssues := Analyzer.GetIssues(FCurrentPath);
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
    FRuleHtmls.AddOrSetValue(Rule.RuleKey, ExpandUNCFileName(FRuleHtmlGenerator.GenerateHtmlFile(Rule)));
  end;

  try
    FNavigationAllowed := True;
    if FVisibleRule <> Rule.RuleKey then begin
      try
        RuleBrowser.Navigate2(FRuleHtmls[Rule.RuleKey]);
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

procedure TLintToolFrame.RuleBrowserBeforeNavigate2(
  ASender: TObject;
  const PDisp: IDispatch;
  const URL: OleVariant;
  const Flags: OleVariant;
  const TargetFrameName: OleVariant;
  const PostData: OleVariant;
  const Headers: OleVariant;
  var Cancel: WordBool);
var
  UrlStr: string;
begin
  Cancel := not FRuleHtmls.ContainsValue(URL);
  if Cancel then begin
    Log.Info('New URL requested in rule webview, intercepting and showing externally');
    UrlStr := URL;
    ShellExecute(0, 'open', PChar(UrlStr), nil, nil, SW_SHOWNORMAL);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolFrame.RuleBrowserNewWindow3(
  ASender: TObject;
  var PDisp: IDispatch;
  var Cancel: WordBool;
  Flags: Cardinal;
  const UrlContext: WideString;
  const Url: WideString);
var
  UrlStr: string;
begin
  Log.Info('New window requested in rule webview, intercepting and showing externally');
  Cancel := True;
  UrlStr := Url;
  ShellExecute(0, 'open', PChar(UrlStr), nil, nil, SW_SHOWNORMAL);
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

//______________________________________________________________________________________________________________________

constructor TRuleHtmlGenerator.Create;
begin
  UpdateColors;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGenerator.UpdateColors;
begin
  FTextColor := ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnText));
  FBgColor := ColorToHex(LintContext.IDEServices.GetSystemColor(clWindow));
  FCodeBgColor := ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnFace));
  FLinkColor := ColorToHex(LintContext.IDEServices.GetSystemColor(clHotLight));
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateHtmlFile(Rule: TRule): string;
begin
  Result := TPath.GetTempFileName;
  TFile.WriteAllText(Result, GenerateHtmlText(Rule), TEncoding.UTF8);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateHtmlText(Rule: TRule): string;
var
  ImpactsHtml: string;
  BodyHtml: string;
  Quality: TSoftwareQuality;
  ImpactSeverity: TImpactSeverity;
begin
  if Assigned(Rule.CleanCode) then begin
    for Quality in Rule.CleanCode.Impacts.Keys do begin
      ImpactSeverity := Rule.CleanCode.Impacts[Quality];
      ImpactsHtml := ImpactsHtml + BuildTooltip(
        Format(
          '<span class="impact %s">%s <img src="%s"/></span>',
          [
            GetImpactSeverityClassName(ImpactSeverity),
            GetSoftwareQualityStr(Quality),
            ImageToBase64(LintResources.ImpactSeverityIcon(ImpactSeverity))
          ]),
        GetImpactTooltip(Quality, ImpactSeverity)
      );
    end;

    BodyHtml := Format(
      BuildTooltip(
        '<h2 class="cleancode">' +
        '  <strong>%s rule</strong> | %s' +
        '</h2>',
        GetAttributeTooltip(Rule.CleanCode.Attribute)
      ) +
      '<h1>%s</h1>' +
      '<hr/>' +
      '<div class="impacts">%s</div>' +
      '%s',
      [
        GetCleanCodeCategoryStr(Rule.CleanCode.Category),
        GetCleanCodeAttributeStr(Rule.CleanCode.Attribute),
        Rule.Name,
        ImpactsHtml,
        WrapHtml(Rule.Desc, 'p')
      ]);

    Result := BuildHtmlPage(BodyHtml, 'cleancode');
  end
  else begin
    BodyHtml := Format(
      '  <h1>%s</h1>' +
      '  <hr/>' +
      '  <h2><img src="%s"/>%s<span class="gap"></span><img src="%s"/>%s</h2>' +
      '  %s',
      [
        Rule.Name,
        ImageToBase64(LintResources.RuleTypeIcon(Rule.RuleType)),
        GetRuleTypeStr(Rule.RuleType),
        ImageToBase64(LintResources.RuleSeverityIcon(Rule.Severity)),
        GetRuleSeverityStr(Rule.Severity),
        WrapHtml(Rule.Desc, 'p')
      ]);

    Result := BuildHtmlPage(BodyHtml);
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.BuildHtmlPage(BodyHtml: string; BodyClass: string = ''): string;
begin
  Result := Format(
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge"/>' +
    '  <style>%s</style>' +
    '</head>' +
    '<body class="%s">%s</body>' +
    '</html>',
    [GenerateCss, BodyClass, BodyHtml]);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateCss: string;
begin
  Result := Format(
    'html {' +
    '  overflow-x: hidden;' +
    '  overflow-y: auto;' +
    '}' +
    'body {' +
    '  font-family: ''Segoe UI'', ''Helvetica Neue'', ''Helvetica'', ''Arial'', sans-serif;' +
    '  color: %s;' +
    '  background-color: %s;' +
    '  font-size: 12px;' +
    '  margin-top: 9px;' +
    '}' +
    'h1 { margin-top: 0px; font-size: 18px; margin-bottom: 0px; }' +
    'h2 {' +
    '  font-size: 12px;' +
    '  font-weight: normal;' +
    '  margin-top: -2px;' +
    '  margin-bottom: 1px;' +
    '}' +
    '.cleancode h2 { margin-top: 0; }' +
    'h2 .gap { display: inline-block; width: 10px; }' +
    'h2 img { display: inline; vertical-align: middle; margin-right: 2px; }' +
    'pre {' +
    '  background-color: %s;' +
    '  padding: 0.3em 0.5em;' +
    '  font-size: 1em;' +
    '  font-family: ''Consolas'', monospace;' +
    '}' +
    'a { color: %s; }' +
    '.tooltip-hover {' +
    '  position: relative;' +
    '}' +
    '.tooltip-content {' +
    '  display: none;' +
    '  position: absolute;' +
    '  top: 20px;' +
    '  left: 16px;' +
    '  width: 70vw;' +
    '  background-color: %s;' +
    '  padding: 6px 6px;' +
    '  z-index; 1;' +
    '  font-size: 12px;' +
    '}' +
    '.tooltip-hover:hover .tooltip-content {' +
    '  display: block;' +
    '}' +
    '.impacts {' +
    '  font-size: 11px;' +
    '  margin-bottom: 0;' +
    '}' +
    'p {' +
    '  margin-top: 10px;' +
    '}' +
    '.impact {' +
    '  display: inline-block;' +
    '  margin-right: 5px;' +
    '  padding: 3px 5px;' +
    '  border-radius: 12px;' +
    '}' +
    '.impact img {' +
    '  display: inline;' +
    '  vertical-align: middle;' +
    '}' +
    '.impact.low { color: rgb(49, 108, 146); background-color: rgb(233, 244, 251); }' +
    '.impact.medium { color: rgb(140, 94, 30); background-color: rgb(254, 245, 208); }' +
    '.impact.high { color: rgb(128, 27, 20); background-color: rgb(254, 228, 226); }',
    [FTextColor, FBgColor, FCodeBgColor, FLinkColor, FCodeBgColor]);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetAttributeTooltip(Attribute: TCleanCodeAttribute): string;
begin
  case Attribute of
    ccaFormatted:
      Result := 'This rule ensures code is presented systematically and regularly.';
    ccaConventional:
      Result := 'This rule ensures code consistently adheres to a single choice.';
    ccaIdentifiable:
      Result := 'This rule ensures names follow a regular structure based on language conventions.';
    ccaClear:
      Result := 'This rule ensures code is self-explanatory, transparently communicating its functionality.';
    ccaLogical:
      Result := 'This rule ensures code has well-formed and sound instructions that work together.';
    ccaComplete:
      Result := 'This rule ensures code constructs are comprehensive and used adequately and thoroughly.';
    ccaEfficient:
      Result := 'This rule ensures code utilizes resources without needless waste.';
    ccaFocused:
      Result := 'This rule ensures each code unit has a single, narrow, and specific scope.';
    ccaDistinct:
      Result := 'This rule ensures code procedures and data do not have unnecessary duplication.';
    ccaModular:
      Result := 'This rule ensures code has been organized to emphasize the separation between its parts.';
    ccaTested:
      Result := 'This rule ensures code has automated checks that provide confidence in the functionality.';
    ccaLawful:
      Result := 'This rule ensures licensing and copyright regulation is respected.';
    ccaTrustworthy:
      Result := 'This rule ensures code abstains from revealing or hard-coding private information.';
    ccaRespectful:
      Result := 'This rule ensures code refrains from using discriminatory and offensive language.';
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetCleanCodeAttributeStr(Attribute: TCleanCodeAttribute): string;
begin
  case Attribute of
    ccaFormatted: Result := 'Formatted';
    ccaConventional: Result := 'Conventional';
    ccaIdentifiable: Result := 'Identifiable';
    ccaClear: Result := 'Clear';
    ccaLogical: Result := 'Logical';
    ccaComplete: Result := 'Complete';
    ccaEfficient: Result := 'Efficient';
    ccaFocused: Result := 'Focused';
    ccaDistinct: Result := 'Distinct';
    ccaModular: Result := 'Modular';
    ccaTested: Result := 'Tested';
    ccaLawful: Result := 'Lawful';
    ccaTrustworthy: Result := 'Trustworthy';
    ccaRespectful: Result := 'Respectful';
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetCleanCodeCategoryStr(Category: TCleanCodeAttributeCategory): string;
begin
  case Category of
    cccConsistent: Result := 'Consistency';
    cccIntentional: Result := 'Intentionality';
    cccAdaptable: Result := 'Adaptability';
    cccResponsible: Result := 'Responsibility';
  end;
end;

function TRuleHtmlGenerator.GetImpactSeverityClassName(Severity: TImpactSeverity): string;
begin
  case Severity of
    imsLow: Result := 'low';
    imsMedium: Result := 'medium';
    imsHigh: Result := 'high';
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetImpactTooltip(Quality: TSoftwareQuality; Severity: TImpactSeverity): string;
var
  SeverityWord: string;
  QualityWord: string;
begin
  case Severity of
    imsLow: SeverityWord := 'low';
    imsMedium: SeverityWord := 'medium';
    imsHigh: SeverityWord := 'high';
  end;

  case Quality of
    sqaSecurity: QualityWord := 'security';
    sqaReliability: QualityWord := 'reliability';
    sqaMaintainability: QualityWord := 'maintainability';
  end;

  Result := Format(
    'This has a %s impact on the %s of your code.',
    [SeverityWord, QualityWord]);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetRuleSeverityStr(Severity: TRuleSeverity): string;
begin
  case Severity of
    rsInfo: Result := 'Info';
    rsMinor: Result := 'Minor';
    rsMajor: Result := 'Major';
    rsCritical: Result := 'Critical';
    rsBlocker: Result := 'Blocker';
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetRuleTypeStr(RuleType: TRuleType): string;
begin
  case RuleType of
    rtCodeSmell: Result := 'Code smell';
    rtBug: Result := 'Bug';
    rtVulnerability: Result := 'Vulnerability';
    rtSecurityHotspot: Result := 'Security hotspot';
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GetSoftwareQualityStr(Quality: TSoftwareQuality): string;
begin
  case Quality of
    sqaSecurity: Result := 'Security';
    sqaReliability: Result := 'Reliability';
    sqaMaintainability: Result := 'Maintainability';
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.BuildTooltip(Html: string; Text: string): string;
begin
  Result := Format('<span class="tooltip-hover"><span class="tooltip-content">%s</span>%s</span>', [Text, Html]);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.ImageToBase64(Image: TGraphic): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Image.SaveToStream(Stream);
    Result := Format('data:image/png;base64,%s', [TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size)]);
  finally
    FreeAndNil(Stream);
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.ColorToHex(Color: TColor): string;
begin
  Color := ColorToRGB(Color);
  Result := '#' +
    IntToHex(GetRValue(Color), 2) +
    IntToHex(GetGValue(Color), 2) +
    IntToHex(GetBValue(Color), 2);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.WrapHtml(Html: string; WrappingTag: string): string;
var
  OpeningTag: string;
  ClosingTag: string;
begin
  OpeningTag := Format('<%s>', [WrappingTag]);
  Result := Trim(Html);

  if not StartsText(OpeningTag, Result) then begin
    ClosingTag := Format('</%s>', [WrappingTag]);
    Result := Format('%s%s%s', [OpeningTag, Result, ClosingTag]);
  end;
end;

//______________________________________________________________________________________________________________________

end.
