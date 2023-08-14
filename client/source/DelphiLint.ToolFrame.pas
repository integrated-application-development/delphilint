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
  , Winapi.Windows
  , SHDocVw
  , DockForm
  , DelphiLint.Data
  , DelphiLint.Context
  , DelphiLint.ToolsApiBase
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

    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    procedure RepaintIssueView;
    procedure GetIssueItemText(
      ListBox: TListBox;
      Issue: TLiveIssue;
      out LocationText: string;
      out MessageText: string;
      out MetadataText: string);


    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisFinished(const Paths: TArray<string>; const Succeeded: Boolean);

    procedure RefreshRuleView;
    procedure SetRuleView(Name: string; RuleKey: string; RuleType: TRuleType; Severity: TRuleSeverity; Desc: string);

    function GetStatusCaption(Status: TCurrentFileStatus; NumIssues: Integer): string;
    procedure UpdateFileStatus(Status: TCurrentFileStatus; NumIssues: Integer = -1);
    procedure UpdateAnalysisStatus(Msg: string; ShowProgress: Boolean = False);

    procedure CreateRuleHtml(
      Name: string;
      RuleKey: string;
      RuleType: TRuleType;
      Severity: TRuleSeverity;
      Desc: string
    );
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
  , Vcl.Themes
  , Vcl.Graphics
  , Vcl.Imaging.pngimage
  , Winapi.ShellAPI
  , Winapi.Messages
  , ToolsAPI
  , DelphiLint.Utils
  , DelphiLint.Logger
  , DelphiLint.Plugin
  , DelphiLint.Resources
  ;

{$R *.dfm}

constructor TLintToolFrame.Create(Owner: TComponent);
var
  Editor: IOTASourceEditor;
begin
  inherited Create(Owner);
  FResizing := False;
  FCurrentPath := '';
  FNavigationAllowed := False;
  FRuleHtmls := TDictionary<string, string>.Create;

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
var
  RuleKey: string;
begin
  for RuleKey in FRuleHtmls.Keys do begin
    TFile.Delete(FRuleHtmls[RuleKey]);
  end;

  FreeAndNil(FRuleHtmls);
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
          Log.Info('Could not get analysis history for file %s with apparently outdated analysis', [Path]);
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
          Log.Info('Could not get analysis history for file %s with apparently up-to-date analysis', [Path]);
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
begin
  ListBox := Control as TListBox;

  if (Index < 0) or (Index >= Length(FIssues)) then begin
    Exit;
  end;

  Issue := FIssues[Index];
  GetIssueItemText(ListBox, Issue, LocationText, MessageText, MetadataText);

  Canvas := ListBox.Canvas;
  Canvas.FillRect(Rect);

  Rule := LintContext.GetRule(Issue.RuleKey);
  if Assigned(Rule) then begin
    IssueType := Rule.RuleType;
    IssueSeverity := Rule.Severity;
  end
  else begin
    IssueType := rtCodeSmell;
    IssueSeverity := rsMajor;
  end;

  Canvas.Draw(
    Rect.Left + C_IssuePadding,
    Rect.Top + C_IssuePadding + 1,
    LintResources.RuleTypeIcon(IssueType));
  Canvas.Draw(
    Rect.Left + C_IssuePadding + C_IssueIconWidth,
    Rect.Top + C_IssuePadding + 1,
    LintResources.RuleSeverityIcon(IssueSeverity));
  TextLeft := Rect.Left + C_IssuePadding + 2 * C_IssueIconWidth;

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

procedure TLintToolFrame.CreateRuleHtml(Name: string; RuleKey: string; RuleType: TRuleType; Severity: TRuleSeverity;
  Desc: string);

  function ColorToHex(Color : TColor): string;
  begin
    Color := ColorToRGB(Color);
    Result := '#' +
      IntToHex(GetRValue(Color), 2) +
      IntToHex(GetGValue(Color), 2) +
      IntToHex(GetBValue(Color), 2);
  end;

  function Process(HtmlDesc: string): string;
  begin
    Result := Trim(HtmlDesc);
    if not StartsText(Result, '<p>') then begin
      Result := Format('<p>%s</p>', [Result]);
    end;
  end;

  function ImageToBase64(Image: TGraphic): string;
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

var
  TextColor: string;
  BgColor: string;
  CodeBgColor: string;
  LinkColor: string;
  Theme: TCustomStyleServices;
  HtmlStr: string;
  FileName: string;
begin
  Theme := (BorlandIDEServices as IOTAIDEThemingServices).StyleServices;
  TextColor := ColorToHex(Theme.GetSystemColor(clBtnText));
  BgColor := ColorToHex(Theme.GetSystemColor(clWindow));
  CodeBgColor := ColorToHex(Theme.GetSystemColor(clBtnFace));
  LinkColor := ColorToHex(Theme.GetSystemColor(clHotLight));

  HtmlStr := Format(
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <style>' +
    '    html {' +
    '      overflow-x: hidden;' +
    '      overflow-y: auto;' +
    '    }' +
    '    body {' +
    '      font-family: ''Segoe UI'', ''Helvetica Neue'', ''Helvetica'', ''Arial'', sans-serif;' +
    '      color: %s;' +
    '      background-color: %s;' +
    '      font-size: 12px;' +
    '    }' +
    '    h1 { margin-top: 0px; font-size: 18px; }' +
    '    h2 {' +
    '      margin-top: -10px;' +
    '      margin-bottom: -10px;' +
    '      font-size: 12px;' +
    '      font-weight: normal;' +
    '    }' +
    '    h2 span { display: inline-block; width: 10px; }' +
    '    h2 img { display: inline; vertical-align: middle; margin-right: 2px; }' +
    '    pre {' +
    '      background-color: %s;' +
    '      padding: 0.3em 0.5em;' +
    '      font-size: 1em;' +
    '      font-family: ''Consolas'', monospace;' +
    '    }' +
    '    a { color: %s; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <h1>%s</h1>' +
    '  <h2><img src="%s"/>%s<span></span><img src="%s"/>%s</h2>' +
    '  %s' +
    '</body>' +
    '</html>',
    [
      TextColor,
      BgColor,
      CodeBgColor,
      LinkColor,
      Name,
      ImageToBase64(LintResources.RuleTypeIcon(RuleType)),
      C_RuleTypeStrs[RuleType],
      ImageToBase64(LintResources.RuleSeverityIcon(Severity)),
      C_RuleSeverityStrs[Severity],
      Process(Desc)
    ]);

  FileName := TPath.GetTempFileName;
  TFile.WriteAllText(FileName, HtmlStr, TEncoding.UTF8);
  FRuleHtmls.Add(RuleKey, ExpandUNCFileName(FileName));
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
  if (not FRuleHtmls.ContainsKey(RuleKey)) or (not FileExists(FRuleHtmls[RuleKey])) then begin
    CreateRuleHtml(Name, RuleKey, RuleType, Severity, Desc);
  end;

  try
    FNavigationAllowed := True;
    if FVisibleRule <> RuleKey then begin
      RuleBrowser.Navigate2(FRuleHtmls[RuleKey]);
      FVisibleRule := RuleKey;
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
    Log.Info('External navigation requested, intercepting and showing externally');
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
  Log.Info('New window requested, intercepting and showing externally');
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
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(AFrame);
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

end.
