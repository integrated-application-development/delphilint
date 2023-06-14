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
  DelphiLint.ToolFrame;

type
  TLintToolWindow = class(TDockableForm)
  private
    FCurrentPath: string;
    FFrame: TLintToolFrame;

    function IsFileScannable(const Path: string): Boolean;
    procedure UpdateFileNameLabel(NewText: string = '');

    procedure RefreshIssueView;
    procedure OnDrawIssueItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
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
  , DelphiLint.IDEUtils
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

  FFrame := TLintToolFrame.Create(Self);
  FFrame.Parent := Self;
  FFrame.Align := alClient;

  FFrame.IssueListBox.OnDrawItem := OnDrawIssueItem;

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

  LocationText := Format('(%d, %d) ', [Issue.StartLine, Issue.StartLineOffset]);
  LocationWidth := Canvas.TextWidth(LocationText);
  Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, LocationText);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(Rect.Left + 4 + LocationWidth, Rect.Top + 4, Issue.Message);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.RefreshIssueView;
var
  Issues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
begin
  FFrame.IssueListBox.Clear;

  if FCurrentPath <> '' then begin
    Issues := LintContext.GetIssues(FCurrentPath);
    for Issue in Issues do begin
      FFrame.IssueListBox.AddItem(Format('%d: %s', [Issue.StartLine, Issue.Message]), Issue);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  if Assigned(GToolWindow) then begin
    GToolWindow.RemoveInstance;
  end;

end.
