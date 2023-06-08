unit DelphiLint.ToolWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ComCtrls, System.ImageList, Vcl.ImgList, DelphiLint.Plugin,
  Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus;

type
  TLintToolWindow = class(TDockableForm)
    LintToolBar: TToolBar;
    LintButton: TToolButton;
    ProgPanel: TPanel;
    ProgLabel: TLabel;
    ProgBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure LintButtonClick(Sender: TObject);
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
end;

//______________________________________________________________________________________________________________________

procedure UnregisterDockableForm(var FormInstance: TLintToolWindow);
begin
  if @UnregisterFieldAddress <> nil then begin
    UnregisterFieldAddress(@FormInstance);
  end;
end;

//______________________________________________________________________________________________________________________

procedure CreateDockableForm(var FormInstance: TLintToolWindow);
begin
  FormInstance := TLintToolWindow.Create(nil);
  RegisterDockableForm(FormInstance);
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

constructor TLintToolWindow.Create(Owner: TComponent);
begin
  inherited;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
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

procedure TLintToolWindow.FormCreate(Sender: TObject);
begin
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

  AnalysisCleared;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.ChangeActiveFile(const Path: string);
var
  History: TFileAnalysisHistory;
begin
  Log.Info('Active file changed to ' + Path);
  if not LintContext.InAnalysis then begin
    Log.Info('Not in analysis, updating status');
    case LintContext.GetAnalysisStatus(Path) of
      fasNeverAnalyzed:
        begin
          Log.Info('File has never been analyzed, clearing.');
          AnalysisCleared;
        end;
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
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisCleared;
begin
  LintButton.ImageIndex := C_ImgDefault;
  LintButton.Hint := 'Scan current file';
  ProgLabel.Caption := 'Not analyzed';
  ProgBar.Style := TProgressBarStyle.pbstNormal;
  ProgBar.Position := 0;
  LintToolBar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisFailed;
begin
  LintButton.ImageIndex := C_ImgError;
  LintButton.Hint := 'Error occurred during analysis';
  ProgLabel.Caption := 'Failed';
  ProgBar.Style := TProgressBarStyle.pbstNormal;
  ProgBar.Position := 0;
  LintToolBar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisStarted;
begin
  LintButton.ImageIndex := C_ImgWorking;
  LintButton.Hint := 'Analysis in progress';
  ProgLabel.Caption := 'Analyzing';
  ProgBar.Style := TProgressBarStyle.pbstMarquee;
  ProgBar.Position := 50;
  LintToolBar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean);
begin
  if IssueCount = 0 then begin
    LintButton.ImageIndex := IfThen(Outdated, C_ImgSuccessWarn, C_ImgSuccess);
    ProgLabel.Caption := Format('No issues%s', [IfThen(Outdated, ' (outdated)', '')]);
  end
  else begin
    LintButton.ImageIndex := IfThen(Outdated, C_ImgIssuesWarn, C_ImgIssues);
    ProgLabel.Caption := Format('%d issues%s', [IssueCount,IfThen(Outdated, ' (outdated)', '')]);
  end;
  LintButton.Hint := 'Analysis succeeded';
  ProgBar.Style := TProgressBarStyle.pbstNormal;
  ProgBar.Position := 100;
  LintToolBar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.LintButtonClick(Sender: TObject);
begin
  LintContext.AnalyzeActiveFile;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  if Assigned(GToolWindow) then begin
    GToolWindow.RemoveInstance;
  end;

end.
