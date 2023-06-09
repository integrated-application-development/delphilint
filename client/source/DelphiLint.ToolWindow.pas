unit DelphiLint.ToolWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, DelphiLint.Plugin,
  Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus, Vcl.Buttons, Vcl.VirtualImage;

type
  TLintToolWindow = class(TDockableForm)
    LintPanel: TPanel;
    ProgLabel: TLabel;
    ProgBar: TProgressBar;
    FileNameLabel: TLabel;
    IssueTreeView: TTreeView;
    LintButton: TBitBtn;
    ProgImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure LintButtonClick(Sender: TObject);
  private const
    CNotAnalyzable = 'n/a';
    CNoFileSelected = '';
  private
    FCurrentPath: string;

    function IsFileScannable(const Path: string): Boolean;
    procedure UpdateFileNameLabel;
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

function TLintToolWindow.IsFileScannable(const Path: string): Boolean;

  function IsProjectFile: Boolean;
  var
    Project: IOTAProject;
    FileList: TStringList;
  begin
    Project := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
    FileList := TStringList.Create;
    try
      Project.GetCompleteFileList(FileList);
      Result := FileList.IndexOf(Path) <> -1;
    finally
      FreeAndNil(FileList);
    end;
  end;

begin
  Result := IsPasFile(Path) and IsProjectFile;
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
var
  Editor: IOTASourceEditor;
begin
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(Self);

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
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.UpdateFileNameLabel;
begin
  if FCurrentPath = CNoFileSelected then begin
    FileNameLabel.Caption := 'No file selected';
  end
  else if FCurrentPath = CNotAnalyzable then begin
    FileNameLabel.Caption := 'File not analyzable';
  end
  else begin
    FileNameLabel.Caption := TPath.GetFileName(FCurrentPath);
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
    LintButton.Enabled := True;

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
  end
  else begin
    FCurrentPath := CNotAnalyzable;
    AnalysisCleared;
    UpdateFileNameLabel;
    LintButton.Enabled := False;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisCleared;
begin
  Plugin.LintImages.GetIcon(C_ImgDefault, ProgImage.Picture.Icon);
  LintButton.Hint := 'Scan current file';
  ProgLabel.Caption := 'Not analyzed';
  ProgBar.Hide;
  Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisFailed;
begin
  Plugin.LintImages.GetIcon(C_ImgError, ProgImage.Picture.Icon);
  LintButton.Hint := 'Error occurred during analysis';
  ProgLabel.Caption := 'Failed';
  ProgBar.Hide;
  Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisStarted;
begin
  Plugin.LintImages.GetIcon(C_ImgWorking, ProgImage.Picture.Icon);
  LintButton.Hint := 'Analysis in progress';
  ProgLabel.Caption := 'Analyzing';
  ProgBar.Show;
  ProgBar.Style := TProgressBarStyle.pbstNormal;
  ProgBar.Style := TProgressBarStyle.pbstMarquee;
  Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean);
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
  LintButton.Hint := 'Analysis succeeded';
  ProgBar.Hide;
  Repaint;
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
