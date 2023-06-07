unit DelphiLint.ToolbarManager;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls, System.Actions, Vcl.ActnList, Vcl.Menus,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TLintToolbarManager = class(TDataModule)
    LintImages: TImageList;
    LintActions: TActionList;
    LintPopupMenu: TPopupMenu;
    ActionAnalyze: TAction;

    procedure DataModuleCreate(Sender: TObject);
    procedure ActionAnalyzeExecute(Sender: TObject);
  private const
    C_ImgDefault = 0;
    C_ImgSuccess = 1;
    C_ImgIssues = 2;
    C_ImgError = 3;
    C_ImgWorking = 4;
    C_ImgSuccessWarn = 5;
    C_ImgIssuesWarn = 6;
  private
    FToolbar: TToolBar;
    FImageIndexOffset: Integer;
    FProgBar: TProgressBar;
    FProgLabel: TLabel;
    FLintButton: TToolButton;

    procedure ResetToolbar(Toolbar: TToolBar);
    procedure CreateToolbar;
    procedure CreateMenuItem;
    function CreateLintMenu(Owner: TComponent): TPopupMenu;
  public
    procedure AnalysisStarted;
    procedure AnalysisFailed;
    procedure AnalysisCleared;
    procedure AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean = False);

    procedure ActiveFileChanged(const Path: string);
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    ToolsAPI
  , Vcl.Forms
  , DelphiLint.Plugin
  , System.Math
  , DelphiLint.Logger
  , System.StrUtils
  ;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.ActiveFileChanged(const Path: string);
var
  History: TFileAnalysisHistory;
begin
  if not Plugin.InAnalysis then begin
    case Plugin.GetAnalysisStatus(Path) of
      fasNeverAnalyzed:
        begin
          Log.Info('File has never been analyzed, clearing.');
          AnalysisCleared;
        end;
      fasOutdatedAnalysis:
        if Plugin.TryGetAnalysisHistory(Path, History) then begin
          AnalysisSucceeded(History.IssuesFound, True);
        end
        else begin
          Log.Info('Could not get analysis history for file ' + Path + ' with apparently outdated analysis.');
          AnalysisCleared;
        end;
      fasUpToDateAnalysis:
        if Plugin.TryGetAnalysisHistory(Path, History) then begin
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

procedure TLintToolbarManager.AnalysisCleared;
begin
  FLintButton.ImageIndex := FImageIndexOffset + C_ImgDefault;
  FLintButton.Hint := 'Scan current file';
  FProgLabel.Caption := 'Not analyzed';
  FProgBar.Style := TProgressBarStyle.pbstNormal;
  FProgBar.Position := 0;
  FToolbar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.AnalysisFailed;
begin
  FLintButton.ImageIndex := FImageIndexOffset + C_ImgError;
  FLintButton.Hint := 'Error occurred during analysis';
  FProgLabel.Caption := 'Failed';
  FProgBar.Style := TProgressBarStyle.pbstNormal;
  FProgBar.Position := 0;
  FToolbar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.AnalysisStarted;
begin
  FLintButton.ImageIndex := FImageIndexOffset + C_ImgWorking;
  FLintButton.Hint := 'Analysis in progress';
  FProgLabel.Caption := 'Analyzing';
  FProgBar.Style := TProgressBarStyle.pbstMarquee;
  FProgBar.Position := 50;
  FToolbar.Repaint;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.AnalysisSucceeded(IssueCount: Integer; Outdated: Boolean = False);
begin
  if IssueCount = 0 then begin
    FLintButton.ImageIndex := FImageIndexOffset + IfThen(Outdated, C_ImgSuccessWarn, C_ImgSuccess);
    FProgLabel.Caption := Format('No issues%s', [IssueCount, IfThen(Outdated, ' (outdated)', '')]);
  end
  else begin
    FLintButton.ImageIndex := FImageIndexOffset + IfThen(Outdated, C_ImgIssuesWarn, C_ImgIssues);
    FProgLabel.Caption := Format('%d issues%s', [IssueCount,IfThen(Outdated, ' (outdated)', '')]);
  end;
  FLintButton.Hint := 'Analysis succeeded';
  FProgBar.Style := TProgressBarStyle.pbstNormal;
  FProgBar.Position := 100;
  FToolbar.Repaint;
end;

//______________________________________________________________________________________________________________________

function TLintToolbarManager.CreateLintMenu(Owner: TComponent): TPopupMenu;

  procedure CreateItem(Menu: TPopupMenu; Caption: string; Action: TAction);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := Menu.CreateMenuItem;
    MenuItem.Caption := Caption;
    MenuItem.Action := Action;
    Menu.Items.Add(MenuItem);
  end;

begin
  Result := TPopupMenu.Create(Owner);
  CreateItem(Result, '', ActionAnalyze);
  CreateItem(Result, 'Analyze Open Files', nil);
  CreateItem(Result, '-', nil);
  CreateItem(Result, 'Restart Server', nil);
  CreateItem(Result, '-', nil);
  CreateItem(Result, 'Project Options', nil);
  CreateItem(Result, 'Settings', nil);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.CreateMenuItem;
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(FToolbar);
  MenuItem.Caption := 'Analyze';
  MenuItem.Action := ActionAnalyze;
  (BorlandIDEServices as INTAServices).AddActionMenu('FileExitItem', ActionAnalyze, MenuItem, False);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.CreateToolbar;
var
  ProgPanel: TPanel;
begin
  FToolbar := (BorlandIDEServices as INTAServices).NewToolbar('DelphiLintToolbar', 'DelphiLint');
  ResetToolbar(FToolbar);
  FToolbar.ShowCaptions := False;
  FToolbar.AutoSize := True;

  ProgPanel := TPanel.Create(FToolbar);
  ProgPanel.Parent := FToolbar;
  ProgPanel.ShowCaption := False;
  ProgPanel.VerticalAlignment := taAlignTop;
  ProgPanel.BorderStyle := bsNone;
  ProgPanel.BevelKind := bkNone;
  ProgPanel.BevelOuter := bvNone;
  ProgPanel.BevelInner := bvNone;
  ProgPanel.Width := 100;
  ProgPanel.Height := 24;
  ProgPanel.ParentBackground := False;

  FLintButton := TToolButton.Create(FToolbar);
  FLintButton.Parent := FToolbar;
  FLintButton.Action := ActionAnalyze;
  FLintButton.ImageIndex := FImageIndexOffset + 0;
  FLintButton.Style := TToolButtonStyle.tbsDropDown;
  FLintButton.DropdownMenu := CreateLintMenu(FToolbar);
  FLintButton.AutoSize := True;

  FProgBar := TProgressBar.Create(ProgPanel);
  FProgBar.Parent := ProgPanel;
  FProgBar.Min := 0;
  FProgBar.Max := 100;
  FProgBar.Step := 1;
  FProgBar.Width := 98;
  FProgBar.Left := 2;
  FProgBar.Top := 16;
  FProgBar.Height := 10;

  FProgLabel := TLabel.Create(ProgPanel);
  FProgLabel.Parent := ProgPanel;
  FProgLabel.Caption := 'Idle';
  FProgLabel.Left := 2;
  FProgLabel.Top := 2;
  FProgLabel.Width := 100;
  FProgLabel.Height := 16;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.DataModuleCreate(Sender: TObject);
var
  NTAServices: INTAServices;
begin
  NTAServices := BorlandIDEServices as INTAServices;
  FImageIndexOffset := NTAServices.AddImages(LintImages);
  CreateToolbar;
  CreateMenuItem;
  AnalysisCleared;

  Plugin.OnAnalysisComplete.AddListener(
    procedure(const Paths: TArray<string>)
    var
      History: TFileAnalysisHistory;
    begin
      if Plugin.TryGetAnalysisHistory(Paths[0], History) then begin
        AnalysisSucceeded(History.IssuesFound, False);
      end;
    end);

  Plugin.OnAnalysisFailed.AddListener(
    procedure(const Paths: TArray<string>) begin
      AnalysisFailed;
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.ResetToolbar(Toolbar: TToolBar);
var
  Index: Integer;
  Button: TToolButton;
begin
  for Index := Toolbar.ButtonCount - 1 downto 0 do begin
    Button := Toolbar.Buttons[Index];
    Toolbar.RemoveControl(Button);
    FreeAndNil(Button);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolbarManager.ActionAnalyzeExecute(Sender: TObject);
begin
  AnalysisStarted;
  Plugin.AnalyzeActiveFile;
end;

//______________________________________________________________________________________________________________________

end.
