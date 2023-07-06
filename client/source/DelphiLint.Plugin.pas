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
unit DelphiLint.Plugin;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls, System.Actions, Vcl.ActnList, Vcl.Menus,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, DelphiLint.IDE, Vcl.Forms, DelphiLint.ToolFrame, DelphiLint.SettingsFrame,
  DelphiLint.OptionsForm;

const
  C_ImgDefault = 0;
  C_ImgSuccess = 1;
  C_ImgIssues = 2;
  C_ImgError = 3;
  C_ImgWorking = 4;
  C_ImgSuccessWarn = 5;
  C_ImgIssuesWarn = 6;

{$I dlversion.inc}

type
  TLintPlugin = class(TDataModule)
    LintImages: TImageList;
    LintActions: TActionList;
    ActionAnalyzeActiveFile: TAction;
    ActionShowToolWindow: TAction;
    ActionAnalyzeShort: TAction;
    ActionOpenProjectOptions: TAction;
    ActionOpenSettings: TAction;
    ActionAnalyzeOpenFiles: TAction;
    ActionRestartServer: TAction;
    procedure ActionShowToolWindowExecute(Sender: TObject);
    procedure ActionAnalyzeActiveFileExecute(Sender: TObject);
    procedure ActionRestartServerExecute(Sender: TObject);
    procedure ActionAnalyzeOpenFilesExecute(Sender: TObject);
    procedure ActionOpenSettingsExecute(Sender: TObject);
    procedure ActionOpenProjectOptionsExecute(Sender: TObject);
  private
    FEditor: TLintEditor;
    FEditorNotifier: Integer;
    FMainMenu: TMenuItem;
    FAnalysisActionsEnabled: Boolean;
    FToolFormInfo: TLintToolFormInfo;
    FToolForm: TCustomForm;
    FAddInOptions: TLintAddInOptions;
    FInfoIndex: Integer;
    FOptionsForm: TLintOptionsForm;
    FEnabled: Boolean;

    procedure CreateMainMenu;
    procedure DestroyMainMenu;

    procedure InitPluginInfo;

    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisEnded(const Paths: TArray<string>);
    procedure OnActiveFileChanged(const Path: string);

    procedure SetAnalysisActionsEnabled(Value: Boolean);

    procedure RefreshAnalysisActions;
    procedure SetPluginEnabled(Value: Boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterToolFrame(Frame: TLintToolFrame);
    procedure OnRegister;

    function BasicSetupComplete: Boolean;

    property AnalysisActionsEnabled: Boolean read FAnalysisActionsEnabled write SetAnalysisActionsEnabled;
    property PluginEnabled: Boolean read FEnabled write SetPluginEnabled;
  end;

procedure Register;

function Plugin: TLintPlugin;
var
  GPlugin: TLintPlugin;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    ToolsAPI
  , DelphiLint.Context
  , DelphiLint.ToolsApiBase
  , Winapi.Windows
  , Vcl.Graphics
  , DelphiLint.Utils
  , DelphiLint.Settings
  , DelphiLint.SetupForm
  ;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeActiveFileExecute(Sender: TObject);
begin
  LintContext.AnalyzeActiveFile;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeOpenFilesExecute(Sender: TObject);
begin
  LintContext.AnalyzeOpenFiles;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionOpenProjectOptionsExecute(Sender: TObject);
begin
  if not Assigned(FOptionsForm) then begin
    FOptionsForm := TLintOptionsForm.Create(nil);
    FOptionsForm.RefreshTheme;
  end;

  FOptionsForm.ShowModal;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionOpenSettingsExecute(Sender: TObject);
begin
  (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.EditOptions('', 'DelphiLint');
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionRestartServerExecute(Sender: TObject);
begin
  LintContext.RestartServer;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionShowToolWindowExecute(Sender: TObject);
begin
  if not Assigned(FToolForm) then begin
    FToolForm := (BorlandIDEServices as INTAServices).CreateDockableForm(FToolFormInfo);
    (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(FToolForm);
  end;

  FToolForm.Show;
  FToolForm.SetFocus;
end;

//______________________________________________________________________________________________________________________

function TLintPlugin.BasicSetupComplete: Boolean;
begin
  LintSettings.Load;
  Result := FileExists(LintSettings.ServerJavaExe)
    and FileExists(LintSettings.ServerJar)
    and FileExists(LintSettings.SonarDelphiJar);
end;

//______________________________________________________________________________________________________________________

function Plugin: TLintPlugin;
begin
  Result := GPlugin;
end;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  GPlugin.OnRegister;
end;

//______________________________________________________________________________________________________________________

constructor TLintPlugin.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  // Set plugin info
  InitPluginInfo;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnRegister;
var
  SetupForm: TLintSetupForm;
begin
  // Editor notifier
  FEditor := TLintEditor.Create;
  FEditor.OnOwnerFreed.AddListener(
    procedure(const Notf: TNotifierBase) begin
      if Assigned(Self) then begin
        FEditor := nil;
      end;
    end);
  FEditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(FEditor);

  // Main menu
  CreateMainMenu;

  // Analysis callbacks
  LintContext.OnAnalysisStarted.AddListener(OnAnalysisStarted);
  LintContext.OnAnalysisComplete.AddListener(OnAnalysisEnded);
  LintContext.OnAnalysisFailed.AddListener(OnAnalysisEnded);

  // Enable actions
  AnalysisActionsEnabled := True;

  // Initialise tool form
  FToolFormInfo := TLintToolFormInfo.Create;
  (BorlandIDEServices as INTAServices).RegisterDockableForm(FToolFormInfo);

  // Settings form
  FAddInOptions := TLintAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(FAddInOptions);

  // Project options form
  (BorlandIDEServices as IOTAIDEThemingServices).RegisterFormClass(TLintOptionsForm);

  // Setup form
  (BorlandIDEServices as IOTAIDEThemingServices).RegisterFormClass(TLintSetupForm);
  FEnabled := BasicSetupComplete;
  if not FEnabled then begin
    SetupForm := TLintSetupForm.Create(nil);
    SetupForm.RefreshTheme;
    SetupForm.ShowModal;
  end;

  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnAnalysisStarted(const Paths: TArray<string>);
begin
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnAnalysisEnded(const Paths: TArray<string>);
begin
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

destructor TLintPlugin.Destroy;
begin
  if LintContextValid then begin
    LintContext.OnAnalysisStarted.RemoveListener(OnAnalysisStarted);
    LintContext.OnAnalysisComplete.RemoveListener(OnAnalysisEnded);
    LintContext.OnAnalysisFailed.RemoveListener(OnAnalysisEnded);
  end;

  DestroyMainMenu;
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FEditorNotifier);
  (BorlandIDEServices as INTAServices).UnregisterDockableForm(FToolFormInfo);
  (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(FInfoIndex);
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(FAddInOptions);

  FreeAndNil(FOptionsForm);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.CreateMainMenu;

  procedure AddItem(Action: TAction);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    MenuItem.Action := Action;
    FMainMenu.Add(MenuItem);
  end;

  procedure AddSeparator;
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    MenuItem.Caption := '-';
    FMainMenu.Add(MenuItem);
  end;

var
  NTAServices: INTAServices;
begin
  NTAServices := (BorlandIDEServices as INTAServices);
  FMainMenu := TMenuItem.Create(NTAServices.MainMenu);
  FMainMenu.Caption := 'Delphi&Lint';

  AddItem(ActionShowToolWindow);
  AddSeparator;
  AddItem(ActionAnalyzeActiveFile);
  AddItem(ActionAnalyzeOpenFiles);
  AddSeparator;
  AddItem(ActionOpenProjectOptions);
  AddItem(ActionOpenSettings);
  AddItem(ActionRestartServer);

  NTAServices.AddActionMenu('ToolsMenu', nil, FMainMenu);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.DestroyMainMenu;
begin
  FreeAndNil(FMainMenu);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.InitPluginInfo;
var
  Icon: TBitmap;
  Handle: HBITMAP;
  VersionStr: string;
begin
  VersionStr := Format('%d.%d.%d', [C_DlMajorVersion, C_DlMinorVersion, C_DlPatchVersion]);
  if C_DlIsDevVersion then begin
    VersionStr := Format('%s.%s', [VersionStr, C_DlCommit]);
  end;

  Icon := TBitmap.Create(24, 24);
  try
    Icon.LoadFromResourceName(HInstance, 'DELPHILINT_SPLASH');
    Handle := Icon.Handle;

    SplashScreenServices.AddPluginBitmap(
      'DelphiLint',
      Handle,
      False,
      'Code analyzer powered by SonarDelphi',
      VersionStr);
  finally
    FreeAndNil(Icon);
  end;

  try
    Icon := TBitmap.Create(48, 48);
    Icon.LoadFromResourceName(HInstance, 'DELPHILINT_ICON');
    Handle := Icon.Handle;

    FInfoIndex := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(
      'DelphiLint ' + VersionStr,
      'Free and open source Delphi code linter, powered by the SonarDelphi code analysis tool for SonarQube.'
      + #13#10#13#10'Copyright © 2023 Integrated Application Development',
      Handle);
  finally
    FreeAndNil(Icon);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.SetAnalysisActionsEnabled(Value: Boolean);
begin
  FAnalysisActionsEnabled := Value;
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.SetPluginEnabled(Value: Boolean);
begin
  FEnabled := Value;
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.RefreshAnalysisActions;
var
  Index: Integer;
begin
  for Index := 0 to LintActions.ActionCount - 1 do begin
    LintActions[Index].Enabled := PluginEnabled;
  end;


  if FAnalysisActionsEnabled and (not LintContext.InAnalysis) and PluginEnabled then begin
    ActionAnalyzeActiveFile.Enabled := True;
    ActionAnalyzeShort.Enabled := True;
    ActionAnalyzeOpenFiles.Enabled := True;
  end
  else begin
    ActionAnalyzeActiveFile.Enabled := False;
    ActionAnalyzeShort.Enabled := False;
    ActionAnalyzeOpenFiles.Enabled := False;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.RegisterToolFrame(Frame: TLintToolFrame);
begin
  FEditor.OnActiveFileChanged.AddListener(Frame.ChangeActiveFile);
  FEditor.OnActiveFileChanged.AddListener(OnActiveFileChanged);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnActiveFileChanged(const Path: string);
begin
  AnalysisActionsEnabled := IsFileInProject(Path);
end;

//______________________________________________________________________________________________________________________

initialization
 GPlugin := TLintPlugin.Create(nil);

finalization
  FreeAndNil(GPlugin);

end.
