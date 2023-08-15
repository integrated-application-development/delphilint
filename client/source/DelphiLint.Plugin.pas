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
    System.Classes
  , System.ImageList
  , System.Actions
  , Vcl.ImgList
  , Vcl.Controls
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.Forms
  , DelphiLint.IDE
  , DelphiLint.ToolFrame
  , DelphiLint.SettingsFrame
  , DelphiLint.OptionsForm
  ;

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
    FActionListIndex: Integer;
    FMainMenu: TMenuItem;
    FAnalysisActionsEnabled: Boolean;
    FToolFormInfo: TLintToolFormInfo;
    FToolForm: TCustomForm;
    FAddInOptions: TLintAddInOptions;
    FInfoIndex: Integer;
    FOptionsForm: TLintOptionsForm;
    FEnabled: Boolean;

    procedure ShowToolWindow;

    procedure CreateMainMenu;
    procedure DestroyMainMenu;

    procedure InitPluginInfo;

    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisEnded(const Paths: TArray<string>);
    procedure OnActiveFileChanged(const Path: string);

    procedure SetAnalysisActionsEnabled(Value: Boolean);

    procedure RefreshAnalysisActions;
    procedure SetPluginEnabled(Value: Boolean);

    procedure RemoveToolbarActions;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterToolFrame(Frame: TLintToolFrame);
    procedure OnRegister;

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
    System.SysUtils
  , Vcl.ComCtrls
  , Winapi.Windows
  , ToolsAPI
  , DelphiLint.Analyzer
  , DelphiLint.ToolsApiBase
  , DelphiLint.Utils
  , DelphiLint.SetupForm
  , DelphiLint.Version
  , DelphiLint.Settings
  , DelphiLint.Resources
  ;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeActiveFileExecute(Sender: TObject);
begin
  if LintSettings.ClientAutoShowToolWindow then begin
    ShowToolWindow;
  end;
  Analyzer.AnalyzeActiveFile;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeOpenFilesExecute(Sender: TObject);
begin
  if LintSettings.ClientAutoShowToolWindow then begin
    ShowToolWindow;
  end;
  Analyzer.AnalyzeOpenFiles;
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
  Analyzer.RestartServer;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionShowToolWindowExecute(Sender: TObject);
begin
  ShowToolWindow;
  FToolForm.SetFocus;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ShowToolWindow;
begin
  if not Assigned(FToolForm) then begin
    FToolForm := (BorlandIDEServices as INTAServices).CreateDockableForm(FToolFormInfo);
    (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(FToolForm);
  end;

  FToolForm.Show;
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
  FActionListIndex := (BorlandIDEServices as INTAIDEInsightService).AddActionList(LintActions);
  CreateMainMenu;

  // Analysis callbacks
  Analyzer.OnAnalysisStarted.AddListener(OnAnalysisStarted);
  Analyzer.OnAnalysisComplete.AddListener(OnAnalysisEnded);
  Analyzer.OnAnalysisFailed.AddListener(OnAnalysisEnded);

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
  FEnabled := TLintSetupForm.TryFixSetup(False);

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
  if AnalyzerValid then begin
    Analyzer.OnAnalysisStarted.RemoveListener(OnAnalysisStarted);
    Analyzer.OnAnalysisComplete.RemoveListener(OnAnalysisEnded);
    Analyzer.OnAnalysisFailed.RemoveListener(OnAnalysisEnded);
  end;

  (BorlandIDEServices as INTAIDEInsightService).RemoveActionList(FActionListIndex);
  RemoveToolbarActions;
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
var
  NTAServices: INTAServices;

  procedure AddItem(Action: TAction);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    MenuItem.Action := Action;
    FMainMenu.Add(MenuItem);
    NTAServices.AddActionMenu('', Action, nil);
  end;

  procedure AddSeparator;
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    MenuItem.Caption := '-';
    FMainMenu.Add(MenuItem);
  end;

begin
  NTAServices := (BorlandIDEServices as INTAServices);
  NTAServices.AddImages(LintImages);

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
  VersionStr: string;
begin
  VersionStr := DelphiLintVersion;

  SplashScreenServices.AddPluginBitmap(
    'DelphiLint',
    LintResources.DelphiLintSplash.Handle,
    False,
    'Code analyzer powered by SonarDelphi',
    VersionStr);

  FInfoIndex := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(
    'DelphiLint ' + VersionStr,
    'Free and open source Delphi code linter, powered by the SonarDelphi code analysis tool for SonarQube.'
    + #13#10#13#10'Copyright © 2023 Integrated Application Development',
    LintResources.DelphiLintIcon.Handle);
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


  if FAnalysisActionsEnabled and (not Analyzer.InAnalysis) and PluginEnabled then begin
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

procedure TLintPlugin.RemoveToolbarActions;

  procedure RemoveAction(Action: TAction; ToolBar: TToolBar);
  var
    I: Integer;
    Button: TToolButton;
  begin
    for I := ToolBar.ButtonCount - 1 downto 0 do
    begin
      Button := ToolBar.Buttons[I];
      if Button.Action = Action then begin
        ToolBar.Perform(CM_CONTROLCHANGE, WPARAM(Button), 0);
        FreeAndNil(Button);
      end;
    end;
  end;

const
  CToolBars: array of string = [
    sCustomToolBar, sStandardToolBar, sDebugToolBar, sViewToolBar, sDesktopToolBar,
    sAlignToolbar, sBrowserToolbar, sHTMLDesignToolbar, sHTMLFormatToolbar, sHTMLTableToolbar, sPersonalityToolBar,
    sPositionToolbar, sSpacingToolbar, sIDEInsightToolbar, sPlatformDeviceToolbar
  ];
var
  NTAServices: INTAServices;
  ToolBar: string;
begin
  NTAServices := (BorlandIDEServices as INTAServices);

  for ToolBar in CToolBars do begin
    RemoveAction(ActionAnalyzeActiveFile, NTAServices.ToolBar[ToolBar]);
    RemoveAction(ActionAnalyzeOpenFiles, NTAServices.ToolBar[ToolBar]);
    RemoveAction(ActionShowToolWindow, NTAServices.ToolBar[ToolBar]);
    RemoveAction(ActionOpenProjectOptions, NTAServices.ToolBar[ToolBar]);
    RemoveAction(ActionOpenSettings, NTAServices.ToolBar[ToolBar]);
    RemoveAction(ActionRestartServer, NTAServices.ToolBar[ToolBar]);
  end;
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
