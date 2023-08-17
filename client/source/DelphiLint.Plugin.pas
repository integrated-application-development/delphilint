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
  , DelphiLint.Context
  , DelphiLint.IDEBaseTypes
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
  LintContext.IDEServices.EditOptions('', 'DelphiLint');
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
    FToolForm := LintContext.IDEServices.CreateDockableForm(FToolFormInfo);
    LintContext.IDEServices.ApplyTheme(FToolForm);
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
  FEditorNotifier := LintContext.IDEServices.AddEditorNotifier(FEditor);

  // Main menu
  FActionListIndex := LintContext.IDEServices.AddActionListToIDEInsight(LintActions);
  CreateMainMenu;

  // Analysis callbacks
  Analyzer.OnAnalysisStarted.AddListener(OnAnalysisStarted);
  Analyzer.OnAnalysisComplete.AddListener(OnAnalysisEnded);
  Analyzer.OnAnalysisFailed.AddListener(OnAnalysisEnded);

  // Enable actions
  AnalysisActionsEnabled := True;

  // Initialise tool form
  FToolFormInfo := TLintToolFormInfo.Create;
  LintContext.IDEServices.RegisterDockableForm(FToolFormInfo);

  // Settings form
  FAddInOptions := TLintAddInOptions.Create;
  LintContext.IDEServices.RegisterAddInOptions(FAddInOptions);

  // Project options form
  LintContext.IDEServices.RegisterFormClass(TLintOptionsForm);

  // Setup form
  LintContext.IDEServices.RegisterFormClass(TLintSetupForm);
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
  if ContextValid then begin
    Analyzer.OnAnalysisStarted.RemoveListener(OnAnalysisStarted);
    Analyzer.OnAnalysisComplete.RemoveListener(OnAnalysisEnded);
    Analyzer.OnAnalysisFailed.RemoveListener(OnAnalysisEnded);
  end;

  LintContext.IDEServices.RemoveActionListFromIDEInsight(FActionListIndex);
  RemoveToolbarActions;
  DestroyMainMenu;
  LintContext.IDEServices.RemoveEditorNotifier(FEditorNotifier);
  LintContext.IDEServices.UnregisterDockableForm(FToolFormInfo);
  LintContext.IDEServices.RemovePluginInfo(FInfoIndex);
  LintContext.IDEServices.UnregisterAddInOptions(FAddInOptions);

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
    LintContext.IDEServices.AddAction(Action);
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
  LintContext.IDEServices.AddImages(LintImages);

  FMainMenu := TMenuItem.Create(LintContext.IDEServices.GetMainMenu);
  FMainMenu.Caption := 'Delphi&Lint';

  AddItem(ActionShowToolWindow);
  AddSeparator;
  AddItem(ActionAnalyzeActiveFile);
  AddItem(ActionAnalyzeOpenFiles);
  AddSeparator;
  AddItem(ActionOpenProjectOptions);
  AddItem(ActionOpenSettings);
  AddItem(ActionRestartServer);

  LintContext.IDEServices.AddMenu('ToolsMenu', FMainMenu);
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

  LintContext.IDEServices.AddPluginBitmap(
    'DelphiLint',
    LintResources.DelphiLintSplash,
    False,
    'Code analyzer powered by SonarDelphi',
    VersionStr);

  FInfoIndex := LintContext.IDEServices.AddPluginInfo(
    'DelphiLint ' + VersionStr,
    'Free and open source Delphi code linter, powered by the SonarDelphi code analysis tool for SonarQube.'
    + #13#10#13#10'Copyright © 2023 Integrated Application Development',
    LintResources.DelphiLintIcon);
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
    'CustomToolBar', 'StandardToolBar', 'DebugToolBar', 'ViewToolBar', 'DesktopToolBar',
    'AlignToolbar', 'BrowserToolbar', 'HTMLDesignToolbar', 'HTMLFormatToolbar', 'HTMLTableToolbar', 'PersonalityToolBar',
    'PositionToolbar', 'SpacingToolbar', 'IDEInsightToolbar', 'PlatformDeviceToolbar'
  ];
var
  ToolBar: string;
begin
  for ToolBar in CToolBars do begin
    RemoveAction(ActionAnalyzeActiveFile, LintContext.IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionAnalyzeOpenFiles, LintContext.IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionShowToolWindow, LintContext.IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionOpenProjectOptions, LintContext.IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionOpenSettings, LintContext.IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionRestartServer, LintContext.IDEServices.GetToolBar(ToolBar));
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
