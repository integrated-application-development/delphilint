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
  , DelphiLint.Handlers
  , DelphiLint.ToolFrame
  , DelphiLint.SettingsFrame
  , DelphiLint.OptionsForm
  , DelphiLint.Context
  , DelphiLint.Events
  ;

type
  TPluginCore = class(TDataModule)
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
    FEditor: TEditorHandler;
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
    procedure InitPluginInfo(IDEServices: IIDEServices);

    procedure OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
    procedure OnActiveFileChanged(const Path: string);

    procedure SetAnalysisActionsEnabled(Value: Boolean);

    procedure RefreshAnalysisActions;
    procedure SetPluginEnabled(Value: Boolean);

    procedure RemoveToolbarActions(IDEServices: IIDEServices);
  public
    constructor Create(Owner: TComponent); overload; override;
    constructor Create(Owner: TComponent; IDEServices: IIDEServices); reintroduce; overload;

    procedure OnRegister;
    procedure OnDeregister(IDEServices: IIDEServices);

    property AnalysisActionsEnabled: Boolean read FAnalysisActionsEnabled write SetAnalysisActionsEnabled;
    property PluginEnabled: Boolean read FEnabled write SetPluginEnabled;
    property Editor: TEditorHandler read FEditor;
  end;

  TIDEPlugin = class(TInterfacedObject, IPlugin)
  private
    FCore: TPluginCore;
  public
    procedure EnablePlugin;
    procedure DisablePlugin;
    function GetPluginEnabled: Boolean;
    function GetOnActiveFileChanged: TEventNotifier<string>;
    procedure Init;
    procedure Deinit(IDEServices: IIDEServices);

    constructor Create(IDEServices: IIDEServices);
    destructor Destroy; override;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    System.SysUtils
  , System.UITypes
  , Vcl.ComCtrls
  , Vcl.Dialogs
  , Winapi.Windows
  , DelphiLint.Utils
  , DelphiLint.SetupForm
  , DelphiLint.Version
  , DelphiLint.Resources
  , DelphiLint.LiveData
  ;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ActionAnalyzeActiveFileExecute(Sender: TObject);
var
  ProjectFile: string;
  SourceEditor: IIDESourceEditor;
begin
  if LintContext.Settings.ClientAutoShowToolWindow then begin
    ShowToolWindow;
  end;

  if not TryGetProjectFile(ProjectFile) then begin
    TaskMessageDlg(
      'DelphiLint cannot analyze the active file.',
      'There is no open Delphi project.',
      mtWarning,
      [mbOK],
      0);
  end
  else if not TryGetCurrentSourceEditor(SourceEditor) then begin
    TaskMessageDlg(
      'DelphiLint cannot analyze the active file.',
      'There are no open files that can be analyzed.',
      mtWarning,
      [mbOK],
      0);
  end
  else begin
    if LintContext.Settings.ClientSaveBeforeAnalysis then begin
      Log.Debug('Saving file before analysis');
      SourceEditor.Module.Save(True);
    end;
    Analyzer.AnalyzeFiles([SourceEditor.FileName, ProjectFile], ProjectFile);
    Exit;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ActionAnalyzeOpenFilesExecute(Sender: TObject);
var
  ProjectFile: string;
  Modules: TArray<IIDEModule>;
  Files: TArray<string>;
  Module: IIDEModule;
begin
  if LintContext.Settings.ClientAutoShowToolWindow then begin
    ShowToolWindow;
  end;

  if TryGetProjectFile(ProjectFile) then begin
    Modules := DelphiLint.Utils.GetOpenSourceModules;

    if LintContext.Settings.ClientSaveBeforeAnalysis then begin
      for Module in Modules do begin
        try
          Module.Save(True);
        except
          on E: Exception do begin
            Log.Warn('Module %s could not be saved', [Module.FileName]);
          end;
        end;
      end;
    end;

    Files := TArrayUtils.Map<IIDEModule, string>(
      Modules,
      function(Module: IIDEModule): string
      begin
        Result := Module.FileName;
      end);
    SetLength(Files, Length(Files) + 1);
    Files[Length(Files) - 1] := ProjectFile;

    if Length(Files) = 1 then begin
      TaskMessageDlg(
        'DelphiLint cannot analyze all open files.',
        'There are no open files that can be analyzed.',
        mtWarning,
        [mbOK],
        0);
      Exit;
    end;

    Analyzer.AnalyzeFiles(Files, ProjectFile);
  end
  else begin
    TaskMessageDlg(
      'DelphiLint cannot analyze all open files.',
      'There is no open Delphi project.',
      mtWarning,
      [mbOK],
      0);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ActionOpenProjectOptionsExecute(Sender: TObject);
begin
  if not Assigned(FOptionsForm) then begin
    FOptionsForm := TLintOptionsForm.Create(nil);
    FOptionsForm.RefreshTheme;
  end;

  FOptionsForm.ShowModal;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ActionOpenSettingsExecute(Sender: TObject);
begin
  LintContext.IDEServices.EditOptions('', 'DelphiLint');
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ActionRestartServerExecute(Sender: TObject);
begin
  Analyzer.RestartServer;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ActionShowToolWindowExecute(Sender: TObject);
begin
  ShowToolWindow;
  FToolForm.SetFocus;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.ShowToolWindow;
begin
  if not Assigned(FToolForm) then begin
    FToolForm := LintContext.IDEServices.CreateDockableForm(FToolFormInfo);
    LintContext.IDEServices.ApplyTheme(FToolForm);
  end;

  FToolForm.Show;
end;

//______________________________________________________________________________________________________________________

constructor TPluginCore.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  // Set plugin info
  InitPluginInfo(LintContext.IDEServices);
end;

//______________________________________________________________________________________________________________________

constructor TPluginCore.Create(Owner: TComponent; IDEServices: IIDEServices);
begin
  inherited Create(Owner);

  // Set plugin info
  InitPluginInfo(IDEServices);
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.OnRegister;
begin
  // Editor notifier
  FEditor := TEditorHandler.Create;
  FEditor.OnOwnerFreed.AddListener(
    procedure(const Notf: IIDEHandler) begin
      if Assigned(Self) then begin
        FEditor := nil;
      end;
    end);
  FEditorNotifier := LintContext.IDEServices.AddEditorNotifier(FEditor);
  FEditor.OnActiveFileChanged.AddListener(OnActiveFileChanged);

  // Main menu
  FActionListIndex := LintContext.IDEServices.AddActionListToIDEInsight(LintActions);
  CreateMainMenu;

  // Analysis callbacks
  Analyzer.OnAnalysisStateChanged.AddListener(OnAnalysisStateChanged);

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
  FEnabled := LintContext.ValidateSetup;

  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.OnDeregister(IDEServices: IIDEServices);
begin
  IDEServices.RemoveActionListFromIDEInsight(FActionListIndex);
  RemoveToolbarActions(IDEServices);
  FreeAndNil(FMainMenu);
  IDEServices.RemoveEditorNotifier(FEditorNotifier);
  IDEServices.UnregisterDockableForm(FToolFormInfo);
  IDEServices.RemovePluginInfo(FInfoIndex);
  IDEServices.UnregisterAddInOptions(FAddInOptions);
  FreeAndNil(FOptionsForm);
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
begin
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.CreateMainMenu;

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

procedure TPluginCore.InitPluginInfo(IDEServices: IIDEServices);
var
  VersionStr: string;
begin
  VersionStr := DelphiLintVersion;

  IDEServices.AddPluginBitmap(
    'DelphiLint',
    LintResources.DelphiLintSplash,
    False,
    'Code analyzer powered by SonarDelphi',
    VersionStr);

  IDEServices.AddPluginInfo(
    'DelphiLint ' + VersionStr,
    'Free and open source Delphi code linter, powered by the SonarDelphi code analysis tool for SonarQube.'
    + #13#10#13#10'Copyright © 2023 Integrated Application Development',
    LintResources.DelphiLintIcon);
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.SetAnalysisActionsEnabled(Value: Boolean);
begin
  FAnalysisActionsEnabled := Value;
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.SetPluginEnabled(Value: Boolean);
begin
  FEnabled := Value;
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.RefreshAnalysisActions;
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

procedure TPluginCore.RemoveToolbarActions(IDEServices: IIDEServices);

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
    'CustomToolBar', 'StandardToolBar', 'DebugToolBar', 'ViewToolBar', 'DesktopToolBar', 'AlignToolbar',
    'BrowserToolbar', 'HTMLDesignToolbar', 'HTMLFormatToolbar', 'HTMLTableToolbar', 'PersonalityToolBar',
    'PositionToolbar', 'SpacingToolbar', 'IDEInsightToolbar', 'PlatformDeviceToolbar'
  ];
var
  ToolBar: string;
begin
  for ToolBar in CToolBars do begin
    RemoveAction(ActionAnalyzeActiveFile, IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionAnalyzeOpenFiles, IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionShowToolWindow, IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionOpenProjectOptions, IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionOpenSettings, IDEServices.GetToolBar(ToolBar));
    RemoveAction(ActionRestartServer, IDEServices.GetToolBar(ToolBar));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPluginCore.OnActiveFileChanged(const Path: string);
begin
  AnalysisActionsEnabled := IsFileInProject(Path);
end;

//______________________________________________________________________________________________________________________

constructor TIDEPlugin.Create(IDEServices: IIDEServices);
begin
  inherited Create;
  FCore := TPluginCore.Create(nil, IDEServices);
end;

//______________________________________________________________________________________________________________________

destructor TIDEPlugin.Destroy;
begin
  FreeAndNil(FCore);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPlugin.EnablePlugin;
begin
  FCore.PluginEnabled := True;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPlugin.DisablePlugin;
begin
  FCore.PluginEnabled := False;
end;

//______________________________________________________________________________________________________________________

function TIDEPlugin.GetOnActiveFileChanged: TEventNotifier<string>;
begin
  Result := FCore.Editor.OnActiveFileChanged;
end;

//______________________________________________________________________________________________________________________

function TIDEPlugin.GetPluginEnabled: Boolean;
begin
  Result := FCore.PluginEnabled;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPlugin.Init;
begin
  FCore.OnRegister;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPlugin.Deinit(IDEServices: IIDEServices);
begin
  FCore.OnDeregister(IDEServices);
end;

//______________________________________________________________________________________________________________________

end.
