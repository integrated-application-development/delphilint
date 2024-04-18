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
unit DelphiLintTest.MockContext;

interface

uses
    System.Generics.Collections
  , System.Classes
  , System.UITypes
  , Vcl.Forms
  , Vcl.Themes
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.ComCtrls
  , Vcl.Graphics
  , Vcl.Controls
  , DelphiLint.IDEBaseTypes
  , DelphiLint.Context
  , DelphiLint.Events
  , DelphiLint.Data
  , DelphiLint.LiveData
  , DelphiLint.Settings
  , DelphiLint.ProjectOptions
  , DelphiLintTest.MockUtils
  ;

type
  TAnalyzerCallType = (
    azcAnalyzeFiles,
    azcRestartServer,
    azcUpdateIssueLine,
    azcClearFile
  );

  TMockAnalyzer = class(THookedObject<TAnalyzerCallType>, IAnalyzer)
  private
    FOnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext>;

    FFileHistories: TDictionary<string, TFileAnalysisHistory>;
    FFileStatuses: TDictionary<string, TFileAnalysisStatus>;
    FIssues: TObjectDictionary<string, TDictionary<Integer, ILiveIssue>>;
    FRules: TObjectDictionary<string, TRule>;
    FCurrentAnalysis: TCurrentAnalysis;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MockFileHistory(Path: string; History: TFileAnalysisHistory);
    procedure MockFileIssues(Path: string; Issues: TArray<ILiveIssue>); overload;
    procedure MockFileIssues(Path: string; Issues: TObjectDictionary<Integer, ILiveIssue>); overload;
    procedure MockFileIssue(Path: string; Line: Integer; Issue: ILiveIssue); overload;
    procedure MockFileStatus(Path: string; Status: TFileAnalysisStatus);
    procedure MockRule(RuleKey: string; Rule: TRule);
    procedure MockCurrentAnalysis(CurrentAnalysis: TCurrentAnalysis);

    function GetOnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext>;
    function GetCurrentAnalysis: TCurrentAnalysis;
    function GetInAnalysis: Boolean;
    function GetIssues(FileName: string; Line: Integer = -1): TArray<ILiveIssue>;
    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeFiles(const Paths: TArray<string>; const ProjectFile: string);
    procedure ClearFile(const FileName: string);
    procedure RestartServer;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
  end;

  TMockIDEObject<T> = class(THookedObject<T>)
  public
    function Raw: IInterface;
  end;

  TMockIDEObject = class(TInterfacedObject)
  public
    function Raw: IInterface;
  end;

  TEditViewCallType = (
    evcPaint,
    evcGoToPosition
  );

  TMockEditView = class(TMockIDEObject<TEditViewCallType>, IIDEEditView)
  private
    FFileName: string;
    FLineTracker: IIDEEditLineTracker;
    FNotifiers: TDictionary<Integer, IIDEViewHandler>;
    FNextId: Integer;
    FLeftColumn: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint;
    procedure GoToPosition(const Line: Integer; const Column: Integer);
    function GetFileName: string;
    function GetLineTracker: IIDEEditLineTracker;
    function AddNotifier(Notifier: IIDEViewHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    function GetLeftColumn: Integer;

    property MockedFileName: string read FFileName write FFileName;
    property MockedLineTracker: IIDEEditLineTracker read FLineTracker write FLineTracker;
    property MockedNotifiers: TDictionary<Integer, IIDEViewHandler> read FNotifiers write FNotifiers;
    property MockedLeftColumn: Integer read FLeftColumn write FLeftColumn;
  end;

  TMockSourceEditor = class(TMockIDEObject, IIDESourceEditor)
  private
    FModule: IIDEModule;
    FEditViews: TList<IIDEEditView>;
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModule: IIDEModule;
    function GetEditViewCount: Integer;
    function GetEditView(Index: Integer): IIDEEditView;
    function GetFileName: string;

    property MockedModule: IIDEModule read FModule write FModule;
    property MockedEditViews: TList<IIDEEditView> read FEditViews write FEditViews;
    property MockedFileName: string read FFileName write FFileName;
  end;

  TMockProject = class(TMockIDEObject, IIDEProject)
  private
    FFileList: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetCompleteFileList(FileList: TStrings);
    property MockedFileList: TList<string> read FFileList write FFileList;
  end;

  TModuleCallType = (mdcSave);

  TMockModule = class(TMockIDEObject<TModuleCallType>, IIDEModule)
  private
    FSourceEditor: IIDESourceEditor;
    FFileName: string;
  public
    function GetSourceEditor: IIDESourceEditor;
    procedure Save(const ForceSave: Boolean);
    function GetFileName: string;

    property MockedSourceEditor: IIDESourceEditor read FSourceEditor write FSourceEditor;
    property MockedFileName: string read FFileName write FFileName;
  end;

  TMockEditLineTracker = class(TMockIDEObject, IIDEEditLineTracker)
  private
    FFileName: string;
    FNotifiers: TDictionary<Integer, IIDEEditLineHandler>;
    FLines: TDictionary<Integer, Integer>;
    FNextId: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetFileName: string;
    procedure Clear;
    function AddNotifier(Notifier: IIDEEditLineHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure AddLine(const Line: Integer; const Value: Integer);

    property MockedFileName: string read FFileName write FFileName;
    property MockedNotifiers: TDictionary<Integer, IIDEEditLineHandler> read FNotifiers write FNotifiers;
    property MockedLines: TDictionary<Integer, Integer> read FLines write FLines;
  end;

  TIDEServicesCallType = (
    iscAddImages,
    iscApplyTheme,
    iscCreateDockableForm,
    iscEditOptions,
    iscRegisterAddInOptions,
    iscUnregisterAddInOptions,
    iscRegisterDockableForm,
    iscUnregisterDockableForm,
    iscRegisterFormClass
  );

  TMockPluginInfo = class(TObject)
  private
    FTitle: string;
    FDescription: string;
    FIcon: TBitmap;
  public
    constructor Create(Title: string; Description: string; Icon: TBitmap);
    destructor Destroy; override;

    property Title: string read FTitle;
    property Description: string read FDescription;
    property Icon: TBitmap read FIcon;
  end;

  TMockIDE = class(TObject)
  private
    FActions: TList<TCustomAction>;
    FIDEInsightActions: TList<TCustomActionList>;
    FEditorNotifiers: TDictionary<Integer, IIDEEditorHandler>;
    FMenus: TObjectDictionary<string, TMenuItem>;
    FPluginInfos: TObjectDictionary<Integer, TMockPluginInfo>;
    FPluginTitle: string;
    FPluginIcon: TBitmap;
    FPluginVersion: string;
    FPluginLicenseStatus: string;
    FPluginRegistered: Boolean;
    FMainMenu: TMainMenu;
  public
    constructor Create;
    destructor Destroy; override;

    property Actions: TList<TCustomAction> read FActions;
    property IDEInsightActions: TList<TCustomActionList> read FIDEInsightActions;
    property EditorNotifiers: TDictionary<Integer, IIDEEditorHandler> read FEditorNotifiers;
    property Menus: TObjectDictionary<string, TMenuItem> read FMenus;
    property PluginInfos: TObjectDictionary<Integer, TMockPluginInfo> read FPluginInfos;

    property PluginTitle: string read FPluginTitle write FPluginTitle;
    property PluginIcon: TBitmap read FPluginIcon write FPluginIcon;
    property PluginVersion: string read FPluginVersion write FPluginVersion;
    property PluginLicenseStatus: string read FPluginLicenseStatus write FPluginLicenseStatus;
    property PluginRegistered: Boolean read FPluginRegistered write FPluginRegistered;

    property MainMenu: TMainMenu read FMainMenu write FMainMenu;
  end;

  TMockIDEServices = class(THookedObject<TIDEServicesCallType>, IIDEServices)
  private
    FIDE: TMockIDE;
    FNextId: Integer;
    FMacros: TDictionary<string, string>;
    FRootDirectory: string;
    FStyleColors: TDictionary<TStyleColor, TColor>;
    FSystemColors: TDictionary<TColor, TColor>;
    FToolBars: TObjectDictionary<string, TToolBar>;
    FActiveProject: IIDEProject;
    FCurrentModule: IIDEModule;
    FModules: TList<IIDEModule>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MockMacro(Name: string; Value: string);
    procedure MockStyleColor(Key: TStyleColor; Value: TColor);
    procedure MockSystemColor(Key: TColor; Value: TColor);
    procedure MockToolBar(Id: string; ToolBar: TToolBar);
    procedure MockActiveProject(Project: IIDEProject);
    procedure MockCurrentModule(Module: IIDEModule);
    procedure MockModules(Modules: TList<IIDEModule>);

    // From IOTAIDEThemingServices
    procedure ApplyTheme(Component: TComponent);
    procedure RegisterFormClass(FormClass: TCustomFormClass);
    function GetStyleColor(Color: TStyleColor): TColor;
    function GetSystemColor(Color: TColor): TColor;

    // From IOTAServices
    function GetRootDirectory: string;
    procedure EditOptions(const Area: string; const PageCaption: string);
    function ExpandRootMacro(const Str: string): string;

    // From INTAIDEInsightService
    function AddActionListToIDEInsight(Actions: TCustomActionList): Integer;
    procedure RemoveActionListFromIDEInsight(const Index: Integer);

    // From INTAServices
    function GetMainMenu: TMainMenu;
    function AddImages(const ImageList: TImageList): Integer;
    procedure AddAction(Action: TCustomAction);
    procedure AddMenu(const Name: string; Item: TMenuItem);
    function GetToolBar(ToolBarId: string): TToolBar;
    function CreateDockableForm(const CustomForm: TCustomDockableFormBase): TCustomForm;
    procedure RegisterDockableForm(const CustomForm: TCustomDockableFormBase);
    procedure UnregisterDockableForm(const CustomForm: TCustomDockableFormBase);

    // From IOTAAboutBoxServices
    function AddPluginInfo(
      const Title: string;
      const Description: string;
      Image: TBitmap
    ): Integer;
    procedure RemovePluginInfo(const Index: Integer);

    // From IOTASplashScreenServices
    procedure AddPluginBitmap(
      const Caption: string;
      Image: TBitmap;
      IsUnregistered: Boolean;
      const LicenseStatus: string;
      const Version: string
    );

    // From IOTAModuleServices
    function GetCurrentModule: IIDEModule;
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IIDEModule;
    function GetActiveProject: IIDEProject;

    // From IOTAEditorServices
    function AddEditorNotifier(Notifier: IIDEEditorHandler): Integer;
    procedure RemoveEditorNotifier(const Index: Integer);

    // From INTAEnvironmentOptionsServices
    procedure RegisterAddInOptions(const Options: TAddInOptionsBase);
    procedure UnregisterAddInOptions(const Options: TAddInOptionsBase);

    property IDE: TMockIDE read FIDE;
  end;

  TMockLintContext = class(TInterfacedObject, ILintContext)
  private
    FAnalyzer: IAnalyzer;
    FIDEServices: IIDEServices;
    FPlugin: IPlugin;
    FSettings: TLintSettings;
    FProjectOptionsList: TObjectDictionary<string, TLintProjectOptions>;
    FSetupValid: Boolean;
    FTempSettingsFile: string;

    procedure Init;
    procedure Deinit;
  protected
    function GetAnalyzer: IAnalyzer;
    function GetIDEServices: IIDEServices;
    function GetPlugin: IPlugin;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MockAnalyzer(Analyzer: IAnalyzer);
    procedure MockIDEServices(IDEServices: IIDEServices);
    procedure MockPlugin(Plugin: IPlugin);
    procedure MockSettings; overload;
    procedure MockSettings(Settings: TLintSettings); overload;
    procedure MockProjectOptions(ProjectFile: string; ProjectOptions: TLintProjectOptions);
    procedure MockInvalidSetup;
    procedure Reset;

    function GetSettings: TLintSettings;
    function GetProjectOptions(ProjectFile: string): TLintProjectOptions;
    function ValidateSetup: Boolean;
  end;

function MockContext: TMockLintContext;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , DelphiLint.Utils
  ;

//______________________________________________________________________________________________________________________

function MockContext: TMockLintContext;
begin
  Result := TMockLintContext(LintContext);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.ClearFile(const FileName: string);
begin
  NotifyEvent(azcClearFile, [FileName]);
end;

//______________________________________________________________________________________________________________________

constructor TMockAnalyzer.Create;
begin
  inherited;
  FOnAnalysisStateChanged := TEventNotifier<TAnalysisStateChangeContext>.Create;

  FFileHistories := TDictionary<string, TFileAnalysisHistory>.Create;
  FFileStatuses := TDictionary<string, TFileAnalysisStatus>.Create;
  FIssues := TObjectDictionary<string, TDictionary<Integer, ILiveIssue>>.Create;
  FRules := TObjectDictionary<string, TRule>.Create;
end;

//______________________________________________________________________________________________________________________

destructor TMockAnalyzer.Destroy;
begin
  FreeAndNil(FOnAnalysisStateChanged);
  FreeAndNil(FFileHistories);
  FreeAndNil(FFileStatuses);
  FreeAndNil(FIssues);
  FreeAndNil(FRules);
  FreeAndNil(FCurrentAnalysis);

  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.AnalyzeFiles(const Paths: TArray<string>; const ProjectFile: string);
begin
  NotifyEvent(azcAnalyzeFiles, [Paths, ProjectFile]);
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetAnalysisStatus(Path: string): TFileAnalysisStatus;
begin
  Path := NormalizePath(Path);
  if not FFileStatuses.ContainsKey(Path) then begin
    raise EMockError.CreateFmt('Status for file ''%s'' has not been mocked', [Path]);
  end;

  Result := FFileStatuses[Path];
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetCurrentAnalysis: TCurrentAnalysis;
begin
  Result := FCurrentAnalysis;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetInAnalysis: Boolean;
begin
  Result := Assigned(FCurrentAnalysis);
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetIssues(FileName: string; Line: Integer): TArray<ILiveIssue>;
var
  ReturnIssues: TList<ILiveIssue>;
  LineNum: Integer;
begin
  FileName := NormalizePath(FileName);

  if FIssues.ContainsKey(FileName) then begin
    ReturnIssues := TList<ILiveIssue>.Create;
    try
      if Line = -1 then begin
        for LineNum in FIssues[FileName].Keys do begin
          ReturnIssues.AddRange(FIssues[FileName][LineNum]);
        end;
      end
      else if FIssues[FileName].ContainsKey(Line) then begin
        ReturnIssues.AddRange(FIssues[FileName][Line]);
      end;

      Result := ReturnIssues.ToArray;
    finally
      FreeAndNil(ReturnIssues);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetOnAnalysisStateChanged: DelphiLint.Events.TEventNotifier<TAnalysisStateChangeContext>;
begin
  Result := FOnAnalysisStateChanged;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetRule(RuleKey: string; AllowRefresh: Boolean): TRule;
begin
  if not FRules.ContainsKey(RuleKey) then begin
    raise EMockError.CreateFmt('Rule ''%s'' has not been mocked', [RuleKey]);
  end;

  Result := FRules[RuleKey];
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockCurrentAnalysis(CurrentAnalysis: TCurrentAnalysis);
begin
  FreeAndNil(FCurrentAnalysis);
  FCurrentAnalysis := CurrentAnalysis;
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileHistory(Path: string; History: TFileAnalysisHistory);
begin
  Path := NormalizePath(Path);
  FFileHistories.AddOrSetValue(Path, History);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileIssue(Path: string; Line: Integer; Issue: ILiveIssue);
begin
  Path := NormalizePath(Path);
  FIssues.AddOrSetValue(
    Path,
    TObjectDictionary<Integer, ILiveIssue>.Create([TPair<Integer, ILiveIssue>.Create(Line, Issue)]));
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileIssues(Path: string; Issues: TArray<ILiveIssue>);
var
  I: Integer;
begin
  Path := NormalizePath(Path);
  FIssues.AddOrSetValue(Path, TObjectDictionary<Integer, ILiveIssue>.Create);

  for I := 0 to Length(Issues) - 1 do begin
    FIssues[Path].AddOrSetValue(Issues[I].StartLine, Issues[I]);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileIssues(Path: string; Issues: TObjectDictionary<Integer, ILiveIssue>);
begin
  Path := NormalizePath(Path);
  FIssues.AddOrSetValue(Path, Issues);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileStatus(Path: string; Status: TFileAnalysisStatus);
begin
  Path := NormalizePath(Path);
  FFileStatuses.AddOrSetValue(Path, Status);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockRule(RuleKey: string; Rule: TRule);
begin
  FRules.AddOrSetValue(RuleKey, Rule);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.RestartServer;
begin
  NotifyEvent(azcRestartServer, []);
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
begin
  Path := NormalizePath(Path);
  Result := FFileHistories.TryGetValue(Path, History);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);
begin
  NotifyEvent(azcUpdateIssueLine, [FilePath, OriginalLine, NewLine]);
end;

//______________________________________________________________________________________________________________________

constructor TMockLintContext.Create;
begin
  inherited;
  Init;
end;

//______________________________________________________________________________________________________________________

destructor TMockLintContext.Destroy;
begin
  Deinit;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.Init;
begin
  FProjectOptionsList := TObjectDictionary<string, TLintProjectOptions>.Create;
  FSetupValid := True;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.Deinit;
begin
  FAnalyzer := nil;
  FPlugin := nil;
  FreeAndNil(FSettings);
  FIDEServices := nil;
  FreeAndNil(FProjectOptionsList);

  if FTempSettingsFile <> '' then begin
    TFile.Delete(FTempSettingsFile);
    FTempSettingsFile := '';
  end;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetAnalyzer: IAnalyzer;
begin
  if not Assigned(FAnalyzer) then begin
    raise EMockError.Create('Analyzer has not been mocked');
  end;

  Result := FAnalyzer;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetIDEServices: IIDEServices;
begin
  if not Assigned(FIDEServices) then begin
    raise EMockError.Create('IDE services have not been mocked');
  end;

  Result := FIDEServices;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetPlugin: IPlugin;
begin
  if not Assigned(FPlugin) then begin
    raise EMockError.Create('Plugin has not been mocked');
  end;

  Result := FPlugin;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetProjectOptions(ProjectFile: string): TLintProjectOptions;
begin
  ProjectFile := NormalizePath(ProjectFile);
  if not FProjectOptionsList.ContainsKey(ProjectFile) then begin
    raise EMockError.CreateFmt('Project options for file ''%s'' have not been mocked', [ProjectFile]);
  end;

  Result := FProjectOptionsList[ProjectFile];
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetSettings: TLintSettings;
begin
  if not Assigned(FSettings) then begin
    raise EMockError.Create('Settings have not been mocked');
  end;

  Result := FSettings;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockAnalyzer(Analyzer: IAnalyzer);
begin
  FAnalyzer := Analyzer;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockIDEServices(IDEServices: IIDEServices);
begin
  FIDEServices := IDEServices;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockInvalidSetup;
begin
  FSetupValid := False;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockPlugin(Plugin: IPlugin);
begin
  FPlugin := Plugin;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockProjectOptions(ProjectFile: string; ProjectOptions: TLintProjectOptions);
begin
  FProjectOptionsList.AddOrSetValue(NormalizePath(ProjectFile), ProjectOptions);
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockSettings;
begin
  FreeAndNil(FSettings);
  if FTempSettingsFile = '' then begin
    FTempSettingsFile := TPath.GetTempFileName;
  end;
  FSettings := TLintSettings.Create(FTempSettingsFile);
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockSettings(Settings: TLintSettings);
begin
  FreeAndNil(FSettings);
  FSettings := Settings;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.Reset;
begin
  Deinit;
  Init;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.ValidateSetup: Boolean;
begin
  Result := FSetupValid;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockActiveProject(Project: IIDEProject);
begin
  FActiveProject := Project;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockCurrentModule(Module: IIDEModule);
begin
  FCurrentModule := Module;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockModules(Modules: TList<IIDEModule>);
begin
  FreeAndNil(FModules);
  FModules := Modules;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockMacro(Name: string; Value: string);
begin
  FMacros.AddOrSetValue(Name, Value);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockStyleColor(Key: TStyleColor; Value: TColor);
begin
  FStyleColors.AddOrSetValue(Key, Value);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockSystemColor(Key: TColor; Value: TColor);
begin
  FSystemColors.AddOrSetValue(Key, Value);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.MockToolBar(Id: string; ToolBar: TToolBar);
begin
  FToolBars.Add(Id, ToolBar);
end;

//______________________________________________________________________________________________________________________

constructor TMockIDEServices.Create;
begin
  inherited;
  FIDE := TMockIDE.Create;
  FMacros := TDictionary<string, string>.Create;
  FStyleColors := TDictionary<TStyleColor, TColor>.Create;
  FSystemColors := TDictionary<TColor, TColor>.Create;
  FToolBars := TObjectDictionary<string, TToolBar>.Create;
  FModules := TList<IIDEModule>.Create;
end;

//______________________________________________________________________________________________________________________

destructor TMockIDEServices.Destroy;
begin
  FreeAndNil(FIDE);
  FreeAndNil(FMacros);
  FreeAndNil(FStyleColors);
  FreeAndNil(FSystemColors);
  FreeAndNil(FToolBars);
  FreeAndNil(FModules);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.AddAction(Action: TCustomAction);
begin
  FIDE.Actions.Add(Action);
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.AddActionListToIDEInsight(Actions: TCustomActionList): Integer;
begin
  Result := FIDE.IDEInsightActions.Count;
  FIDE.IDEInsightActions.Add(Actions);
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.AddEditorNotifier(Notifier: IIDEEditorHandler): Integer;
begin
  Result := FNextId;
  Inc(FNextId);

  FIDE.EditorNotifiers.Add(Result, Notifier);
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.AddImages(const ImageList: TImageList): Integer;
begin
  // TODO apply image list to mock IDE
  NotifyEvent(iscAddImages, [ImageList]);
  Result := 0;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.AddMenu(const Name: string; Item: TMenuItem);
begin
  FIDE.Menus.Add(Name, Item);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.AddPluginBitmap(const Caption: string; Image: TBitmap; IsUnregistered: Boolean;
  const LicenseStatus: string; const Version: string);
begin
  FIDE.PluginTitle := Caption;
  FIDE.PluginIcon := Image;
  FIDE.PluginLicenseStatus := LicenseStatus;
  FIDE.PluginVersion := Version;
  FIDE.PluginRegistered := not IsUnregistered;
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.AddPluginInfo(const Title: string; const Description: string; Image: TBitmap): Integer;
begin
  Result := FNextId;
  Inc(FNextId);
  FIDE.PluginInfos.Add(Result, TMockPluginInfo.Create(Title, Description, Image));
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.ApplyTheme(Component: TComponent);
begin
  NotifyEvent(iscApplyTheme, [Component]);
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.CreateDockableForm(const CustomForm: TCustomDockableFormBase): TCustomForm;
begin
  // TODO ?
  NotifyEvent(iscCreateDockableForm, [CustomForm]);
  Result := nil;
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.EditOptions(const Area: string; const PageCaption: string);
begin
  NotifyEvent(iscEditOptions, [Area, PageCaption]);
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.ExpandRootMacro(const Str: string): string;
begin
  if not FMacros.ContainsKey(Str) then begin
    raise EMockError.CreateFmt('Macro ''%s'' has not been mocked', [Str]);
  end;

  Result := FMacros[Str];
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetActiveProject: IIDEProject;
begin
  Result := FActiveProject;
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetCurrentModule: IIDEModule;
begin
  Result := FCurrentModule;
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetMainMenu: TMainMenu;
begin
  Result := FIDE.MainMenu;
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetModule(Index: Integer): IIDEModule;
begin
  Result := FModules[Index];
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetModuleCount: Integer;
begin
  Result := FModules.Count;
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetRootDirectory: string;
begin
  if FRootDirectory = '' then begin
    raise EMockError.Create('Root directory has not been mocked');
  end;

  Result := FRootDirectory;
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetStyleColor(Color: TStyleColor): TColor;
begin
  if not FStyleColors.ContainsKey(Color) then begin
    raise EMockError.Create('Style color has not been mocked');
  end;

  Result := FStyleColors[Color];
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetSystemColor(Color: TColor): TColor;
begin
  if not FSystemColors.ContainsKey(Color) then begin
    raise EMockError.Create('System color has not been mocked');
  end;

  Result := FSystemColors[Color];
end;

//______________________________________________________________________________________________________________________

function TMockIDEServices.GetToolBar(ToolBarId: string): TToolBar;
begin
  if not FToolBars.ContainsKey(ToolBarId) then begin
    raise EMockError.CreateFmt('Toolbar ''%s'' has not been mocked', [ToolBarId]);
  end;

  Result := FToolBars[ToolBarId];
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.RegisterAddInOptions(const Options: TAddInOptionsBase);
begin
  NotifyEvent(iscRegisterAddInOptions, [Options]);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.RegisterDockableForm(const CustomForm: TCustomDockableFormBase);
begin
  NotifyEvent(iscRegisterDockableForm, [CustomForm]);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.RegisterFormClass(FormClass: TCustomFormClass);
begin
  NotifyEvent(iscRegisterFormClass, [FormClass]);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.RemoveActionListFromIDEInsight(const Index: Integer);
begin
  FIDE.IDEInsightActions.Delete(Index);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.RemoveEditorNotifier(const Index: Integer);
begin
  FIDE.EditorNotifiers.Remove(Index);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.RemovePluginInfo(const Index: Integer);
begin
  FIDE.PluginInfos.Remove(Index);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.UnregisterAddInOptions(const Options: TAddInOptionsBase);
begin
  NotifyEvent(iscUnregisterAddInOptions, [Options]);
end;

//______________________________________________________________________________________________________________________

procedure TMockIDEServices.UnregisterDockableForm(const CustomForm: TCustomDockableFormBase);
begin
  NotifyEvent(iscUnregisterDockableForm, [CustomForm]);
end;

//______________________________________________________________________________________________________________________

constructor TMockIDE.Create;
begin
  inherited;
  FActions := TList<TCustomAction>.Create;
  FIDEInsightActions := TList<TCustomActionList>.Create;
  FEditorNotifiers := TDictionary<Integer, IIDEEditorHandler>.Create;
  FMenus := TObjectDictionary<string, TMenuItem>.Create;
  FPluginInfos := TObjectDictionary<Integer, TMockPluginInfo>.Create;
end;

destructor TMockIDE.Destroy;
begin
  FreeAndNil(FActions);
  FreeAndNil(FIDEInsightActions);
  FreeAndNil(FEditorNotifiers);
  FreeAndNil(FMenus);
  FreeAndNil(FPluginInfos);
  FreeAndNil(FPluginIcon);
  FreeAndNil(FMainMenu);
  inherited;
end;

//______________________________________________________________________________________________________________________

constructor TMockPluginInfo.Create(Title: string; Description: string; Icon: TBitmap);
begin
  inherited Create;
  FTitle := Title;
  FDescription := Description;
  FIcon := Icon;
end;

destructor TMockPluginInfo.Destroy;
begin
  FreeAndNil(FIcon);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TMockIDEObject.Raw: IInterface;
begin
  Result := nil;
end;

//______________________________________________________________________________________________________________________

function TMockIDEObject<T>.Raw: IInterface;
begin
  Result := nil;
end;

//______________________________________________________________________________________________________________________

constructor TMockEditView.Create;
begin
  inherited;
  FNotifiers := TDictionary<Integer, IIDEViewHandler>.Create;
end;

destructor TMockEditView.Destroy;
begin
  FreeAndNil(FNotifiers);
  inherited;
end;

function TMockEditView.AddNotifier(Notifier: IIDEViewHandler): Integer;
begin
  FNotifiers.Add(FNextId, Notifier);
  Result := FNextId;
  FNextId := FNextId + 1;
end;

function TMockEditView.GetFileName: string;
begin
  Result := FFileName;
end;

function TMockEditView.GetLeftColumn: Integer;
begin
  Result := FLeftColumn;
end;

function TMockEditView.GetLineTracker: IIDEEditLineTracker;
begin
  Result := FLineTracker;
end;

procedure TMockEditView.GoToPosition(const Line: Integer; const Column: Integer);
begin
  NotifyEvent(evcGoToPosition, [Line, Column]);
end;

procedure TMockEditView.Paint;
begin
  NotifyEvent(evcPaint, []);
end;

procedure TMockEditView.RemoveNotifier(Index: Integer);
begin
  FNotifiers.Remove(Index);
end;

//______________________________________________________________________________________________________________________

constructor TMockSourceEditor.Create;
begin
  inherited;
  FEditViews := TList<IIDEEditView>.Create;
end;

destructor TMockSourceEditor.Destroy;
begin
  FreeAndNil(FEditViews);
  inherited;
end;

function TMockSourceEditor.GetEditView(Index: Integer): IIDEEditView;
begin
  Result := FEditViews[Index];
end;

function TMockSourceEditor.GetEditViewCount: Integer;
begin
  Result := FEditViews.Count;
end;

function TMockSourceEditor.GetFileName: string;
begin
  Result := FFileName;
end;

function TMockSourceEditor.GetModule: IIDEModule;
begin
  Result := FModule;
end;

//______________________________________________________________________________________________________________________

constructor TMockProject.Create;
begin
  inherited;
  FFileList := TList<string>.Create;
end;

destructor TMockProject.Destroy;
begin
  FreeAndNil(FFileList);
  inherited;
end;

procedure TMockProject.GetCompleteFileList(FileList: TStrings);
var
  FileName: string;
begin
  for FileName in FFileList do begin
    FileList.Add(FileName);
  end;
end;

//______________________________________________________________________________________________________________________

function TMockModule.GetFileName: string;
begin
  Result := FFileName;
end;

function TMockModule.GetSourceEditor: IIDESourceEditor;
begin
  Result := FSourceEditor;
end;

procedure TMockModule.Save(const ForceSave: Boolean);
begin
  NotifyEvent(mdcSave, [ForceSave]);
end;

//______________________________________________________________________________________________________________________

procedure TMockEditLineTracker.AddLine(const Line: Integer; const Value: Integer);
begin
  FLines.AddOrSetValue(Line, Value);
end;

function TMockEditLineTracker.AddNotifier(Notifier: IIDEEditLineHandler): Integer;
begin
  FNotifiers.Add(FNextId, Notifier);
  Result := FNextId;
  FNextId := FNextId + 1;
end;

procedure TMockEditLineTracker.Clear;
begin
  FLines.Clear;
end;

constructor TMockEditLineTracker.Create;
begin
  inherited;
  FNotifiers := TDictionary<Integer, IIDEEditLineHandler>.Create;
  FLines := TDictionary<Integer, Integer>.Create;
end;

destructor TMockEditLineTracker.Destroy;
begin
  FreeAndNil(FNotifiers);
  FreeAndNil(FLines);
  inherited;
end;

function TMockEditLineTracker.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TMockEditLineTracker.RemoveNotifier(Index: Integer);
begin
  FNotifiers.Remove(Index);
end;

//______________________________________________________________________________________________________________________

initialization
  SetLintContext(TMockLintContext.Create);

end.
