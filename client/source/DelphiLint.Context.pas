unit DelphiLint.Context;

interface

uses
    System.UITypes
  , System.Classes
  , Winapi.Windows
  , Vcl.Themes
  , Vcl.Forms
  , Vcl.ActnList
  , Vcl.Controls
  , Vcl.Menus
  , Vcl.Graphics
  , Vcl.ComCtrls
  , DelphiLint.Events
  , DelphiLint.Data
  , DelphiLint.Settings
  , DelphiLint.ProjectOptions
  , DelphiLint.IDEBaseTypes
  ;

type
  ILogger = interface
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
  end;

  IAnalyzer = interface
    function GetOnAnalysisStarted: TEventNotifier<TArray<string>>;
    function GetOnAnalysisComplete: TEventNotifier<TArray<string>>;
    function GetOnAnalysisFailed: TEventNotifier<TArray<string>>;
    function GetCurrentAnalysis: TCurrentAnalysis;
    function GetInAnalysis: Boolean;
    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>;
    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeActiveFile;
    procedure AnalyzeOpenFiles;
    procedure RestartServer;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;

    property OnAnalysisStarted: TEventNotifier<TArray<string>> read GetOnAnalysisStarted;
    property OnAnalysisComplete: TEventNotifier<TArray<string>> read GetOnAnalysisComplete;
    property OnAnalysisFailed: TEventNotifier<TArray<string>> read GetOnAnalysisFailed;
    property CurrentAnalysis: TCurrentAnalysis read GetCurrentAnalysis;
    property InAnalysis: Boolean read GetInAnalysis;
  end;

  IIDEProject = interface
    procedure GetCompleteFileList(FileList: TStrings);
    function Raw: IInterface; // IOTAProject
  end;

  IIDEModule = interface;
  IIDEEditLineTracker = interface;
  IIDEViewNotifier = interface;

  IIDEEditView = interface
    procedure Paint;
    procedure GoToPosition(const Line: Integer; const Column: Integer);
    function GetFileName: string;
    function GetLineTracker: IIDEEditLineTracker;
    function AddNotifier(Notifier: IIDEViewNotifier): Integer;
    procedure RemoveNotifier(Index: Integer);
    function GetLeftColumn: Integer;
    function Raw: IInterface; // IOTAEditView

    property FileName: string read GetFileName;
    property LeftColumn: Integer read GetLeftColumn;
  end;

  IIDESourceEditor = interface
    function GetModule: IIDEModule;
    function GetEditViewCount: Integer;
    function GetEditView(Index: Integer): IIDEEditView;
    function GetFileName: string;
    function Raw: IInterface; // IOTASourceEditor

    property FileName: string read GetFileName;
    property Module: IIDEModule read GetModule;
    property EditViewCount: Integer read GetEditViewCount;
    property EditViews[Index: Integer]: IIDEEditView read GetEditView;
  end;

  IIDEModule = interface
    function GetSourceEditor: IIDESourceEditor;
    procedure Save(const ForceSave: Boolean);
    function GetFileName: string;
    function Raw: IInterface; // IOTAModule

    property FileName: string read GetFileName;
    property SourceEditor: IIDESourceEditor read GetSourceEditor;
  end;


  IIDENotifier = interface
    function GetOnReleased: TEventNotifier<IIDENotifier>;
    function GetOnOwnerFreed: TEventNotifier<IIDENotifier>;
    procedure Release;

    property OnReleased: TEventNotifier<IIDENotifier> read GetOnReleased;
    property OnOwnerFreed: TEventNotifier<IIDENotifier> read GetOnOwnerFreed;
  end;

  IIDEEditorNotifier = interface(IIDENotifier)
    procedure OnViewAdded(const View: IIDEEditView);
    procedure OnViewActivated(const View: IIDEEditView);
  end;

  IIDEViewNotifier = interface(IIDENotifier)
    procedure OnBeginPaint(const View: IIDEEditView; var FullRepaint: Boolean);
    procedure OnPaintLine(
      const View: IIDEEditView;
      LineNumber: Integer;
      LineText: string;
      const TextWidth: Integer;
      const Canvas: TCanvas;
      const TextRect: TRect;
      const LineRect: TRect;
      const CellSize: TSize
    );
  end;

  IIDEEditLineNotifier = interface(IIDENotifier)
    procedure OnLineChanged(OldLine: Integer; NewLine: Integer; Data: Integer);
  end;

  IIDEEditLineTracker = interface
    function GetFileName: string;
    procedure Clear;
    function AddNotifier(Notifier: IIDEEditLineNotifier): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure AddLine(const Line: Integer; const Value: Integer);

    property FileName: string read GetFileName;
  end;

  IIDEServices = interface
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
    function AddEditorNotifier(Notifier: IIDEEditorNotifier): Integer;
    procedure RemoveEditorNotifier(const Index: Integer);

    // From INTAEnvironmentOptionsServices
    procedure RegisterAddInOptions(const Options: TAddInOptionsBase);
    procedure UnregisterAddInOptions(const Options: TAddInOptionsBase);
  end;

  IPlugin = interface
    procedure EnablePlugin;
    procedure DisablePlugin;
    function GetPluginEnabled: Boolean;
    function GetOnActiveFileChanged: TEventNotifier<string>;
    procedure Init;
    procedure Deinit(IDEServices: IIDEServices);

    property OnActiveFileChanged: TEventNotifier<string> read GetOnActiveFileChanged;
    property PluginEnabled: Boolean read GetPluginEnabled;
  end;

  ILintContext = interface
    function GetAnalyzer: IAnalyzer;
    function GetLogger: ILogger;
    function GetIDEServices: IIDEServices;
    function GetPlugin: IPlugin;
    function GetSettings: TLintSettings;
    function GetProjectOptions(ProjectFile: string): TLintProjectOptions;

    property Analyzer: IAnalyzer read GetAnalyzer;
    property Log: ILogger read GetLogger;
    property IDEServices: IIDEServices read GetIDEServices;
    property Plugin: IPlugin read GetPlugin;
    property Settings: TLintSettings read GetSettings;
  end;

function LintContext: ILintContext;
function Analyzer: IAnalyzer;
function ContextValid: Boolean;
function Log: ILogger;

procedure SetLintContext(Context: ILintContext);

implementation

var
  GLintContext: ILintContext;
  GFinalized: Boolean = False;

type
  TNoOpLogger = class(TInterfacedObject, ILogger)
  public
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
  end;

//______________________________________________________________________________________________________________________

function LintContext: ILintContext;
begin
  Result := GLintContext;
end;

//______________________________________________________________________________________________________________________

function Analyzer: IAnalyzer;
begin
  Result := LintContext.Analyzer;
end;

//______________________________________________________________________________________________________________________

function Log: ILogger;
var
  Context: ILintContext;
begin
  Context := LintContext;
  if Assigned(Context) then begin
    Result := Context.Log;
  end
  else begin
    Result := TNoOpLogger.Create;
  end;
end;

//______________________________________________________________________________________________________________________

procedure SetLintContext(Context: ILintContext);
begin
  GLintContext := Context;
end;

//______________________________________________________________________________________________________________________

function ContextValid: Boolean;
begin
  Result := not GFinalized;
end;

//______________________________________________________________________________________________________________________

procedure TNoOpLogger.Info(const Msg: string; const Args: array of const);
begin
  // No-op
end;

//______________________________________________________________________________________________________________________

procedure TNoOpLogger.Info(const Msg: string);
begin
  // No-op
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  if Assigned(GLintContext) then begin
    GLintContext.Plugin.Deinit(GLintContext.IDEServices);
  end;
  GFinalized := True;
  GLintContext := nil;

end.
