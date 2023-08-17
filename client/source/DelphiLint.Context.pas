unit DelphiLint.Context;

interface

uses
    System.UITypes
  , System.Classes
  , Vcl.Themes
  , Vcl.Forms
  , Vcl.ActnList
  , Vcl.Controls
  , Vcl.Menus
  , Vcl.Graphics
  , Vcl.ComCtrls
  , DelphiLint.Events
  , DelphiLint.Data
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

  IIDEEditView = interface
    procedure Paint;
    procedure GoToPosition(const Line: Integer; const Column: Integer);
    function Raw: IInterface; // IOTAEditView
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
    function AddEditorNotifier(Notifier: TEditorNotifierBase): Integer;
    procedure RemoveEditorNotifier(const Index: Integer);

    // From INTAEnvironmentOptionsServices
    procedure RegisterAddInOptions(const Options: TAddInOptionsBase);
    procedure UnregisterAddInOptions(const Options: TAddInOptionsBase);
  end;

  ILintContext = interface
    function GetAnalyzer: IAnalyzer;
    function GetLogger: ILogger;
    function GetIDEServices: IIDEServices;

    property Analyzer: IAnalyzer read GetAnalyzer;
    property Log: ILogger read GetLogger;
    property IDEServices: IIDEServices read GetIDEServices;
  end;

function LintContext: ILintContext;
function Analyzer: IAnalyzer;
function ContextValid: Boolean;
function Log: ILogger;

procedure SetLintContext(Context: ILintContext);

implementation

uses
    System.SysUtils
  ;

var
  GLintContext: ILintContext;
  GFinalized: Boolean = False;

type
  TSingleUseLogger = class(TInterfacedObject, ILogger)
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
    Result := TSingleUseLogger.Create;
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

procedure TSingleUseLogger.Info(const Msg: string; const Args: array of const);
begin
  Free;
end;

//______________________________________________________________________________________________________________________

procedure TSingleUseLogger.Info(const Msg: string);
begin
  Free;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  GFinalized := True;
  GLintContext := nil;

end.
