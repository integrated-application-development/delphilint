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
  , DelphiLint.LiveData
  ;

type
  ILogger = interface
    ['{B5D90CF6-B2C9-473D-9DB9-1BB75EAFC517}']
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Msg: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Warn(const Msg: string); overload;
    procedure Warn(const Msg: string; const Args: array of const); overload;
  end;

  TAnalysisStateChange = (
    ascStarted,
    ascSucceeded,
    ascFailed,
    ascCleared,
    ascUpdated
  );

  TAnalysisStateChangeContext = record
    Files: TArray<string>;
    Change: TAnalysisStateChange;
  end;

  IAnalyzer = interface
    ['{F6ECFABE-D0AE-40F2-B0D6-B3B67947D7DE}']
    function GetOnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext>;
    function GetCurrentAnalysis: TCurrentAnalysis;
    function GetInAnalysis: Boolean;
    function GetIssues(FileName: string; Line: Integer = -1; Column: Integer = -1): TArray<ILiveIssue>;
    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeFiles(const Files: TArray<string>; const ProjectFile: string);
    procedure ClearFile(const FileName: string);
    procedure RestartServer;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;

    property OnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext> read GetOnAnalysisStateChanged;
    property CurrentAnalysis: TCurrentAnalysis read GetCurrentAnalysis;
    property InAnalysis: Boolean read GetInAnalysis;
  end;

  IIDEProject = interface
    ['{323288E1-A17D-441D-8BA7-5C4290C6F6FD}']
    procedure GetCompleteFileList(FileList: TStrings);
    function Raw: IInterface; // IOTAProject
  end;

  IIDEModule = interface;
  IIDEEditLineTracker = interface;
  IIDEViewHandler = interface;

  IIDEEditView = interface
    ['{9CA07874-237F-4829-869B-1F99211820A6}']
    procedure Paint;
    procedure GoToPosition(const Line: Integer; const Column: Integer);
    function GetFileName: string;
    function GetLineTracker: IIDEEditLineTracker;
    function AddNotifier(Notifier: IIDEViewHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    function GetLeftColumn: Integer;
    procedure ReplaceText(
      Replacement: string;
      StartLine: Integer;
      StartColumn: Integer;
      EndLine: Integer;
      EndColumn: Integer
    );
    function GetColumn: Integer;
    function GetRow: Integer;
    function GetContextMenu: TPopupMenu;
    function Raw: IInterface; // IOTAEditView

    property FileName: string read GetFileName;
    property LeftColumn: Integer read GetLeftColumn;
    property Column: Integer read GetColumn;
    property Row: Integer read GetRow;
  end;

  IIDESourceEditor = interface
    ['{BF788F6F-5A6D-420D-A440-5BE6862AEEBA}']
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
    ['{B1813022-B926-4436-BDF5-2E590771F581}']
    function GetSourceEditor: IIDESourceEditor;
    procedure Save(const ForceSave: Boolean);
    function GetFileName: string;
    function Raw: IInterface; // IOTAModule

    property FileName: string read GetFileName;
    property SourceEditor: IIDESourceEditor read GetSourceEditor;
  end;


  IIDEHandler = interface
    ['{65472079-8DD3-43E4-9F24-C69674CCDF03}']
    function GetOnReleased: TEventNotifier<IIDEHandler>;
    function GetOnOwnerFreed: TEventNotifier<IIDEHandler>;
    procedure Release;

    property OnReleased: TEventNotifier<IIDEHandler> read GetOnReleased;
    property OnOwnerFreed: TEventNotifier<IIDEHandler> read GetOnOwnerFreed;
  end;

  IIDEEditorHandler = interface(IIDEHandler)
    ['{C1B2BAB1-252E-4C92-B493-58A2BE304D27}']
    procedure OnViewAdded(const View: IIDEEditView);
    procedure OnViewActivated(const View: IIDEEditView);
  end;

  TLinePaintContext = record
    View: IIDEEditView;
    Canvas: TCanvas;
    LineNumber: Integer;
    LineText: string;
    TextRect: TRect;
    LineRect: TRect;
    CellSize: TSize;
  end;

  IIDEViewHandler = interface(IIDEHandler)
    ['{F4FB2AF3-546F-43D1-ACFC-31F8972803C0}']
    procedure OnBeginPaint(const View: IIDEEditView; var FullRepaint: Boolean);
    procedure OnPaintLine(const Context: TLinePaintContext);
  end;

  IIDEEditLineHandler = interface(IIDEHandler)
    ['{E7168514-2EDB-41FB-8F30-A9D6704488B7}']
    procedure OnLineChanged(OldLine: Integer; NewLine: Integer; Data: Integer);
  end;

  IIDEEditLineTracker = interface
    ['{0E41ADA7-FE27-42BD-8153-EBBF96260385}']
    function GetFileName: string;
    procedure Clear;
    function AddNotifier(Notifier: IIDEEditLineHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure AddLine(const Line: Integer; const Value: Integer);

    property FileName: string read GetFileName;
  end;

  IIDEServices = interface
    ['{1812752E-1C48-4951-B012-0FFE467FDE9D}']
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
    function GetTopView: IIDEEditView;

    // From INTAEditorServices
    function GetTopEditWindow: TCustomForm;

    // From INTAEnvironmentOptionsServices
    procedure RegisterAddInOptions(const Options: TAddInOptionsBase);
    procedure UnregisterAddInOptions(const Options: TAddInOptionsBase);
  end;

  IPlugin = interface
    ['{75F7FD1F-CB45-4F19-8197-375321A3ADCB}']
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
    ['{AA76E4DC-8624-428A-846C-7DF097A26E83}']
    function GetAnalyzer: IAnalyzer;
    function GetIDEServices: IIDEServices;
    function GetPlugin: IPlugin;
    function GetSettings: TLintSettings;
    function GetProjectOptions(ProjectFile: string): TLintProjectOptions;

    function ValidateSetup: Boolean;

    property Analyzer: IAnalyzer read GetAnalyzer;
    property IDEServices: IIDEServices read GetIDEServices;
    property Plugin: IPlugin read GetPlugin;
    property Settings: TLintSettings read GetSettings;
  end;

function LintContext: ILintContext;
function Analyzer: IAnalyzer;
function ContextValid: Boolean;
function Log: ILogger;

procedure SetLintContext(Context: ILintContext);
procedure SetLogger(Logger: ILogger);

implementation

var
  GLintContext: ILintContext;
  GLogger: ILogger;
  GFinalized: Boolean = False;

type
  TNoOpLogger = class(TInterfacedObject, ILogger)
  public
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Msg: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Warn(const Msg: string); overload;
    procedure Warn(const Msg: string; const Args: array of const); overload;
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
begin
  if Assigned(GLogger) then begin
    Result := GLogger;
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

procedure SetLogger(Logger: ILogger);
begin
  GLogger := Logger;
end;

//______________________________________________________________________________________________________________________

function ContextValid: Boolean;
begin
  Result := not GFinalized;
end;

//______________________________________________________________________________________________________________________

procedure TNoOpLogger.Debug(const Msg: string);
begin
  // No-op
end;

procedure TNoOpLogger.Debug(const Msg: string; const Args: array of const);
begin
  // No-op
end;

procedure TNoOpLogger.Info(const Msg: string; const Args: array of const);
begin
  // No-op
end;

procedure TNoOpLogger.Info(const Msg: string);
begin
  // No-op
end;

procedure TNoOpLogger.Warn(const Msg: string);
begin
  // No-op
end;

procedure TNoOpLogger.Warn(const Msg: string; const Args: array of const);
begin
  // No-op
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  GFinalized := True;
  GLintContext := nil;
  GLogger := nil;

end.
