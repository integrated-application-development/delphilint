unit DelphiLint.IDEContext;

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
  , DelphiLint.IDEBaseTypes
  , DelphiLint.Context
  , DelphiLint.Settings
  , DelphiLint.ProjectOptions
  ;

{$IFNDEF TOOLSAPI}
  {$MESSAGE FATAL 'This unit requires the ToolsAPI.'}
{$ENDIF}

type
  TToolsApiServices = class(TInterfacedObject, IIDEServices)
  public
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
  end;

  TIDELintContext = class(TInterfacedObject, ILintContext)
  private
    FAnalyzer: IAnalyzer;
    FIDEServices: IIDEServices;
    FPlugin: IPlugin;
    FSettings: TLintSettings;
    FSettingsDir: string;
  protected
    function GetAnalyzer: IAnalyzer;
    function GetIDEServices: IIDEServices;
    function GetPlugin: IPlugin;
  public
    constructor Create;
    destructor Destroy; override;

    function ValidateSetup: Boolean;
    function GetSettings: TLintSettings;
    function GetProjectOptions(ProjectFile: string): TLintProjectOptions;
  end;

procedure Register;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , System.DateUtils
  , Winapi.Windows
  , DelphiLint.Analyzer
  , DelphiLint.Logger
  , DelphiLint.Plugin
  , DelphiLint.SetupForm
  , ToolsAPI
  , DockForm
  ;

type
  TToolsApiWrapper<T: IInterface> = class(TInterfacedObject)
  protected
    FRaw: T;
  public
    constructor Create(Raw: T);
    function Raw: IInterface;
  end;

  TToolsApiEditView = class(TToolsApiWrapper<IOTAEditView>, IIDEEditView)
  public
    procedure Paint;
    procedure GoToPosition(const Line: Integer; const Column: Integer);
    function GetFileName: string;
    function GetLineTracker: IIDEEditLineTracker;
    function AddNotifier(Notifier: IIDEViewHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    function GetLeftColumn: Integer;
  end;

  TToolsApiSourceEditor = class(TToolsApiWrapper<IOTASourceEditor>, IIDESourceEditor)
  public
    function GetModule: IIDEModule;
    function GetEditViewCount: Integer;
    function GetEditView(Index: Integer): IIDEEditView;
    function GetFileName: string;
  end;

  TToolsApiProject = class(TToolsApiWrapper<IOTAProject>, IIDEProject)
  public
    procedure GetCompleteFileList(FileList: TStrings);
  end;

  TToolsApiModule = class(TToolsApiWrapper<IOTAModule>, IIDEModule)
  public
    function GetSourceEditor: IIDESourceEditor;
    procedure Save(const ForceSave: Boolean);
    function GetFileName: string;
  end;

  TToolsApiEditLineTracker = class(TToolsApiWrapper<IOTAEditLineTracker>, IIDEEditLineTracker)
    function GetFileName: string;
    procedure Clear;
    function AddNotifier(Notifier: IIDEEditLineHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure AddLine(const Line: Integer; const Value: Integer);
  end;

  TToolsApiNotifier<T: IIDEHandler> = class(TInterfacedObject, IOTANotifier)
  protected
    FHandler: T;
  public
    constructor Create(Handler: T);
    procedure Destroyed; virtual;
    procedure BeforeSave; virtual;
    procedure AfterSave; virtual;
    procedure Modified; virtual;
  end;

  TToolsApiEditorNotifier = class(TToolsApiNotifier<IIDEEditorHandler>, IOTAEditorNotifier, INTAEditServicesNotifier)
    procedure ViewActivated(const View: IOTAEditView);
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

  TToolsApiViewNotifier = class(TToolsApiNotifier<IIDEViewHandler>, INTAEditViewNotifier)
    procedure EditorIdle(const View: IOTAEditView);
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
    procedure EndPaint(const View: IOTAEditView);
  end;

  TToolsApiEditLineNotifier = class(TToolsApiNotifier<IIDEEditLineHandler>, IOTAEditLineNotifier)
    procedure LineChanged(OldLine, NewLine: Integer; Data: Integer);
  end;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  LintContext.Plugin.Init;
end;

//______________________________________________________________________________________________________________________

function BuildDelphiLintFileLogger: ILogger;
var
  LogDir: string;
  LogPath: string;
begin
  LogDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint\logs');
  TDirectory.CreateDirectory(LogDir);
  LogPath := TPath.Combine(LogDir, 'delphilint-client.log');
  Result := TFileLogger.Create(LogPath);
end;

//______________________________________________________________________________________________________________________

constructor TIDELintContext.Create;
begin
  inherited;
  FPlugin := TIDEPlugin.Create(GetIDEServices);

  FSettingsDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint');
  FSettings := TLintSettings.Create(TPath.Combine(FSettingsDir, 'delphilint.ini'));

  Log.Info('-------------------------------------------------');
  Log.Info('DelphiLint started at %s', [DateToISO8601(Now)]);
end;

//______________________________________________________________________________________________________________________

destructor TIDELintContext.Destroy;
begin
  FPlugin.Deinit(FIDEServices);

  FAnalyzer := nil;
  FPlugin := nil;
  FreeAndNil(FSettings);
  FIDEServices := nil;
  inherited;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetAnalyzer: IAnalyzer;
begin
  if not Assigned(FAnalyzer) then begin
    FAnalyzer := TAnalyzerImpl.Create;
  end;

  Result := FAnalyzer;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetIDEServices: IIDEServices;
begin
  if not Assigned(FIDEServices) then begin
    FIDEServices := TToolsApiServices.Create;
  end;

  Result := FIDEServices;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetPlugin: IPlugin;
begin
  Result := FPlugin;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetProjectOptions(ProjectFile: string): TLintProjectOptions;
begin
  Result := TLintProjectOptions.Create(TPath.ChangeExtension(ProjectFile, '.delphilint'));
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetSettings: TLintSettings;
begin
  Result := FSettings;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.ValidateSetup: Boolean;
begin
  Result := TLintSetupForm.TryFixSetup;

  if Result then begin
    FPlugin.EnablePlugin;
  end
  else begin
    FPlugin.DisablePlugin;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.ApplyTheme(Component: TComponent);
begin
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(Component);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RegisterFormClass(FormClass: TCustomFormClass);
begin
  (BorlandIDEServices as IOTAIDEThemingServices).RegisterFormClass(FormClass);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetStyleColor(Color: TStyleColor): TColor;
begin
  Result := (BorlandIDEServices as IOTAIDEThemingServices).StyleServices.GetStyleColor(Color);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetSystemColor(Color: TColor): TColor;
begin
  Result := (BorlandIDEServices as IOTAIDEThemingServices).StyleServices.GetSystemColor(Color);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetRootDirectory: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetRootDirectory;
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.EditOptions(const Area, PageCaption: string);
begin
  (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.EditOptions(Area, PageCaption);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.ExpandRootMacro(const Str: string): string;
begin
  Result := (BorlandIDEServices as IOTAServices).ExpandRootMacro(Str);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.AddActionListToIDEInsight(Actions: TCustomActionList): Integer;
begin
  Result := (BorlandIDEServices as INTAIDEInsightService).AddActionList(Actions);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RemoveActionListFromIDEInsight(const Index: Integer);
begin
  (BorlandIDEServices as INTAIDEInsightService).RemoveActionList(Index);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetMainMenu: TMainMenu;
begin
  Result := (BorlandIDEServices as INTAServices).MainMenu;
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.AddImages(const ImageList: TImageList): Integer;
begin
  Result := (BorlandIDEServices as INTAServices).AddImages(ImageList);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.AddAction(Action: TCustomAction);
begin
  (BorlandIDEServices as INTAServices).AddActionMenu('', Action, nil);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.AddMenu(const Name: string; Item: TMenuItem);
begin
  (BorlandIDEServices as INTAServices).AddActionMenu(Name, nil, Item);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetToolBar(ToolBarId: string): TToolBar;
begin
  Result := (BorlandIDEServices as INTAServices).GetToolBar(ToolBarId);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.CreateDockableForm(const CustomForm: TCustomDockableFormBase): TCustomForm;
begin
  Result := (BorlandIDEServices as INTAServices).CreateDockableForm(CustomForm);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RegisterDockableForm(const CustomForm: TCustomDockableFormBase);
begin
  (BorlandIDEServices as INTAServices).RegisterDockableForm(CustomForm);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.UnregisterDockableForm(const CustomForm: TCustomDockableFormBase);
begin
  (BorlandIDEServices as INTAServices).UnregisterDockableForm(CustomForm);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.AddPluginInfo(const Title, Description: string; Image: Vcl.Graphics.TBitmap): Integer;
begin
  Result := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(Title, Description, Image.Handle);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RemovePluginInfo(const Index: Integer);
begin
  (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(Index);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.AddPluginBitmap(const Caption: string; Image: Vcl.Graphics.TBitmap; IsUnregistered: Boolean;
  const LicenseStatus, Version: string);
begin
  SplashScreenServices.AddPluginBitmap(Caption, Image.Handle, IsUnregistered, LicenseStatus, Version);
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetCurrentModule: IIDEModule;
var
  Module: IOTAModule;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;

  if Assigned(Module) then begin
    Result := TToolsApiModule.Create(Module);
  end;
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetModuleCount: Integer;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).ModuleCount;
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetModule(Index: Integer): IIDEModule;
var
  Module: IOTAModule;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).Modules[Index];

  if Assigned(Module) then begin
    Result := TToolsApiModule.Create(Module);
  end;
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.GetActiveProject: IIDEProject;
var
  Project: IOTAProject;
begin
  Project := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;

  if Assigned(Project) then begin
    Result := TToolsApiProject.Create(Project);
  end;
end;

//______________________________________________________________________________________________________________________

function TToolsApiServices.AddEditorNotifier(Notifier: IIDEEditorHandler): Integer;
begin
  Result := (BorlandIDEServices as IOTAEditorServices).AddNotifier(TToolsApiEditorNotifier.Create(Notifier));
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RemoveEditorNotifier(const Index: Integer);
begin
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(Index);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RegisterAddInOptions(const Options: TAddInOptionsBase);
begin
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(Options);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.UnregisterAddInOptions(const Options: TAddInOptionsBase);
begin
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(Options);
end;

//______________________________________________________________________________________________________________________

function TToolsApiEditView.GetLeftColumn: Integer;
begin
  Result := FRaw.LeftColumn;
end;

function TToolsApiEditView.GetLineTracker: IIDEEditLineTracker;
begin
  Result := TToolsApiEditLineTracker.Create(FRaw.Buffer.GetEditLineTracker);
end;

procedure TToolsApiEditView.GoToPosition(const Line, Column: Integer);
begin
  FRaw.Buffer.EditPosition.GotoLine(Line);
  FRaw.Buffer.EditPosition.Column;
end;

procedure TToolsApiEditView.Paint;
begin
  FRaw.Paint;
end;

function TToolsApiEditView.AddNotifier(Notifier: IIDEViewHandler): Integer;
begin
  Result := FRaw.AddNotifier(TToolsApiViewNotifier.Create(Notifier));
end;

procedure TToolsApiEditView.RemoveNotifier(Index: Integer);
begin
  FRaw.RemoveNotifier(Index);
end;

function TToolsApiEditView.GetFileName: string;
begin
  Result := FRaw.Buffer.FileName;
end;

//______________________________________________________________________________________________________________________

constructor TToolsApiWrapper<T>.Create(Raw: T);
begin
  inherited Create;
  FRaw := Raw;
end;

function TToolsApiWrapper<T>.Raw: IInterface;
begin
  Result := FRaw;
end;

//______________________________________________________________________________________________________________________

function TToolsApiSourceEditor.GetEditView(Index: Integer): IIDEEditView;
var
  View: IOTAEditView;
begin
  View := FRaw.EditViews[Index];

  if Assigned(View) then begin
    Result := TToolsApiEditView.Create(View);
  end;
end;

function TToolsApiSourceEditor.GetEditViewCount: Integer;
begin
  Result := FRaw.EditViewCount;
end;

function TToolsApiSourceEditor.GetFileName: string;
begin
  Result := FRaw.FileName;
end;

function TToolsApiSourceEditor.GetModule: IIDEModule;
var
  Module: IOTAModule;
begin
  Module := FRaw.Module;

  if Assigned(Module) then begin
    Result := TToolsApiModule.Create(Module);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiProject.GetCompleteFileList(FileList: TStrings);
begin
  FRaw.GetCompleteFileList(FileList);
end;

//______________________________________________________________________________________________________________________

function TToolsApiModule.GetFileName: string;
begin
  Result := FRaw.FileName;
end;

function TToolsApiModule.GetSourceEditor: IIDESourceEditor;
var
  SourceEditor: IOTASourceEditor;
  I: Integer;
begin
  for I := 0 to FRaw.ModuleFileCount - 1 do begin
    if FRaw.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, SourceEditor) = S_OK then begin
      Result := TToolsApiSourceEditor.Create(SourceEditor);
      Break;
    end;
  end;
end;

procedure TToolsApiModule.Save(const ForceSave: Boolean);
begin
  FRaw.Save(False, ForceSave);
end;

//______________________________________________________________________________________________________________________

constructor TToolsApiNotifier<T>.Create(Handler: T);
begin
  inherited Create;
  FHandler := Handler;
end;

procedure TToolsApiNotifier<T>.AfterSave;
begin
  // Empty default implementation
end;

procedure TToolsApiNotifier<T>.BeforeSave;
begin
  // Empty default implementation
end;


procedure TToolsApiNotifier<T>.Destroyed;
begin
  FHandler.OnOwnerFreed.Notify(FHandler);
end;

procedure TToolsApiNotifier<T>.Modified;
begin
  // Empty default implementation
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  FHandler.OnViewActivated(TToolsApiEditView.Create(EditView));
end;

procedure TToolsApiEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation = opInsert then begin
    FHandler.OnViewAdded(TToolsApiEditView.Create(View));
  end;
end;

procedure TToolsApiEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
  // Empty implementation
end;

procedure TToolsApiEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
  // Empty implementation
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiViewNotifier.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  FHandler.OnBeginPaint(TToolsApiEditView.Create(View), FullRepaint);
end;

procedure TToolsApiViewNotifier.EditorIdle(const View: IOTAEditView);
begin
  // Empty implementation
end;

procedure TToolsApiViewNotifier.EndPaint(const View: IOTAEditView);
begin
  // Empty implementation
end;

procedure TToolsApiViewNotifier.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
  const TextWidth: Word; const LineAttributes: TOTAAttributeArray; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);
begin
  FHandler.OnPaintLine(TToolsApiEditView.Create(View), LineNumber, string(LineText), TextWidth, Canvas, TextRect, LineRect, CellSize);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiEditLineTracker.AddLine(const Line, Value: Integer);
begin
  FRaw.AddLine(Line, Value);
end;

function TToolsApiEditLineTracker.AddNotifier(Notifier: IIDEEditLineHandler): Integer;
begin
  Result := FRaw.AddNotifier(TToolsApiEditLineNotifier.Create(Notifier));
end;

procedure TToolsApiEditLineTracker.Clear;
var
  Index: Integer;
begin
  for Index := FRaw.Count - 1 downto 0 do begin
    FRaw.Delete(Index);
  end;
end;

function TToolsApiEditLineTracker.GetFileName: string;
begin
  Result := FRaw.GetEditBuffer.FileName;
end;

procedure TToolsApiEditLineTracker.RemoveNotifier(Index: Integer);
begin
  FRaw.RemoveNotifier(Index);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiEditLineNotifier.LineChanged(OldLine, NewLine, Data: Integer);
begin
  FHandler.OnLineChanged(OldLine, NewLine, Data);
end;

initialization
  SetLogger(BuildDelphiLintFileLogger);
  SetLintContext(TIDELintContext.Create);

end.
