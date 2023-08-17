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
  , DelphiLint.Events
  , DelphiLint.Data
  , DelphiLint.IDEBaseTypes
  , DelphiLint.Context
  ;

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
    function AddEditorNotifier(Notifier: TEditorNotifierBase): Integer;
    procedure RemoveEditorNotifier(const Index: Integer);

    // From INTAEnvironmentOptionsServices
    procedure RegisterAddInOptions(const Options: TAddInOptionsBase);
    procedure UnregisterAddInOptions(const Options: TAddInOptionsBase);
  end;

  TIDELintContext = class(TInterfacedObject, ILintContext)
  private
    FAnalyzer: IAnalyzer;
    FLogger: ILogger;
    FIDEServices: IIDEServices;
  protected
    function GetAnalyzer: IAnalyzer;
    function GetLogger: ILogger;
    function GetIDEServices: IIDEServices;
  end;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , DelphiLint.Analyzer
  , DelphiLint.Logger
  , ToolsAPI
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

function TIDELintContext.GetLogger: ILogger;
var
  LogDir: string;
  LogPath: string;
begin
  if not Assigned(FLogger) then begin
    LogDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint\logs');
    TDirectory.CreateDirectory(LogDir);
    LogPath := TPath.Combine(LogDir, Format('delphilint-client_%s.log', [FormatDateTime('yyyymmdd_hhnnss', Now)]));
    FLogger := TFileLogger.Create(LogPath);
  end;

  Result := FLogger;
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

function TToolsApiServices.AddPluginInfo(const Title, Description: string; Image: TBitmap): Integer;
begin
  Result := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(Title, Description, Image.Handle);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.RemovePluginInfo(const Index: Integer);
begin
  (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(Index);
end;

//______________________________________________________________________________________________________________________

procedure TToolsApiServices.AddPluginBitmap(const Caption: string; Image: TBitmap; IsUnregistered: Boolean;
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

function TToolsApiServices.AddEditorNotifier(Notifier: TEditorNotifierBase): Integer;
begin
  Result := (BorlandIDEServices as IOTAEditorServices).AddNotifier(Notifier);
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

procedure TToolsApiEditView.GoToPosition(const Line, Column: Integer);
begin
  FRaw.Buffer.EditPosition.GotoLine(Line);
  FRaw.Buffer.EditPosition.Column
end;

procedure TToolsApiEditView.Paint;
begin
  FRaw.Paint;
end;

//______________________________________________________________________________________________________________________

constructor TToolsApiWrapper<T>.Create(Raw: T);
begin
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

initialization
  SetLintContext(TIDELintContext.Create);

end.
