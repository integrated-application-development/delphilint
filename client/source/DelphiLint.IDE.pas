unit DelphiLint.IDE;

interface

uses
    System.SysUtils
  , ToolsAPI
  , DelphiLint.Server
  , Vcl.Dialogs
  , Vcl.Graphics
  , WinAPI.Windows
  , System.Classes
  , System.Generics.Collections
  , DelphiLint.Data
  , DelphiLint.Logger
  , DockForm
  , DelphiLint.Events
  ;

type

//______________________________________________________________________________________________________________________

  TIDERefreshEvent = procedure(Issues: TArray<TLintIssue>);

  TLintIDE = class(TObject)
  private
    FServer: TLintServer;
    FActiveIssues: TDictionary<string, TList<TLintIssue>>;
    FLastAnalyzedFiles: TStringList;
    FOutputLog: TLintLogger;
    FAnalyzing: Boolean;
    FOnAnalysisComplete: TEventNotifier<TArray<TLintIssue>>;

    function ToUnixPath(Path: string; Lower: Boolean = False): string;
    function ToWindowsPath(Path: string): string;
    procedure OnAnalyzeResult(Issues: TArray<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure RefreshIssues(Issues: TArray<TLintIssue>);
    procedure DisplayIssues;
    function GetOrInitServer: TLintServer;

  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLintIssue>; overload;

    procedure AnalyzeActiveProject;

    property OnAnalysisComplete: TEventNotifier<TArray<TLintIssue>> read FOnAnalysisComplete;
  end;

//______________________________________________________________________________________________________________________

  TLintMenuItem = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  public type
    TMenuItemAction = reference to procedure;
  private
    FName: string;
    FCaption: string;
    FAction: TMenuItemAction;
  public
    constructor Create(Name: string; Caption: string; Action: TMenuItemAction);

    function GetIDstring: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: string;
  end;

  TLintEditorNotifier = class(TNotifierObject, IOTANotifier, IOTAEditorNotifier, INTAEditServicesNotifier)
  private
    FNotifiers: TDictionary<IOTAEditView, Integer>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ViewActivated(const View: IOTAEditView);
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);

    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

  TLintEditViewNotifier = class(TNotifierObject, IOTANotifier, INTAEditViewNotifier)
  private
    FRepaint: Boolean;
    procedure OnAnalysisComplete(const Issues: TArray<TLintIssue>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure EditorIdle(const View: IOTAEditView);
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
    procedure EndPaint(const View: IOTAEditView);
  end;

//______________________________________________________________________________________________________________________

procedure Register;

function LintIDE: TLintIDE;

implementation

uses
    System.StrUtils
  , DelphiLint.IDEUtils
  , System.Generics.Defaults
  , System.Math
  , DelphiLint.Settings
  , DelphiLint.ProjectOptions
  , System.IOUtils
  ;

var
  G_LintIDE: TLintIDE;
  G_EditorNotifier: Integer;

//______________________________________________________________________________________________________________________

procedure TLintIDE.AnalyzeActiveProject;
var
  AllFiles: TArray<string>;
  ProjectFile: string;
  MainFile: string;
  PasFiles: TArray<string>;
  Server: TLintServer;
  SourceEditor: IOTASourceEditor;
  ProjectOptions: TLintProjectOptions;
  ProjectDir: string;
begin
  SourceEditor := DelphiLint.IDEUtils.GetCurrentSourceEditor;
  if not Assigned(SourceEditor) then begin
    Exit;
  end;

  if not FAnalyzing then begin
    FAnalyzing := True;
    FOutputLog.Clear;
    FOutputLog.Info('Analysis in progress...');

    DelphiLint.IDEUtils.ExtractFiles(AllFiles, ProjectFile, MainFile, PasFiles);

    ProjectOptions := TLintProjectOptions.Create(ProjectFile);
    ProjectDir := ProjectOptions.ProjectBaseDir;
    if ProjectDir = '' then begin
      ProjectDir := TPath.GetDirectoryName(ProjectFile);
    end;

    FLastAnalyzedFiles.Clear;
    FLastAnalyzedFiles.Add(SourceEditor.FileName);

    Server := GetOrInitServer;
    if Assigned(Server) then begin
      Log.Info('Server connected for analysis.');
      Server.Analyze(
        ProjectDir,
        [SourceEditor.FileName, ProjectFile, MainFile],
        LintIDE.OnAnalyzeResult,
        LintIDE.OnAnalyzeError,
        ProjectOptions.SonarHostUrl,
        ProjectOptions.ProjectKey);
    end
    else begin
      Log.Info('Server connection could not be established.');
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  RegisterPackageWizard(TLintMenuItem.Create(
    'analyzeproject',
    'Analyze Project with DelphiLint',
    procedure begin
      LintIDE.AnalyzeActiveProject;
    end
  ));

  G_EditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(TLintEditorNotifier.Create);
end;

//______________________________________________________________________________________________________________________

constructor TLintIDE.Create;
begin
  inherited;
  FActiveIssues := TDictionary<string, TList<TLintIssue>>.Create;
  FOutputLog := TLintLogger.Create('Issues');
  FAnalyzing := False;
  FLastAnalyzedFiles := TStringList.Create;
  FOnAnalysisComplete := TEventNotifier<TArray<TLintIssue>>.Create;

  Log.Clear;
  Log.Info('DelphiLint started.');
end;

//______________________________________________________________________________________________________________________

destructor TLintIDE.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FActiveIssues);
  FreeAndNil(FOutputLog);
  FreeAndNil(FLastAnalyzedFiles);
  FreeAndNil(FOnAnalysisComplete);

  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.DisplayIssues;
var
  FileIssues: TArray<TLintIssue>;
  Issue: TLintIssue;
  FileName: string;
  Stale: Boolean;
begin
  FOutputLog.Clear;

  for FileName in FActiveIssues.Keys do begin
    FileIssues := GetIssues(FileName);
    FOutputLog.Title(Format('[DelphiLint] %s (%d issues)', [FileIssues[0].FilePath, Length(FileIssues)]));
    Stale := FLastAnalyzedFiles.IndexOf(FileName) = -1;

    for Issue in FileIssues do begin
      FOutputLog.Info(
        Format('%s%s', [Issue.Message, IfThen(Stale, ' (stale)', '')]),
        ToWindowsPath(DelphiLint.IDEUtils.GetProjectDirectory + '\' + Issue.FilePath),
        Issue.Range.StartLine,
        Issue.Range.StartLineOffset);
    end;
  end;

  RefreshEditorWindows;
end;

//______________________________________________________________________________________________________________________

function OrderByStartLine(const Left, Right: TLintIssue): Integer;
begin
  Result := TComparer<Integer>.Default.Compare(Left.Range.StartLine, Right.Range.StartLine);
end;

//______________________________________________________________________________________________________________________

function TLintIDE.GetIssues(FileName: string; Line: Integer = -1): TArray<TLintIssue>;
var
  BaseDir: string;
  SanitizedName: string;
  Issue: TLintIssue;
  ResultList: TList<TLintIssue>;
begin
  BaseDir := ToUnixPath(DelphiLint.IDEUtils.GetProjectDirectory, True);
  SanitizedName := ToUnixPath(FileName, True);

  if StartsText(BaseDir, SanitizedName) then begin
    SanitizedName := Copy(SanitizedName, Length(BaseDir) + 2);
  end;

  if FActiveIssues.ContainsKey(SanitizedName) then begin
    if Line = -1 then begin
      Result := FActiveIssues[SanitizedName].ToArray;
      TArray.Sort<TLintIssue>(Result, TComparer<TLintIssue>.Construct(OrderByStartLine));
    end
    else begin
      ResultList := TList<TLintIssue>.Create;

      for Issue in FActiveIssues[SanitizedName] do begin
        if (Issue.Range.StartLine >= Line) and (Issue.Range.EndLine <= Line) then begin
          ResultList.Add(Issue);
        end;
      end;

      ResultList.Sort(TComparer<TLintIssue>.Construct(OrderByStartLine));
      Result := ResultList.ToArray;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintIDE.GetOrInitServer: TLintServer;
begin
  if not Assigned(FServer) then begin
    try
      FServer := TLintServer.Create(LintSettings.ServerPort);
    except
      ShowMessage('Server connection could not be established.');
      FServer := nil;
    end;
  end;
  Result := FServer;
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.OnAnalyzeError(Message: string);
begin
  FAnalyzing := False;
  FOutputLog.Clear;
  FOutputLog.Info('Error during analysis: ' + Message);
  ShowMessage('There was an error during analysis.' + #13#10 + Message);
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.OnAnalyzeResult(Issues: TArray<TLintIssue>);
begin
  FAnalyzing := False;
  RefreshIssues(Issues);
  FOnAnalysisComplete.Notify(Issues);
  DisplayIssues;
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.RefreshIssues(Issues: TArray<TLintIssue>);
var
  Issue: TLintIssue;
  SanitizedPath: string;
begin
  Log.Info(Format('Processing %d issues.', [Length(Issues)]));

  FLastAnalyzedFiles.Clear;

  for Issue in Issues do begin
    SanitizedPath := ToUnixPath(Issue.FilePath, True);

    if FLastAnalyzedFiles.IndexOf(SanitizedPath) = -1 then begin
      // This is the first issue in this file that we've found this run
      FLastAnalyzedFiles.Add(SanitizedPath);

      if FActiveIssues.ContainsKey(SanitizedPath) then begin
        // This filepath has old issues in it, so we want to clear it out
        FActiveIssues[SanitizedPath].Clear;
      end
      else begin
        // There's no space allocated for this filepath - allocate it
        FActiveIssues.Add(SanitizedPath, TList<TLintIssue>.Create);
      end;
    end;

    FActiveIssues[SanitizedPath].Add(Issue);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintIDE.ToUnixPath(Path: string; Lower: Boolean = False): string;
begin
  if Lower then begin
    Path := LowerCase(Path);
  end;

  Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

function TLintIDE.ToWindowsPath(Path: string): string;
begin
  Result := StringReplace(Path, '/', '\', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

function LintIDE: TLintIDE;
begin
  if not Assigned(G_LintIDE) then begin
    G_LintIDE := TLintIDE.Create;
  end;

  Result := G_LintIDE;
end;

//______________________________________________________________________________________________________________________

constructor TLintMenuItem.Create(Name: string; Caption: string; Action: TMenuItemAction);
begin
  FName := Name;
  FCaption := Caption;
  FAction := Action;
end;

//______________________________________________________________________________________________________________________

procedure TLintMenuItem.Execute;
begin
  FAction;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetIDstring: string;
begin
  Result := 'DelphiLint|' + FName;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetMenuText: string;
begin
  Result := FCaption;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetName: string;
begin
  Result := FName;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//______________________________________________________________________________________________________________________

constructor TLintEditorNotifier.Create;
begin
  FNotifiers := TDictionary<IOTAEditView, Integer>.Create;
  Log.Info('Editor notifier created');
end;

//______________________________________________________________________________________________________________________

destructor TLintEditorNotifier.Destroy;
var
  View: IOTAEditView;
begin
  for View in FNotifiers.Keys do begin
    if Assigned(View) then begin
      View.RemoveNotifier(FNotifiers[View]);
    end;
  end;

  FreeAndNil(FNotifiers);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
  // Exposed for interface
end;

//______________________________________________________________________________________________________________________

procedure TLintEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation = opInsert then begin
    Log.Info('View created');
    FNotifiers.Add(View, View.AddNotifier(TLintEditViewNotifier.Create));
  end
  else if FNotifiers.ContainsKey(View) then begin
    Log.Info('View removed');
    FNotifiers.Remove(View);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Exposed for interface
end;

constructor TLintEditViewNotifier.Create;
begin
  FRepaint := False;
  LintIDE.OnAnalysisComplete.AddListener(OnAnalysisComplete);
end;

destructor TLintEditViewNotifier.Destroy;
begin
  LintIDE.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);
  inherited;
end;

procedure TLintEditViewNotifier.OnAnalysisComplete(const Issues: TArray<TLintIssue>);
begin
  FRepaint := True;
end;

procedure TLintEditViewNotifier.EditorIdle(const View: IOTAEditView);
begin
  if FRepaint then begin
    View.GetEditWindow.Form.Repaint;
  end;
end;

procedure TLintEditViewNotifier.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  if FRepaint then begin
    FullRepaint := True;
    FRepaint := False;
  end;
end;

procedure TLintEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  Log.Info('Editor view activated');
  if not FNotifiers.ContainsKey(EditView) then begin
    Log.Info('View created');
    FNotifiers.Add(EditView, EditView.AddNotifier(TLintEditViewNotifier.Create));
  end;
end;

procedure TLintEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  Log.Info('Editor view modified');
end;

procedure TLintEditViewNotifier.EndPaint(const View: IOTAEditView);
begin
  // Exposed for interface
end;

procedure TLintEditViewNotifier.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
  const TextWidth: Word; const LineAttributes: TOTAAttributeArray; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);

  function ColumnToPx(const Col: Integer): Integer;
  begin
    Result := TextRect.Left + (Col + 1 - View.LeftColumn) * CellSize.Width;
  end;

  procedure DrawLine(const StartChar: Integer; const EndChar: Integer);
  var
    StartX: Integer;
    EndX: Integer;
  begin
    Canvas.Pen.Color := clWebGold;
    Canvas.Pen.Width := 1;

    StartX := Max(ColumnToPx(StartChar), TextRect.Left);
    EndX := Max(ColumnToPx(EndChar), TextRect.Left);

    Canvas.MoveTo(StartX, TextRect.Bottom - 1);
    Canvas.LineTo(EndX, TextRect.Bottom - 1);
  end;

  procedure DrawMessage(const Msg: string);
  begin
    Canvas.Font.Color := clWebGold;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(LineRect.Left + (2 * CellSize.Width), LineRect.Top, '!');
    Canvas.TextOut(TextRect.Right, TextRect.Top, Msg);
  end;

var
  CurrentModule: IOTAModule;
  Issues: TArray<TLintIssue>;
  Issue: TLintIssue;
  Msg: string;
begin
  CurrentModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Issues := LintIDE.GetIssues(CurrentModule.FileName, LineNumber);

  if Length(Issues) > 0 then begin
    for Issue in Issues do begin
      Msg := Msg + ' - ' + Issue.Message;
      DrawLine(Issue.Range.StartLineOffset, Issue.Range.EndLineOffset);
    end;

    DrawMessage(Msg);
  end;
end;

procedure TLintEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
  // Exposed for interface
end;

initialization

finalization
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(G_EditorNotifier);
  FreeAndNil(G_LintIDE);

end.
