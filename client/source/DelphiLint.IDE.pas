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
  ;

type

//______________________________________________________________________________________________________________________

  TLintIDE = class(TObject)
  private
    FServer: TLintServer;
    FActiveIssues: TDictionary<string, TList<TLintIssue>>;
    FLastAnalyzedFiles: TStringList;
    FOutputLog: TLintLogger;
    FAnalyzing: Boolean;

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
  public
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
  , DelphiLint.FileUtils
  , System.Generics.Defaults
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
begin
  if not FAnalyzing then begin
    FAnalyzing := True;
    FOutputLog.Clear;
    FOutputLog.Info('Analysis in progress...');

    DelphiLint.FileUtils.ExtractFiles(AllFiles, ProjectFile, MainFile, PasFiles);

    FLastAnalyzedFiles.Clear;
    FLastAnalyzedFiles.AddStrings(AllFiles);

    Server := GetOrInitServer;
    if Assigned(Server) then begin
      Log.Info('Server connected for analysis.');
      Server.Analyze(
        DelphiLint.FileUtils.GetProjectDirectory(MainFile),
        AllFiles,
        LintIDE.OnAnalyzeResult,
        LintIDE.OnAnalyzeError);
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
        ToWindowsPath(DelphiLint.FileUtils.GetProjectDirectory + '\' + Issue.FilePath),
        Issue.Range.StartLine,
        Issue.Range.StartLineOffset);
    end;
  end;
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
  BaseDir := ToUnixPath(DelphiLint.FileUtils.GetProjectDirectory, True);
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
      FServer := TLintServer.Create('{URL REMOVED}');
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
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.OnAnalyzeResult(Issues: TArray<TLintIssue>);
begin
  FAnalyzing := False;
  RefreshIssues(Issues);
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

procedure TLintEditViewNotifier.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TLintEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TLintEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TLintEditViewNotifier.EditorIdle(const View: IOTAEditView);
begin
  // Exposed for interface
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

  procedure DrawLine(const StartChar: Integer; const EndChar: Integer);
  begin
    Canvas.Pen.Color := clWebGold;
    Canvas.Pen.Width := 2;
    Canvas.MoveTo(TextRect.Left + (StartChar * CellSize.Width), TextRect.Bottom - 1);
    Canvas.LineTo(TextRect.Left + (EndChar * CellSize.Width), TextRect.Bottom - 1);
  end;

var
  CurrentModule: IOTAModule;
  Issues: TArray<TLintIssue>;
  Issue: TLintIssue;
begin
  CurrentModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Issues := LintIDE.GetIssues(CurrentModule.FileName, LineNumber);

  for Issue in Issues do begin
    DrawLine(Issue.Range.StartLineOffset, Issue.Range.EndLineOffset);
  end;
end;

procedure TLintEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin

end;

procedure TLintEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin

end;

procedure TLintEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin

end;

procedure TLintEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin

end;

initialization

finalization
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(G_EditorNotifier);
  FreeAndNil(G_LintIDE);

end.
