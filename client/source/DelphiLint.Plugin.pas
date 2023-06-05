unit DelphiLint.Plugin;

interface

uses
    DelphiLint.Server
  , System.Classes
  , DelphiLint.Data
  , System.Generics.Collections
  , DelphiLint.Logger
  , DelphiLint.Events
  ;

type
  TLintPlugin = class(TObject)
  private
    FServer: TLintServer;
    FActiveIssues: TDictionary<string, TList<TLintIssue>>;
    FLastAnalyzedFiles: TStringList;
    FOutputLog: TLintLogger;
    FAnalyzing: Boolean;
    FOnAnalysisComplete: TEventNotifier<TArray<TLintIssue>>;

    function ToUnixPath(Path: string; Lower: Boolean = False): string;
    procedure OnAnalyzeResult(Issues: TArray<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure RefreshIssues(Issues: TArray<TLintIssue>);
    procedure DisplayIssues;
    function GetOrInitServer: TLintServer;

  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLintIssue>; overload;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeFiles(
      const Files: TArray<string>;
      const BaseDir: string;
      const SonarHostUrl: string = '';
      const ProjectKey: string = '');
    procedure AnalyzeActiveFile;

    property OnAnalysisComplete: TEventNotifier<TArray<TLintIssue>> read FOnAnalysisComplete;
  end;

function Plugin: TLintPlugin;

implementation

uses
    ToolsAPI
  , DelphiLint.ProjectOptions
  , DelphiLint.IDEUtils
  , System.IOUtils
  , System.SysUtils
  , System.StrUtils
  , System.Generics.Defaults
  , DelphiLint.Settings
  , Vcl.Dialogs
  ;

var
  GLintPlugin: TLintPlugin;


function Plugin: TLintPlugin;
begin
  if not Assigned(GLintPlugin) then begin
    GLintPlugin := TLintPlugin.Create;
  end;

  Result := GLintPlugin;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.AnalyzeActiveFile;
var
  AllFiles: TArray<string>;
  ProjectFile: string;
  MainFile: string;
  PasFiles: TArray<string>;
  SourceEditor: IOTASourceEditor;
  ProjectOptions: TLintProjectOptions;
  ProjectDir: string;
begin
  SourceEditor := DelphiLint.IDEUtils.GetCurrentSourceEditor;
  if not Assigned(SourceEditor) then begin
    Exit;
  end;

  DelphiLint.IDEUtils.ExtractFiles(AllFiles, ProjectFile, MainFile, PasFiles);

  ProjectOptions := TLintProjectOptions.Create(ProjectFile);
  ProjectDir := ProjectOptions.ProjectBaseDir;
  if ProjectDir = '' then begin
    ProjectDir := TPath.GetDirectoryName(ProjectFile);
  end;

  AnalyzeFiles(
    [ProjectFile, SourceEditor.FileName],
    ProjectDir,
    ProjectOptions.SonarHostUrl,
    ProjectOptions.ProjectKey);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.AnalyzeFiles(
  const Files: TArray<string>;
  const BaseDir: string;
  const SonarHostUrl: string = '';
  const ProjectKey: string = '');
var
  Server: TLintServer;
  FilePath: string;
begin
  if FAnalyzing then begin
    Log.Info('Already in analysis.');
    Exit;
  end;

  FAnalyzing := True;
  FOutputLog.Clear;
  FOutputLog.Info('Analysis in progress...');

  FLastAnalyzedFiles.Clear;
  for FilePath in Files do begin
    if DelphiLint.IDEUtils.IsPasFile(FilePath) or DelphiLint.IDEUtils.IsMainFile(FilePath) then begin
      FLastAnalyzedFiles.Add(FilePath);
    end;
  end;

  Server := GetOrInitServer;
  if Assigned(Server) then begin
    Log.Info('Server connected for analysis.');
    Server.Analyze(
      BaseDir,
      Files,
      OnAnalyzeResult,
      OnAnalyzeError,
      SonarHostUrl,
      ProjectKey);
  end
  else begin
    Log.Info('Server connection could not be established.');
    FOutputLog.Info('Analysis failed - server connection could not be established.');
  end;
end;

//______________________________________________________________________________________________________________________

constructor TLintPlugin.Create;
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

destructor TLintPlugin.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FActiveIssues);
  FreeAndNil(FOutputLog);
  FreeAndNil(FLastAnalyzedFiles);
  FreeAndNil(FOnAnalysisComplete);

  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.DisplayIssues;
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
        Issue.FilePath,
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

function TLintPlugin.GetIssues(FileName: string; Line: Integer = -1): TArray<TLintIssue>;
var
  SanitizedName: string;
  Issue: TLintIssue;
  ResultList: TList<TLintIssue>;
begin
  SanitizedName := ToUnixPath(FileName, True);
  if FActiveIssues.ContainsKey(SanitizedName) then begin
    if Line = -1 then begin
      Result := FActiveIssues[SanitizedName].ToArray;
      TArray.Sort<TLintIssue>(Result, TComparer<TLintIssue>.Construct(OrderByStartLine));
    end
    else begin
      ResultList := TList<TLintIssue>.Create;
      try
        for Issue in FActiveIssues[SanitizedName] do begin
          if (Issue.Range.StartLine >= Line) and (Issue.Range.EndLine <= Line) then begin
            ResultList.Add(Issue);
          end;
        end;

        ResultList.Sort(TComparer<TLintIssue>.Construct(OrderByStartLine));
        Result := ResultList.ToArray;
      finally
        FreeAndNil(ResultList);
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintPlugin.GetOrInitServer: TLintServer;
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

procedure TLintPlugin.OnAnalyzeError(Message: string);
begin
  FAnalyzing := False;
  FOutputLog.Clear;
  FOutputLog.Info('Error during analysis: ' + Message);
  ShowMessage('There was an error during analysis.' + #13#10 + Message);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnAnalyzeResult(Issues: TArray<TLintIssue>);
begin
  FAnalyzing := False;
  RefreshIssues(Issues);
  FOnAnalysisComplete.Notify(Issues);
  DisplayIssues;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.RefreshIssues(Issues: TArray<TLintIssue>);
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

function TLintPlugin.ToUnixPath(Path: string; Lower: Boolean = False): string;
begin
  if Lower then begin
    Path := LowerCase(Path);
  end;

  Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.UpdateIssueLine(FilePath: string; OriginalLine, NewLine: Integer);
var
  SanitizedPath: string;
  Issue: TLintIssue;
  Delta: Integer;
  Index: Integer;
  NewRange: TRange;
begin
  SanitizedPath := ToUnixPath(FilePath, True);

  Delta := NewLine - OriginalLine;

  Log.Info(Format('Updating line from %d to %d', [OriginalLine, NewLine]));

  if FActiveIssues.ContainsKey(SanitizedPath) then begin
    for Index := 0 to FActiveIssues[SanitizedPath].Count - 1 do begin
      Issue := FActiveIssues[SanitizedPath][Index];

      if Issue.Range.StartLine = OriginalLine then begin
        NewRange := Issue.Range;
        NewRange.StartLine := NewRange.StartLine + Delta;
        NewRange.EndLine := NewRange.EndLine + Delta;
        Issue.Range := NewRange;
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(GLintPlugin);

end.
