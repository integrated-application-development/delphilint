{
DelphiLint Client for RAD Studio
Copyright (C) 2023 Integrated Application Development

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.Context;

interface

uses
    DelphiLint.Server
  , System.Classes
  , DelphiLint.Data
  , System.Generics.Collections
  , DelphiLint.Logger
  , DelphiLint.Events
  , ToolsAPI
  , DelphiLint.IDE
  ;

type
  TLiveIssue = class(TObject)
  private
    FRuleKey: string;
    FMessage: string;
    FFilePath: string;
    FStartLine: Integer;
    FEndLine: Integer;
    FStartLineOffset: Integer;
    FEndLineOffset: Integer;
    FLinesMoved: Integer;

    function GetStartLine: Integer;
    function GetEndLine: Integer;

  public
    property RuleKey: string read FRuleKey;
    property Message: string read FMessage;
    property FilePath: string read FFilePath write FFilePath;
    property OriginalStartLine: Integer read FStartLine;
    property OriginalEndLine: Integer read FEndLine;
    property StartLine: Integer read GetStartLine;
    property EndLine: Integer read GetEndLine;
    property StartLineOffset: Integer read FStartLineOffset;
    property EndLineOffset: Integer read FEndLineOffset;
    property LinesMoved: Integer read FLinesMoved write FLinesMoved;

    constructor CreateFromData(Issue: TLintIssue);
    procedure NewLineMoveSession;
  end;

  TFileAnalysisHistory = record
    AnalysisTime: TDateTime;
    Success: Boolean;
    IssuesFound: Integer;
    FileHash: string;
  end;

  TCurrentAnalysis = class(TObject)
  private
    FPaths: TArray<string>;
  public
    constructor Create(Paths: TArray<string>);
    property Paths: TArray<string> read FPaths;
  end;

  TFileAnalysisStatus = (
    fasNeverAnalyzed,
    fasOutdatedAnalysis,
    fasUpToDateAnalysis
  );

  TLintContext = class(TObject)
  private
    FServer: TLintServer;
    FActiveIssues: TObjectDictionary<string, TObjectList<TLiveIssue>>;
    FFileAnalyses: TDictionary<string, TFileAnalysisHistory>;
    FOutputLog: TLintLogger;
    FCurrentAnalysis: TCurrentAnalysis;
    FOnAnalysisStarted: TEventNotifier<TArray<string>>;
    FOnAnalysisComplete: TEventNotifier<TArray<string>>;
    FOnAnalysisFailed: TEventNotifier<TArray<string>>;
    FSaveBlockers: TDictionary<IOTAModule, TLintModuleSaveBlocker>;

    function ToUnixPath(Path: string; Lower: Boolean = False): string;
    procedure OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure SaveIssues(Issues: TObjectList<TLintIssue>);
    procedure DisplayIssues;
    function GetOrInitServer: TLintServer;
    procedure RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);
    function GetInAnalysis: Boolean;

    procedure BlockSavingForAnalysisFiles;
    procedure UnblockSavingForAnalysisFiles;

  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>; overload;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeFiles(
      const Files: TArray<string>;
      const BaseDir: string;
      const SonarHostUrl: string = '';
      const ProjectKey: string = '');
    procedure AnalyzeActiveFile;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;

    property OnAnalysisStarted: TEventNotifier<TArray<string>> read FOnAnalysisStarted;
    property OnAnalysisComplete: TEventNotifier<TArray<string>> read FOnAnalysisComplete;
    property OnAnalysisFailed: TEventNotifier<TArray<string>> read FOnAnalysisFailed;

    property CurrentAnalysis: TCurrentAnalysis read FCurrentAnalysis;
    property InAnalysis: Boolean read GetInAnalysis;
  end;

function LintContext: TLintContext;
function LintContextValid: Boolean;

implementation

uses
    DelphiLint.ProjectOptions
  , DelphiLint.IDEUtils
  , System.IOUtils
  , System.SysUtils
  , System.StrUtils
  , System.Generics.Defaults
  , DelphiLint.Settings
  , Vcl.Dialogs
  , System.Hash
  , System.DateUtils
  ;

var
  GLintContext: TLintContext;
  GContextInvalid: Boolean;

//______________________________________________________________________________________________________________________

function LintContext: TLintContext;
begin
  if LintContextValid and not Assigned(GLintContext) then begin
    GLintContext := TLintContext.Create;
  end;
  Result := GLintContext;
end;

//______________________________________________________________________________________________________________________

function LintContextValid: Boolean;
begin
  Result := not GContextInvalid;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.BlockSavingForAnalysisFiles;

  procedure BlockSave(Module: IOTAModule);
  var
    SaveBlocker: TLintModuleSaveBlocker;
    NotfIndex: Integer;
  begin
    SaveBlocker := TLintModuleSaveBlocker.Create(
      'Please wait for DelphiLint analysis to complete before saving this file.');
    NotfIndex := Module.AddNotifier(SaveBlocker);
    FSaveBlockers.Add(Module, SaveBlocker);

    SaveBlocker.OnReleased.AddListener(
      procedure(const Notf: TNotifierBase) begin
        Module.RemoveNotifier(NotfIndex);
      end);
    SaveBlocker.OnOwnerFreed.AddListener(
      procedure(const Notf: TNotifierBase) begin
        FSaveBlockers.Remove(Module);
      end);
  end;

var
  FilePath: string;
  Module: IOTAModule;
begin
  for FilePath in FCurrentAnalysis.FPaths do begin
    Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FilePath);
    if not Assigned(Module) then begin
      Log.Info('Module could not be found for path ' + FilePath + ', saving will not be disabled.');
      Continue;
    end;
    BlockSave(Module);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.UnblockSavingForAnalysisFiles;

  procedure UnblockSave(Module: IOTAModule);
  begin
    if FSaveBlockers.ContainsKey(Module) then begin
      FSaveBlockers.Remove(Module);
    end
    else begin
      Log.Info('Attempted to unblock saving for a file that was never blocked.');
    end;
  end;

var
  FilePath: string;
  Module: IOTAModule;
begin
  for FilePath in FCurrentAnalysis.FPaths do begin
    Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FilePath);
    if not Assigned(Module) then begin
      Log.Info('Module could not be found for path ' + FilePath + ', saving will not be re-enabled.');
      Continue;
    end;
    UnblockSave(Module);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.AnalyzeActiveFile;
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
    [SourceEditor.FileName, ProjectFile],
    ProjectDir,
    ProjectOptions.SonarHostUrl,
    ProjectOptions.ProjectKey);
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.AnalyzeFiles(
  const Files: TArray<string>;
  const BaseDir: string;
  const SonarHostUrl: string = '';
  const ProjectKey: string = '');
var
  Server: TLintServer;
begin
  if Assigned(FCurrentAnalysis) then begin
    Log.Info('Already in analysis.');
    Exit;
  end;

  FOutputLog.Clear;
  FCurrentAnalysis := TCurrentAnalysis.Create(Files);
  FOnAnalysisStarted.Notify(Files);
  BlockSavingForAnalysisFiles;

  Server := GetOrInitServer;
  if Assigned(Server) then begin
    Server.Analyze(
      BaseDir,
      Files,
      OnAnalyzeResult,
      OnAnalyzeError,
      SonarHostUrl,
      ProjectKey);
  end
  else begin
    UnblockSavingForAnalysisFiles;
    FOutputLog.Info('Analysis failed - server connection could not be established.');
    FOnAnalysisFailed.Notify(Files);
  end;
end;

//______________________________________________________________________________________________________________________

constructor TLintContext.Create;
begin
  inherited;
  FActiveIssues := TObjectDictionary<string, TObjectList<TLiveIssue>>.Create;
  FOutputLog := TLintLogger.Create('Issues');
  FCurrentAnalysis := nil;
  FFileAnalyses := TDictionary<string, TFileAnalysisHistory>.Create;
  FOnAnalysisStarted := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisComplete := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisFailed := TEventNotifier<TArray<string>>.Create;
  FSaveBlockers := TDictionary<IOTAModule, TLintModuleSaveBlocker>.Create;

  Log.Clear;
  Log.Info('Context initialised.');
end;

//______________________________________________________________________________________________________________________

destructor TLintContext.Destroy;
var
  SaveBlocker: TLintModuleSaveBlocker;
begin
  for SaveBlocker in FSaveBlockers.Values do begin
    SaveBlocker.Release;
  end;
  FreeAndNil(FSaveBlockers);

  FreeAndNil(FServer);
  FreeAndNil(FActiveIssues);
  FreeAndNil(FOutputLog);
  FreeAndNil(FFileAnalyses);
  FreeAndNil(FOnAnalysisStarted);
  FreeAndNil(FOnAnalysisComplete);
  FreeAndNil(FOnAnalysisFailed);
  FreeAndNil(FCurrentAnalysis);

  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.DisplayIssues;
var
  FileIssues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
  FileName: string;
  Stale: Boolean;
begin
  FOutputLog.Clear;

  for FileName in FActiveIssues.Keys do begin
    FileIssues := GetIssues(FileName);
    FOutputLog.Title(Format('[DelphiLint] %s (%d issues)', [FileIssues[0].FilePath, Length(FileIssues)]));
    Stale := GetAnalysisStatus(FileName) = fasOutdatedAnalysis;

    for Issue in FileIssues do begin
      FOutputLog.Info(
        Format('%s%s', [Issue.Message, IfThen(Stale, ' (outdated)', '')]),
        Issue.FilePath,
        Issue.StartLine,
        Issue.StartLineOffset);
    end;
  end;

  RefreshEditorWindows;
end;

//______________________________________________________________________________________________________________________

function OrderByStartLine(const Left, Right: TLiveIssue): Integer;
begin
  Result := TComparer<Integer>.Default.Compare(Left.OriginalStartLine, Right.OriginalStartLine);
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetInAnalysis: Boolean;
begin
  Result := Assigned(FCurrentAnalysis);
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>;
var
  SanitizedName: string;
  Issue: TLiveIssue;
  ResultList: TList<TLiveIssue>;
begin
  SanitizedName := ToUnixPath(FileName, True);
  if FActiveIssues.ContainsKey(SanitizedName) then begin
    if Line = -1 then begin
      Result := FActiveIssues[SanitizedName].ToArray;
      TArray.Sort<TLiveIssue>(Result, TComparer<TLiveIssue>.Construct(OrderByStartLine));
    end
    else begin
      ResultList := TList<TLiveIssue>.Create;
      try
        for Issue in FActiveIssues[SanitizedName] do begin
          if (Issue.StartLine >= Line) and (Issue.EndLine <= Line) then begin
            ResultList.Add(Issue);
          end;
        end;

        ResultList.Sort(TComparer<TLiveIssue>.Construct(OrderByStartLine));
        Result := ResultList.ToArray;
      finally
        FreeAndNil(ResultList);
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetOrInitServer: TLintServer;
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

function TLintContext.GetAnalysisStatus(Path: string): TFileAnalysisStatus;
var
  SanitizedPath: string;
  History: TFileAnalysisHistory;
begin
  SanitizedPath := ToUnixPath(Path, True);

  if FFileAnalyses.ContainsKey(SanitizedPath) then begin
    History := FFileAnalyses[SanitizedPath];
    if THashMD5.GetHashStringFromFile(Path) = History.FileHash then begin
      Result := TFileAnalysisStatus.fasUpToDateAnalysis;
    end
    else begin
      Result := TFileAnalysisStatus.fasOutdatedAnalysis;
    end;
  end
  else begin
    Result := TFileAnalysisStatus.fasNeverAnalyzed;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.OnAnalyzeError(Message: string);
var
  Path: string;
begin
  UnblockSavingForAnalysisFiles;

  FOutputLog.Info('Error during analysis: ' + Message);

  for Path in FCurrentAnalysis.Paths do begin
    RecordAnalysis(Path, False, 0);
  end;

  FOnAnalysisFailed.Notify(FCurrentAnalysis.Paths);

  FreeAndNil(FCurrentAnalysis);

  ShowMessage('There was an error during analysis.' + #13#10 + Message);
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
begin
  try
    SaveIssues(Issues);
  finally
    FreeAndNil(Issues);
  end;

  FOnAnalysisComplete.Notify(FCurrentAnalysis.Paths);
  FreeAndNil(FCurrentAnalysis);

  DisplayIssues;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);
var
  SanitizedPath: string;
  History: TFileAnalysisHistory;
begin
  History.AnalysisTime := Now;
  History.Success := Success;
  History.IssuesFound := IssuesFound;
  History.FileHash := THashMD5.GetHashStringFromFile(Path);

  SanitizedPath := ToUnixPath(Path, True);
  FFileAnalyses.AddOrSetValue(SanitizedPath, History);

  Log.Info(Format(
    'Analysis recorded for %s at %s, (%s, %d issues found)',
    [
      Path,
      FormatDateTime('hh:nn:ss', History.AnalysisTime),
      IfThen(Success, 'successful', 'failure'),
      IssuesFound
    ]));
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.SaveIssues(Issues: TObjectList<TLintIssue>);
var
  Issue: TLintIssue;
  LiveIssue: TLiveIssue;
  SanitizedPath: string;
  NewIssues: TDictionary<string, TObjectList<TLiveIssue>>;
  Path: string;
  NewIssuesForFile: TObjectList<TLiveIssue>;
  IssueCount: Integer;
begin
  NewIssues := TDictionary<string, TObjectList<TLiveIssue>>.Create;
  try
    // Split issues by file and convert to live issues
    for Issue in Issues do begin
      LiveIssue := TLiveIssue.CreateFromData(Issue);

      SanitizedPath := ToUnixPath(Issue.FilePath, True);
      if not NewIssues.ContainsKey(SanitizedPath) then begin
        NewIssues.Add(SanitizedPath, TObjectList<TLiveIssue>.Create);
      end;
      NewIssues[SanitizedPath].Add(LiveIssue);
    end;

    // Process issues per file
    for Path in FCurrentAnalysis.Paths do begin
      SanitizedPath := ToUnixPath(Path, True);

      // Remove current active issues
      if FActiveIssues.ContainsKey(SanitizedPath) then begin
        FActiveIssues.Remove(SanitizedPath);
      end;

      // Add new active issues (if there are any)
      IssueCount := 0;
      if NewIssues.TryGetValue(SanitizedPath, NewIssuesForFile) then begin
        FActiveIssues.Add(SanitizedPath, NewIssuesForFile);
        IssueCount := FActiveIssues[SanitizedPath].Count;
      end;

      // Record analysis
      RecordAnalysis(Path, True, IssueCount);
    end;
  finally
    FreeAndNil(NewIssues);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.ToUnixPath(Path: string; Lower: Boolean = False): string;
begin
  if Lower then begin
    Path := LowerCase(Path);
  end;

  Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

function TLintContext.TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
begin
  Result := FFileAnalyses.TryGetValue(ToUnixPath(Path, True), History);
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.UpdateIssueLine(FilePath: string; OriginalLine, NewLine: Integer);
var
  SanitizedPath: string;
  Issue: TLiveIssue;
  Delta: Integer;
  Index: Integer;
begin
  SanitizedPath := ToUnixPath(FilePath, True);
  Delta := NewLine - OriginalLine;

  if FActiveIssues.ContainsKey(SanitizedPath) then begin
    for Index := 0 to FActiveIssues[SanitizedPath].Count - 1 do begin
      Issue := FActiveIssues[SanitizedPath][Index];

      if Issue.OriginalStartLine = OriginalLine then begin
        Issue.LinesMoved := Delta;
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TLiveIssue.CreateFromData(Issue: TLintIssue);
begin
  FRuleKey := Issue.RuleKey;
  FMessage := Issue.Message;
  FFilePath := Issue.FilePath;
  FStartLine := Issue.Range.StartLine;
  FEndLine := Issue.Range.EndLine;
  FStartLineOffset := Issue.Range.StartLineOffset;
  FEndLineOffset := Issue.Range.EndLineOffset;
  FLinesMoved := 0;
end;

function TLiveIssue.GetStartLine: Integer;
begin
  Result := FStartLine + LinesMoved;
end;

function TLiveIssue.GetEndLine: Integer;
begin
  Result := FEndLine + LinesMoved;
end;

procedure TLiveIssue.NewLineMoveSession;
begin
  FStartLine := StartLine;
  FEndLine := EndLine;
  FLinesMoved := 0;
end;

{ TCurrentAnalysis }

constructor TCurrentAnalysis.Create(Paths: TArray<string>);
begin
  FPaths := Paths;
end;

initialization
  GContextInvalid := False;

finalization
  FreeAndNil(GLintContext);
  GContextInvalid := True;

end.
