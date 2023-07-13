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
  , DelphiLint.Events
  , System.SysUtils
  , System.SyncObjs
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
    FTethered: Boolean;
    FLines: TList<string>;

    function GetStartLine: Integer;
    function GetEndLine: Integer;
    procedure PopulateLines(const FileLines: TArray<string>);
  public
    constructor Create(Issue: TLintIssue; FileLines: TArray<string>);
    destructor Destroy; override;

    procedure NewLineMoveSession;
    procedure UpdateTether(LineNum: Integer; LineText: string);
    procedure Untether;

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
    property Tethered: Boolean read FTethered;
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
    function IncludesFile(const Path: string): Boolean;

    property Paths: TArray<string> read FPaths;
  end;

  TFileAnalysisStatus = (
    fasNeverAnalyzed,
    fasOutdatedAnalysis,
    fasUpToDateAnalysis
  );

  TLintContext = class(TObject)
  private const
    C_ErrorTitle = 'DelphiLint error';
  private
    FServer: TLintServer;
    FActiveIssues: TObjectDictionary<string, TObjectList<TLiveIssue>>;
    FFileAnalyses: TDictionary<string, TFileAnalysisHistory>;
    FRules: TObjectDictionary<string, TRule>;
    FCurrentAnalysis: TCurrentAnalysis;
    FOnAnalysisStarted: TEventNotifier<TArray<string>>;
    FOnAnalysisComplete: TEventNotifier<TArray<string>>;
    FOnAnalysisFailed: TEventNotifier<TArray<string>>;
    FServerTerminateEvent: TEvent;
    FServerLock: TMutex;

    procedure OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure OnServerTerminated(Sender: TObject);
    procedure SaveIssues(Issues: TObjectList<TLintIssue>);
    procedure EnsureServerInited;
    function GetInitedServer: TLintServer;
    function TryRefreshRules: Boolean;
    procedure RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);
    function GetInAnalysis: Boolean;

    function FilterNonProjectFiles(const InFiles: TArray<string>; const BaseDir: string): TArray<string>;

    procedure AnalyzeFiles(
      const Files: TArray<string>;
      const BaseDir: string;
      const SonarHostUrl: string = '';
      const ProjectKey: string = '';
      const ApiToken: string = '';
      const ProjectPropertiesPath: string = '';
      const DownloadPlugin: Boolean = True);
    procedure AnalyzeFilesWithProjectOptions(const Files: TArray<string>; const ProjectFile: string);
  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>; overload;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeActiveFile;
    procedure AnalyzeOpenFiles;

    procedure RestartServer;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;

    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;

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
  , DelphiLint.Utils
  , System.IOUtils
  , System.StrUtils
  , System.Generics.Defaults
  , DelphiLint.Settings
  , Vcl.Dialogs
  , System.Hash
  , DelphiLint.Logger
  , ToolsAPI
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

procedure TLintContext.AnalyzeActiveFile;
var
  ProjectFile: string;
  SourceEditor: IOTASourceEditor;
begin
  if TryGetCurrentSourceEditor(SourceEditor) and TryGetProjectFile(ProjectFile) then begin
    AnalyzeFilesWithProjectOptions([SourceEditor.FileName, ProjectFile], ProjectFile);
  end
  else begin
    TaskMessageDlg(C_ErrorTitle, 'There are no open analyzable files.', TMsgDlgType.mtError, [mbOK], 0);
    Exit;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.AnalyzeOpenFiles;
var
  ProjectFile: string;
  Files: TArray<string>;
begin
  if TryGetProjectFile(ProjectFile) then begin
    Files := DelphiLint.Utils.GetOpenSourceFiles;
    SetLength(Files, Length(Files) + 1);
    Files[Length(Files) - 1] := ProjectFile;

    if Length(Files) = 1 then begin
      TaskMessageDlg(C_ErrorTitle, 'There are no open files that can be analyzed.', mtError, [mbOK], 0);
      Exit;
    end;

    AnalyzeFilesWithProjectOptions(Files, ProjectFile);
  end
  else begin
    TaskMessageDlg(
      C_ErrorTitle,
      'Could not analyze file - please open a Delphi project first.',
      mtError,
      [mbOK],
      0);
    Exit;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.AnalyzeFilesWithProjectOptions(const Files: TArray<string>; const ProjectFile: string);
var
  ProjectOptions: TLintProjectOptions;
  SonarHostUrl: string;
  ProjectKey: string;
  SonarHostToken: string;
begin
  ProjectOptions := TLintProjectOptions.Create(ProjectFile);
  try
    if ProjectOptions.AnalysisConnectedMode then begin
      SonarHostUrl := ProjectOptions.SonarHostUrl;
      ProjectKey := ProjectOptions.SonarHostProjectKey;
      SonarHostToken := ProjectOptions.SonarHostToken;
    end;

    AnalyzeFiles(
      Files,
      IfThen(
        ProjectOptions.AnalysisBaseDir <> '',
        ProjectOptions.AnalysisBaseDirAbsolute,
        TPath.GetDirectoryName(ProjectFile)),
      SonarHostUrl,
      ProjectKey,
      SonarHostToken,
      ProjectOptions.ProjectPropertiesPath,
      ProjectOptions.SonarHostDownloadPlugin
    );
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.AnalyzeFiles(
  const Files: TArray<string>;
  const BaseDir: string;
  const SonarHostUrl: string = '';
  const ProjectKey: string = '';
  const ApiToken: string = '';
  const ProjectPropertiesPath: string = '';
  const DownloadPlugin: Boolean = True);
var
  Server: TLintServer;
  IncludedFiles: TArray<string>;
begin
  if InAnalysis then begin
    Log.Info('Analysis requested, but we are currently in analysis - ignoring');
    Exit;
  end;

  IncludedFiles := FilterNonProjectFiles(Files, BaseDir);
  FCurrentAnalysis := TCurrentAnalysis.Create(IncludedFiles);
  FOnAnalysisStarted.Notify(IncludedFiles);

  FServerLock.Acquire;
  try
    try
      Server := GetInitedServer;
      Server.Analyze(
        BaseDir,
        IncludedFiles,
        OnAnalyzeResult,
        OnAnalyzeError,
        SonarHostUrl,
        ProjectKey,
        ApiToken,
        ProjectPropertiesPath,
        DownloadPlugin);
    except
      on E: ELintServerError do begin
        TaskMessageDlg(C_ErrorTitle, Format('%s.', [E.Message]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    FServerLock.Release;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.FilterNonProjectFiles(const InFiles: TArray<string>; const BaseDir: string): TArray<string>;
var
  NormalizedBaseDir: string;
  FileName: string;
  OutFiles: TStringList;
begin
  NormalizedBaseDir := NormalizePath(BaseDir);

  OutFiles := TStringList.Create;
  try
    for FileName in InFiles do begin
      if StartsStr(NormalizedBaseDir, NormalizePath(FileName)) then begin
        OutFiles.Add(FileName);
      end
      else begin
        Log.Info('Excluding non-project file %s from analysis', [FileName]);
      end;
    end;

    Result := OutFiles.ToStringArray;
  finally
    FreeAndNil(OutFiles);
  end;
end;


//______________________________________________________________________________________________________________________

constructor TLintContext.Create;
begin
  inherited;
  FActiveIssues := TObjectDictionary<string, TObjectList<TLiveIssue>>.Create;
  FCurrentAnalysis := nil;
  FFileAnalyses := TDictionary<string, TFileAnalysisHistory>.Create;
  FOnAnalysisStarted := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisComplete := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisFailed := TEventNotifier<TArray<string>>.Create;
  FRules := TObjectDictionary<string, TRule>.Create;
  FServerLock := TMutex.Create;
  FServer := nil;

  Log.Clear;
  Log.Info('DelphiLint context initialised');
end;

//______________________________________________________________________________________________________________________

destructor TLintContext.Destroy;
begin
  FServerLock.Acquire;
  try
    if Assigned(FServer) then begin
      FServerTerminateEvent := TEvent.Create;
      try
        FServer.Terminate;
        FServerTerminateEvent.WaitFor(1200);
      finally
        FreeAndNil(FServerTerminateEvent);
      end;
    end;
  finally
    FServerLock.Release;
  end;

  FreeAndNil(FRules);
  FreeAndNil(FActiveIssues);
  FreeAndNil(FFileAnalyses);
  FreeAndNil(FOnAnalysisStarted);
  FreeAndNil(FOnAnalysisComplete);
  FreeAndNil(FOnAnalysisFailed);
  FreeAndNil(FCurrentAnalysis);
  FreeAndNil(FServerLock);

  inherited;
end;

//______________________________________________________________________________________________________________________

function OrderByStartLine(const Left: TLiveIssue; const Right: TLiveIssue): Integer;
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
  SanitizedName := NormalizePath(FileName);
  if FActiveIssues.ContainsKey(SanitizedName) then begin
    if Line = -1 then begin
      Result := FActiveIssues[SanitizedName].ToArray;
      TArray.Sort<TLiveIssue>(Result, TComparer<TLiveIssue>.Construct(OrderByStartLine));
    end
    else begin
      ResultList := TList<TLiveIssue>.Create;
      try
        for Issue in FActiveIssues[SanitizedName] do begin
          if (Line >= Issue.StartLine) and (Line <= Issue.EndLine) then begin
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

procedure TLintContext.EnsureServerInited;
begin
  FServerLock.Acquire;
  try
    if not Assigned(FServer) then begin
      FServer := TLintServer.Create;
      FServer.OnTerminate := OnServerTerminated;
      FServer.FreeOnTerminate := True;
    end;
  finally
    FServerLock.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.OnServerTerminated(Sender: TObject);
begin
  FServerLock.Acquire;
  try
    FServer := nil;
  finally
    FServerLock.Release;
  end;

  if InAnalysis then begin
    OnAnalyzeError('Analysis failed as the server was terminated');
  end;

  if Assigned(FServerTerminateEvent) then begin
    FServerTerminateEvent.SetEvent;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetInitedServer: TLintServer;
begin
  EnsureServerInited;
  Result := FServer;
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetAnalysisStatus(Path: string): TFileAnalysisStatus;
var
  NormalizedPath: string;
  History: TFileAnalysisHistory;
begin
  NormalizedPath := NormalizePath(Path);

  if FFileAnalyses.ContainsKey(NormalizedPath) then begin
    History := FFileAnalyses[NormalizedPath];
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
begin
  TThread.Queue(
    TThread.Current,
    procedure
    var
      Path: string;
      Paths: TArray<string>;
    begin
      for Path in FCurrentAnalysis.Paths do begin
        RecordAnalysis(Path, False, 0);
      end;

      Paths := FCurrentAnalysis.Paths;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisFailed.Notify(Paths);

      TaskMessageDlg(C_ErrorTitle, Message + '.', mtError, [mbOK], 0);
    end);
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
begin
  TThread.Queue(
    TThread.Current,
    procedure
    var
      Paths: TArray<string>;
    begin
      try
        SaveIssues(Issues);
      finally
        FreeAndNil(Issues);
      end;

      Paths := FCurrentAnalysis.Paths;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisComplete.Notify(Paths);
    end);
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

  SanitizedPath := NormalizePath(Path);
  FFileAnalyses.AddOrSetValue(SanitizedPath, History);

  Log.Info(
    'Analysis recorded for %s at %s, (%s, %d issues found)',
    [
      Path,
      FormatDateTime('hh:nn:ss', History.AnalysisTime),
      IfThen(Success, 'successful', 'failure'),
      IssuesFound
    ]);
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.SaveIssues(Issues: TObjectList<TLintIssue>);
var
  Issue: TLintIssue;
  LiveIssue: TLiveIssue;
  SanitizedPath: string;
  NewIssues: TDictionary<string, TObjectList<TLiveIssue>>;
  FileContents: TDictionary<string, TArray<string>>;
  Path: string;
  NewIssuesForFile: TObjectList<TLiveIssue>;
  IssueCount: Integer;
begin
  try
    FileContents := TDictionary<string, TArray<string>>.Create;
    NewIssues := TDictionary<string, TObjectList<TLiveIssue>>.Create;

    // Split issues by file and convert to live issues
    for Issue in Issues do begin
      SanitizedPath := NormalizePath(Issue.FilePath);
      if not NewIssues.ContainsKey(SanitizedPath) then begin
        NewIssues.Add(SanitizedPath, TObjectList<TLiveIssue>.Create);
        // TODO: Improve encoding handling
        FileContents.Add(SanitizedPath, TFile.ReadAllLines(Issue.FilePath, TEncoding.ANSI));
      end;

      LiveIssue := TLiveIssue.Create(Issue, FileContents[SanitizedPath]);
      NewIssues[SanitizedPath].Add(LiveIssue);
    end;

    // Process issues per file
    for Path in FCurrentAnalysis.Paths do begin
      SanitizedPath := NormalizePath(Path);

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
    FreeAndNil(FileContents);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
begin
  Result := FFileAnalyses.TryGetValue(NormalizePath(Path), History);
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);
var
  SanitizedPath: string;
  Issue: TLiveIssue;
  Delta: Integer;
  Index: Integer;
begin
  SanitizedPath := NormalizePath(FilePath);
  Delta := NewLine - OriginalLine;

  if FActiveIssues.ContainsKey(SanitizedPath) then begin
    for Index := 0 to FActiveIssues[SanitizedPath].Count - 1 do begin
      Issue := FActiveIssues[SanitizedPath][Index];

      if Issue.OriginalStartLine = OriginalLine then begin
        if NewLine = -1 then begin
          Issue.Untether;
        end
        else begin
          Issue.LinesMoved := Delta;
        end;
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.TryRefreshRules: Boolean;
var
  Server: TLintServer;
  ProjectFile: string;
  ProjectOptions: TLintProjectOptions;
  RulesRetrieved: TEvent;
  TimedOut: Boolean;
  SonarHostUrl: string;
  ProjectKey: string;
  SonarHostToken: string;
  DownloadPlugin: Boolean;
begin
  Log.Info('Refreshing ruleset');
  Result := False;

  if not TryGetProjectFile(ProjectFile) then begin
    Log.Info('Not in a project, aborting refresh');
    Exit;
  end;

  try
    RulesRetrieved := TEvent.Create;
    ProjectOptions := TLintProjectOptions.Create(ProjectFile);
    TimedOut := False;

    DownloadPlugin := False;
    if ProjectOptions.AnalysisConnectedMode then begin
      SonarHostUrl := ProjectOptions.SonarHostUrl;
      ProjectKey := ProjectOptions.SonarHostProjectKey;
      SonarHostToken := ProjectOptions.SonarHostToken;
      DownloadPlugin := ProjectOptions.SonarHostDownloadPlugin;
    end;

    FServerLock.Acquire;
    try
      try
        Server := GetInitedServer;
      except
        on E: ELintServerError do begin
          TaskMessageDlg(C_ErrorTitle, Format('%s.', [E.Message]), mtError, [mbOK], 0);
          Exit;
        end;
      end;

      Server.RetrieveRules(
        SonarHostUrl,
        ProjectKey,
        procedure(Rules: TObjectDictionary<string, TRule>)
        begin
          if not TimedOut then begin
            // The main thread is blocked waiting for this, so FRules is guaranteed not to be accessed.
            // If FRules is ever accessed by a third thread a mutex will be required.
            FreeAndNil(FRules);
            FRules := Rules;
            RulesRetrieved.SetEvent;
            Log.Info('Retrieved %d rules', [FRules.Count]);
          end
          else begin
            Log.Info('Server retrieved rules after timeout had expired');
          end;
        end,
        procedure(ErrorMsg: string) begin
          if not TimedOut then begin
            RulesRetrieved.SetEvent;
            Log.Info('Error retrieving latest rules: ' + ErrorMsg);
          end
          else begin
            Log.Info('Server rule retrieval returned error after timeout had expired');
          end;
        end,
        SonarHostToken,
        DownloadPlugin);
    finally
      FServerLock.Release;
    end;

    if RulesRetrieved.WaitFor(3000) = TWaitResult.wrSignaled then begin
      Result := True;
    end else begin
      TimedOut := True;
      Result := False;
      Log.Info('Rule retrieval timed out');
    end;
  finally
    FreeAndNil(ProjectOptions);
    FreeAndNil(RulesRetrieved);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.RestartServer;
begin
  FServerLock.Acquire;
  try
    if Assigned(FServer) then begin
      FServerTerminateEvent := TEvent.Create;
      try
        FServer.Terminate;
        FServer := nil;

        if FServerTerminateEvent.WaitFor(3000) <> wrSignaled then begin
          TaskMessageDlg(C_ErrorTitle, 'Server was unresponsive to termination request.', mtError, [mbOK], 0);
        end;
      finally
        FreeAndNil(FServerTerminateEvent);
      end;
    end;
  finally
    FServerLock.Release;
  end;

  if InAnalysis then begin
    OnAnalyzeError('Analysis failed because the server was restarted');
  end;

  try
    EnsureServerInited;
    TaskMessageDlg('DelphiLint', 'Server restarted successfully.', mtInformation, [mbOK], 0);
  except
    on E: ELintServerError do begin
      TaskMessageDlg(C_ErrorTitle, Format('%s.', [E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;
begin
  Result := nil;

  if FRules.ContainsKey(RuleKey) then begin
    Result := FRules[RuleKey];
  end
  else if AllowRefresh then begin
    Log.Info('No rule with rulekey %s found, refreshing ruleset', [RuleKey]);
    if TryRefreshRules then begin
      Result := GetRule(RuleKey, False);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TLiveIssue.Create(Issue: TLintIssue; FileLines: TArray<string>);
begin
  FRuleKey := Issue.RuleKey;
  FMessage := Issue.Message;
  FFilePath := Issue.FilePath;

  if Assigned(Issue.Range) then begin
    FStartLine := Issue.Range.StartLine;
    FEndLine := Issue.Range.EndLine;
    FStartLineOffset := Issue.Range.StartLineOffset;
    FEndLineOffset := Issue.Range.EndLineOffset;
  end
  else begin
    FStartLine := 1;
    FEndLine := 2;
    FStartLineOffset := 0;
    FEndLineOffset := 0;
  end;
  FLinesMoved := 0;
  FLines := TList<string>.Create;
  FTethered := True;

  PopulateLines(FileLines);
end;

//______________________________________________________________________________________________________________________

destructor TLiveIssue.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.PopulateLines(const FileLines: TArray<string>);
var
  LineNum: Integer;
begin
  if FileExists(FFilePath) and (Length(FileLines) > StartLine) then begin
    LineNum := StartLine;

    while (LineNum <= EndLine) and (LineNum < Length(FileLines)) do begin
      FLines.Add(FileLines[LineNum - 1]);
      Inc(LineNum);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.Untether;
begin
  FTethered := False;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.UpdateTether(LineNum: Integer; LineText: string);
var
  Delta: Integer;
begin
  if not Tethered then begin
    Exit;
  end;

  Delta := LineNum - StartLine;
  if (Delta >= 0) and (Delta < FLines.Count) then begin
    if (FLines[Delta] <> LineText) then begin
      Untether;
    end;
  end
  else begin
    Log.Info(
      'Attempted to tether by providing line %d for issue at issue on lines %d-%d',
      [LineNum, StartLine, EndLine]);
  end;
end;

//______________________________________________________________________________________________________________________

function TLiveIssue.GetStartLine: Integer;
begin
  Result := FStartLine + LinesMoved;
end;

//______________________________________________________________________________________________________________________

function TLiveIssue.GetEndLine: Integer;
begin
  Result := FEndLine + LinesMoved;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssue.NewLineMoveSession;
begin
  FStartLine := StartLine;
  FEndLine := EndLine;
  FLinesMoved := 0;
end;

//______________________________________________________________________________________________________________________

constructor TCurrentAnalysis.Create(Paths: TArray<string>);
begin
  FPaths := Paths;
end;

function TCurrentAnalysis.IncludesFile(const Path: string): Boolean;
var
  PathEl: string;
begin
  Result := False;

  for PathEl in FPaths do begin
    if NormalizePath(PathEl) = NormalizePath(Path) then begin
      Result := True;
      Exit;
    end;
  end;
end;

initialization
  GContextInvalid := False;

finalization
  FreeAndNil(GLintContext);
  GContextInvalid := True;

end.
