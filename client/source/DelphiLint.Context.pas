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
    FServerTerminated: Boolean;
    FActiveIssues: TObjectDictionary<string, TObjectList<TLiveIssue>>;
    FFileAnalyses: TDictionary<string, TFileAnalysisHistory>;
    FRules: TObjectDictionary<string, TRule>;
    FOutputLog: TLintLogger;
    FCurrentAnalysis: TCurrentAnalysis;
    FOnAnalysisStarted: TEventNotifier<TArray<string>>;
    FOnAnalysisComplete: TEventNotifier<TArray<string>>;
    FOnAnalysisFailed: TEventNotifier<TArray<string>>;

    function ToUnixPath(Path: string; Lower: Boolean = False): string;
    procedure OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure OnServerTerminated(Sender: TObject);
    procedure SaveIssues(Issues: TObjectList<TLintIssue>);
    procedure DisplayIssues;
    procedure EnsureServerInited;
    function GetInitedServer: TLintServer;
    procedure RefreshRules;
    procedure RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);
    function GetInAnalysis: Boolean;

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
  , DelphiLint.IDEUtils
  , System.IOUtils
  , System.SysUtils
  , System.StrUtils
  , System.Generics.Defaults
  , DelphiLint.Settings
  , Vcl.Dialogs
  , System.Hash
  , ToolsAPI
  , System.SyncObjs
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
  ProjectOptions: TLintProjectOptions;
  ProjectDir: string;
begin
  SourceEditor := DelphiLint.IDEUtils.GetCurrentSourceEditor;
  if not Assigned(SourceEditor) then begin
    Exit;
  end;

  ProjectFile := DelphiLint.IDEUtils.GetProjectFile;
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
  if InAnalysis then begin
    Log.Info('Already in analysis.');
    Exit;
  end;

  FOutputLog.Clear;
  FCurrentAnalysis := TCurrentAnalysis.Create(Files);
  FOnAnalysisStarted.Notify(Files);

  Server := GetInitedServer;
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
    FOutputLog.Info('Analysis failed - server connection could not be established.');
    FOnAnalysisFailed.Notify(Files);
  end;
end;

//______________________________________________________________________________________________________________________

constructor TLintContext.Create;
begin
  inherited;
  FActiveIssues := TObjectDictionary<string, TObjectList<TLiveIssue>>.Create;
  FOutputLog := TLintLogger.Create('Issues', False);
  FCurrentAnalysis := nil;
  FFileAnalyses := TDictionary<string, TFileAnalysisHistory>.Create;
  FOnAnalysisStarted := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisComplete := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisFailed := TEventNotifier<TArray<string>>.Create;
  FRules := TObjectDictionary<string, TRule>.Create;
  FServer := nil;

  Log.Clear;
  Log.Info('Context initialised.');
end;

//______________________________________________________________________________________________________________________

destructor TLintContext.Destroy;
begin
  FreeAndNil(FRules);
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
  Rule: TRule;
  RuleMsg: string;
begin
  FOutputLog.Clear;

  for FileName in FActiveIssues.Keys do begin
    FileIssues := GetIssues(FileName);
    FOutputLog.Title(Format('[DelphiLint] %s (%d issues)', [FileIssues[0].FilePath, Length(FileIssues)]));

    for Issue in FileIssues do begin
      Rule := GetRule(Issue.RuleKey);

      FOutputLog.Info(
        Issue.Message,
        Issue.FilePath,
        Issue.StartLine,
        Issue.StartLineOffset);

      RuleMsg := 'No associated rule found';
      if Assigned(Rule) then begin
        RuleMsg := Format('%s - %s, %s: %s', [Rule.Name, Rule.RuleType, Rule.Severity, Rule.Desc]);
      end;
      FOutputLog.Info(RuleMsg);
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

procedure TLintContext.EnsureServerInited;
begin
  if Assigned(FServer) and FServerTerminated then begin
    FreeAndNil(FServer);
  end;

  if not Assigned(FServer) then begin
    try
      FServer := TLintServer.Create(LintSettings.ServerPort);
      FServer.FreeOnTerminate := False;
      FServer.OnTerminate := OnServerTerminated;
      FServerTerminated := False;
    except
      ShowMessage('Server connection could not be established.');
      FServer := nil;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.OnServerTerminated(Sender: TObject);
begin
  FServerTerminated := True;
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
begin
  TThread.Queue(
    TThread.Current,
    procedure
    var
      Path: string;
      Paths: TArray<string>;
    begin
      FOutputLog.Info('Error during analysis: ' + Message);

      for Path in FCurrentAnalysis.Paths do begin
        RecordAnalysis(Path, False, 0);
      end;

      Paths := FCurrentAnalysis.Paths;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisFailed.Notify(Paths);

      ShowMessage('There was an error during analysis.' + #13#10 + Message);
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

      DisplayIssues;
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

procedure TLintContext.RefreshRules;
var
  Server: TLintServer;
  ProjectFile: string;
  ProjectOptions: TLintProjectOptions;
  RulesRetrieved: TEvent;
  TimedOut: Boolean;
begin
  Log.Info('Refreshing ruleset');

  Server := GetInitedServer;

  ProjectFile := DelphiLint.IDEUtils.GetProjectFile;
  ProjectOptions := TLintProjectOptions.Create(ProjectFile);

  RulesRetrieved := TEvent.Create;
  TimedOut := False;
  try
    Server.RetrieveRules(
      ProjectOptions.SonarHostUrl,
      ProjectOptions.ProjectKey,
      procedure(Rules: TObjectDictionary<string, TRule>)
      begin
        if not TimedOut then begin
          // The main thread is blocked waiting for this, so FRules is guaranteed not to be accessed.
          // If FRules is ever accessed by a third thread a mutex will be required.
          FreeAndNil(FRules);
          FRules := Rules;
          Log.Info('Retrieved ' + IntToStr(FRules.Count) + ' rules');

          RulesRetrieved.SetEvent;
        end
        else begin
          Log.Info('Server retrieved rules after timeout had expired');
        end;
      end,
      procedure(ErrorMsg: string) begin
        if not TimedOut then begin
          Log.Info('Error retrieving latest rules: ' + ErrorMsg);
          RulesRetrieved.SetEvent;
        end
        else begin
          Log.Info('Server rule retrieval returned error after timeout had expired');
        end;
      end);

    if RulesRetrieved.WaitFor(3000) <> TWaitResult.wrSignaled then begin
      TimedOut := True;
      Log.Info('Rule retrieval timed out');
    end;
  finally
    FreeAndNil(RulesRetrieved);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintContext.GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;
begin
  if FRules.ContainsKey(RuleKey) then begin
    Result := FRules[RuleKey];
  end
  else if AllowRefresh then begin
    Log.Info('No rule with rulekey ' + RuleKey + ' found, refreshing.');
    RefreshRules;
    Result := GetRule(RuleKey, False);
  end
  else begin
    Result := nil;
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
