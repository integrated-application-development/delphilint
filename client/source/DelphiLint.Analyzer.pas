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
unit DelphiLint.Analyzer;

interface

uses
    DelphiLint.Server
  , DelphiLint.Data
  , System.Generics.Collections
  , DelphiLint.Events
  , System.SyncObjs
  , DelphiLint.Context
  , DelphiLint.LiveData
  ;

type
  TAnalyzerImpl = class(TInterfacedObject, IAnalyzer)
  private const
    CServerAcquireErrorMsg = 'There was a problem reaching the DelphiLint server.';
  private type
    TServerCallback = reference to procedure(Server: ILintServer);
  private
    FServerThread: TLintServerThread;
    FActiveIssues: TObjectDictionary<string, TList<ILiveIssue>>;
    FFileAnalyses: TDictionary<string, TFileAnalysisHistory>;
    FRules: TObjectDictionary<string, TRule>;
    FCurrentAnalysis: TCurrentAnalysis;
    FOnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext>;
    FServerTerminateEvent: TEvent;

    procedure OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure SaveIssues(Issues: TObjectList<TLintIssue>; IssuesHaveMetadata: Boolean = False);
    function TryRefreshRules: Boolean;
    procedure RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);

    procedure TriggerServerTerminateEvent(Sender: TObject);

    function FilterNonProjectFiles(const InFiles: TArray<string>; const BaseDir: string): TArray<string>;

    procedure DoAnalyzeFiles(Options: TAnalyzeOptions; const DownloadPlugin: Boolean = True);

    procedure ExecuteWithServer(Callback: TServerCallback; OnError: TErrorAction);

  protected
    function GetOnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext>;
    function GetCurrentAnalysis: TCurrentAnalysis;
    function GetInAnalysis: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1; Column: Integer = -1): TArray<ILiveIssue>;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeFiles(const Files: TArray<string>; const ProjectFile: string);
    procedure ClearFile(const FileName: string);

    procedure RestartServer;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;

    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;
  end;

implementation

uses
    System.SysUtils
  , System.Classes
  , System.IOUtils
  , System.StrUtils
  , System.Generics.Defaults
  , System.Hash
  , Vcl.Dialogs
  , DelphiLint.ProjectOptions
  , DelphiLint.Utils
  ;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.ClearFile(const FileName: string);
var
  NormalizedName: string;
  Issue: ILiveIssue;
  StateChange: TAnalysisStateChangeContext;
begin
  NormalizedName := NormalizePath(FileName);

  if FFileAnalyses.ContainsKey(NormalizedName) then begin
    FFileAnalyses.Remove(NormalizedName);
  end;

  if FActiveIssues.ContainsKey(NormalizedName) then begin
    for Issue in FActiveIssues[NormalizedName] do begin
      Issue.Untether;
    end;
    FActiveIssues.Remove(NormalizedName);
  end;

  StateChange.Change := ascCleared;
  StateChange.Files := [FileName];
  FOnAnalysisStateChanged.Notify(StateChange);
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.AnalyzeFiles(const Files: TArray<string>; const ProjectFile: string);
var
  ProjectOptions: TLintProjectOptions;
  AnalyzeOptions: TAnalyzeOptions;
begin
  ProjectOptions := LintContext.GetProjectOptions(ProjectFile);
  try
    AnalyzeOptions := TAnalyzeOptions.Create(
      IfThen(
        ProjectOptions.AnalysisBaseDir <> '',
        ProjectOptions.AnalysisBaseDirAbsolute,
        TPath.GetDirectoryName(ProjectFile)),
      Files,
      LintContext.Settings.SonarDelphiVersion
    );
    AnalyzeOptions.ProjectPropertiesPath := ProjectOptions.ProjectPropertiesPath;

    if ProjectOptions.AnalysisConnectedMode then begin
      AnalyzeOptions.Sonar := TSonarProjectOptions.Create(
        ProjectOptions.SonarHostUrl,
        LintContext.Settings.GetSonarHostToken(
          ProjectOptions.SonarHostUrl,
          ProjectOptions.SonarHostProjectKey
        ),
        ProjectOptions.SonarHostProjectKey
      );
    end;

    DoAnalyzeFiles(AnalyzeOptions, ProjectOptions.SonarHostDownloadPlugin);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.DoAnalyzeFiles(Options: TAnalyzeOptions; const DownloadPlugin: Boolean = True);
var
  StateChange: TAnalysisStateChangeContext;
begin
  if GetInAnalysis then begin
    Log.Info('An analysis has been requested, but it will be ignored as there is another in progress');
    Exit;
  end;

  Options.InputFiles := FilterNonProjectFiles(Options.InputFiles, Options.BaseDir);
  FCurrentAnalysis := TCurrentAnalysis.Create(Options.InputFiles);

  StateChange.Files := Options.InputFiles;
  StateChange.Change := ascStarted;
  FOnAnalysisStateChanged.Notify(StateChange);

  ExecuteWithServer(
    procedure(Server: ILintServer) begin
      Server.Analyze(Options, OnAnalyzeResult, OnAnalyzeError, DownloadPlugin);
    end,
    procedure(Msg: string) begin
      OnAnalyzeError('');
      TaskMessageDlg(
        CServerAcquireErrorMsg,
        Format('%s.', [Msg]),
        mtError,
        [mbOK],
        0
      );
    end
  );
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.FilterNonProjectFiles(const InFiles: TArray<string>; const BaseDir: string): TArray<string>;
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

constructor TAnalyzerImpl.Create;
begin
  inherited;
  FActiveIssues := TObjectDictionary<string, TList<ILiveIssue>>.Create;
  FCurrentAnalysis := nil;
  FFileAnalyses := TDictionary<string, TFileAnalysisHistory>.Create;
  FOnAnalysisStateChanged := TEventNotifier<TAnalysisStateChangeContext>.Create;
  FRules := TObjectDictionary<string, TRule>.Create;
  FServerThread := TLintServerThread.Create;
  FServerThread.FreeOnTerminate := False;
end;

//______________________________________________________________________________________________________________________

destructor TAnalyzerImpl.Destroy;
var
  ServerWaitResult: TWaitResult;
begin
  FServerTerminateEvent := TEvent.Create;
  try
    FServerThread.OnTerminate := TriggerServerTerminateEvent;
    FServerThread.Terminate;
    Log.Debug('Server told to terminate, waiting...');
    ServerWaitResult := FServerTerminateEvent.WaitFor(1500);
  finally
    FreeAndNil(FServerTerminateEvent);
  end;

  if ServerWaitResult = wrSignaled then begin
    Log.Debug('Received OnTerminate signal from server thread, freeing');
    FreeAndNil(FServerThread);
  end
  else begin
    FServerThread.FreeOnTerminate := True;
    Log.Warn('Wait for server thread to terminate timed out. This may leak memory');
  end;

  FreeAndNil(FRules);
  FreeAndNil(FActiveIssues);
  FreeAndNil(FFileAnalyses);
  FreeAndNil(FOnAnalysisStateChanged);
  FreeAndNil(FCurrentAnalysis);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.ExecuteWithServer(Callback: TServerCallback; OnError: TErrorAction);
var
  Server: ILintServer;
begin
  try
    Server := FServerThread.AcquireServer;
    try
      Callback(Server);
    finally
      FServerThread.ReleaseServer;
    end;
  except
    on E: ELintServerError do begin
      OnError(E.Message);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function OrderIssuesByRange(const Left: ILiveIssue; const Right: ILiveIssue): Integer;
begin
  Result := TComparer<Integer>.Default.Compare(Left.OriginalStartLine, Right.OriginalStartLine);
  if Result = 0 then begin
    Result := TComparer<Integer>.Default.Compare(Left.StartLineOffset, Right.StartLineOffset);
  end;
  if Result = 0 then begin
    Result := TComparer<string>.Default.Compare(Left.RuleKey, Right.RuleKey);
  end;
  if Result = 0 then begin
    Result := TComparer<Integer>.Default.Compare(Left.EndLineOffset, Right.EndLineOffset);
  end;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetInAnalysis: Boolean;
begin
  Result := Assigned(FCurrentAnalysis);
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetOnAnalysisStateChanged: TEventNotifier<TAnalysisStateChangeContext>;
begin
  Result := FOnAnalysisStateChanged;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetCurrentAnalysis: TCurrentAnalysis;
begin
  Result := FCurrentAnalysis;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetIssues(FileName: string; Line: Integer = -1; Column: Integer = -1): TArray<ILiveIssue>;

  function Matches(Issue: ILiveIssue): Boolean;
  var
    AfterStart: Boolean;
    BeforeEnd: Boolean;
  begin
    Result := (Issue.StartLine <= Line) and (Issue.EndLine >= Line);

    if Result and (Column <> -1) then begin
      AfterStart := (Issue.StartLine < Line) or (Issue.StartLineOffset <= Column);
      BeforeEnd := (Issue.EndLine > Line) or (Issue.EndLineOffset >= Column);
      Result := AfterStart and BeforeEnd;
    end;
  end;

var
  SanitizedName: string;
  Issue: ILiveIssue;
  ResultList: TList<ILiveIssue>;
begin
  SanitizedName := NormalizePath(FileName);
  if FActiveIssues.ContainsKey(SanitizedName) then begin
    if Line = -1 then begin
      Result := FActiveIssues[SanitizedName].ToArray;
      TArray.Sort<ILiveIssue>(Result, TComparer<ILiveIssue>.Construct(OrderIssuesByRange));
    end
    else begin
      ResultList := TList<ILiveIssue>.Create;
      try
        for Issue in FActiveIssues[SanitizedName] do begin
          if Matches(Issue) then begin
            ResultList.Add(Issue);
          end;
        end;

        ResultList.Sort(TComparer<ILiveIssue>.Construct(OrderIssuesByRange));
        Result := ResultList.ToArray;
      finally
        FreeAndNil(ResultList);
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetAnalysisStatus(Path: string): TFileAnalysisStatus;
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

procedure TAnalyzerImpl.OnAnalyzeError(Message: string);
begin
  TThread.Queue(
    TThread.Current,
    procedure
    var
      Path: string;
      StateChange: TAnalysisStateChangeContext;
    begin
      for Path in FCurrentAnalysis.Paths do begin
        RecordAnalysis(Path, False, 0);
      end;

      StateChange.Files := FCurrentAnalysis.Paths;
      StateChange.Change := ascFailed;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisStateChanged.Notify(StateChange);

      if Message <> '' then begin
        TaskMessageDlg('DelphiLint encountered a problem during analysis.', Message + '.', mtWarning, [mbOK], 0);
      end;
    end);
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
var
  HasMetadata: Boolean;
  ProjectFile: string;
  ProjectOptions: TLintProjectOptions;
begin
  HasMetadata := False;
  if TryGetProjectFile(ProjectFile) then begin
    try
      ProjectOptions := LintContext.GetProjectOptions(ProjectFile);
      HasMetadata := ProjectOptions.AnalysisConnectedMode;
    finally
      FreeAndNil(ProjectOptions);
    end;
  end;

  TThread.Queue(
    TThread.Current,
    procedure
    var
      StateChange: TAnalysisStateChangeContext;
    begin
      try
        SaveIssues(Issues, HasMetadata);
      finally
        FreeAndNil(Issues);
      end;

      StateChange.Files := FCurrentAnalysis.Paths;
      StateChange.Change := ascSucceeded;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisStateChanged.Notify(StateChange);
    end);
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);
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

  Log.Debug(
    'Analysis recorded for %s at %s, (%s, %d issues found)',
    [
      Path,
      FormatDateTime('hh:nn:ss', History.AnalysisTime),
      IfThen(Success, 'successful', 'failure'),
      IssuesFound
    ]);
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.SaveIssues(Issues: TObjectList<TLintIssue>; IssuesHaveMetadata: Boolean = False);

  function GetDelphiFileLines(FilePath: string): TArray<string>;
  var
    FileBytes: TBytes;
    Encoding: TEncoding;
    Preamble: Integer;
    FileStr: string;
    StringList: TStringList;
  begin
    FileBytes := TFile.ReadAllBytes(FilePath);
    Encoding := nil;
    Preamble := TEncoding.GetBufferEncoding(FileBytes, Encoding, TEncoding.Default);
    FileStr := Encoding.GetString(FileBytes, Preamble, Length(FileBytes) - Preamble);

    Log.Info('Detected encoding %s for file %s', [Encoding.EncodingName, FilePath]);

    StringList := TStringList.Create;
    try
      StringList.Text := FileStr;
      Result := StringList.ToStringArray;
    finally
      FreeAndNil(StringList);
    end;
  end;

var
  Issue: TLintIssue;
  LiveIssue: ILiveIssue;
  SanitizedPath: string;
  NewIssues: TDictionary<string, TList<ILiveIssue>>;
  FileContents: TDictionary<string, TArray<string>>;
  RelevantLines: TArray<string>;
  Path: string;
  NewIssuesForFile: TList<ILiveIssue>;
  IssueCount: Integer;
begin
  try
    FileContents := TDictionary<string, TArray<string>>.Create;
    NewIssues := TDictionary<string, TList<ILiveIssue>>.Create;

    // Split issues by file and convert to live issues
    for Issue in Issues do begin
      SanitizedPath := NormalizePath(Issue.FilePath);
      if not NewIssues.ContainsKey(SanitizedPath) then begin
        NewIssues.Add(SanitizedPath, TList<ILiveIssue>.Create);
        FileContents.Add(SanitizedPath, GetDelphiFileLines(Issue.FilePath));
      end;

      if Assigned(Issue.Range) then begin
        RelevantLines := Copy(
          FileContents[SanitizedPath],
          Issue.Range.StartLine - 1,
          Issue.Range.EndLine - Issue.Range.StartLine + 1);
      end
      else begin
        RelevantLines := Copy(FileContents[SanitizedPath], 0, 1);
      end;

      LiveIssue := TLiveIssueImpl.Create(Issue, RelevantLines, IssuesHaveMetadata);
      LiveIssue.OnUntethered.AddListener(
        procedure(const Line: Integer)
        var
          StateChange: TAnalysisStateChangeContext;
        begin
          StateChange.Files := [SanitizedPath];
          StateChange.Change := ascUpdated;
          FOnAnalysisStateChanged.Notify(StateChange);
        end);
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

procedure TAnalyzerImpl.TriggerServerTerminateEvent(Sender: TObject);
begin
  if Assigned(FServerTerminateEvent) then begin
    FServerTerminateEvent.SetEvent;
  end;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
begin
  Result := FFileAnalyses.TryGetValue(NormalizePath(Path), History);
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);
var
  SanitizedPath: string;
  Issue: ILiveIssue;
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

function TAnalyzerImpl.TryRefreshRules: Boolean;
var
  ProjectFile: string;
  ProjectOptions: TLintProjectOptions;
  RulesRetrieved: TEvent;
  TimedOut: Boolean;
  DownloadPlugin: Boolean;
  SonarOptions: TSonarProjectOptions;
begin
  Log.Debug('Refreshing ruleset');
  Result := False;

  if not TryGetProjectFile(ProjectFile) then begin
    Log.Warn('Not in a project, aborting refresh');
    Exit;
  end;

  try
    RulesRetrieved := TEvent.Create;
    ProjectOptions := LintContext.GetProjectOptions(ProjectFile);
    TimedOut := False;

    DownloadPlugin := False;
    if ProjectOptions.AnalysisConnectedMode then begin
      SonarOptions := TSonarProjectOptions.Create(
        ProjectOptions.SonarHostUrl,
        LintContext.Settings.GetSonarHostToken(
          ProjectOptions.SonarHostUrl,
          ProjectOptions.SonarHostProjectKey
        ),
        ProjectOptions.SonarHostProjectKey
      );
      DownloadPlugin := ProjectOptions.SonarHostDownloadPlugin;
    end;

    ExecuteWithServer(
      procedure(Server: ILintServer) begin
        Server.RetrieveRules(
          SonarOptions,
          LintContext.Settings.SonarDelphiVersion,
          procedure(Rules: TObjectDictionary<string, TRule>)
          begin
            if not TimedOut then begin
              // The main thread is blocked waiting for this, so FRules is guaranteed not to be accessed.
              // If FRules is ever accessed by a third thread a mutex will be required.
              FreeAndNil(FRules);
              FRules := Rules;
              RulesRetrieved.SetEvent;
            end
            else begin
              Log.Warn('Server retrieved rules after timeout had expired');
            end;
          end,
          procedure(ErrorMsg: string) begin
            if not TimedOut then begin
              RulesRetrieved.SetEvent;
              Log.Warn('Error retrieving latest rules: ' + ErrorMsg);
            end
            else begin
              Log.Warn('Server rule retrieval returned error after timeout had expired');
            end;
          end,
          DownloadPlugin
        );
      end,
      procedure(Msg: string) begin
        TaskMessageDlg(
          CServerAcquireErrorMsg,
          Format('%s.', [Msg]),
          mtError,
          [mbOK],
          0
        );
      end
    );

    if RulesRetrieved.WaitFor(3000) = TWaitResult.wrSignaled then begin
      Result := True;
    end else begin
      TimedOut := True;
      Result := False;
      Log.Warn('Rule retrieval timed out');
    end;
  finally
    FreeAndNil(ProjectOptions);
    FreeAndNil(RulesRetrieved);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.RestartServer;
var
  RestartSuccessful: Boolean;
begin
  RestartSuccessful := False;

  ExecuteWithServer(
    procedure(Server: ILintServer) begin
      Server := nil;
      FServerThread.RefreshServer;
      RestartSuccessful := True;
    end,
    procedure(Msg: string) begin
      TaskMessageDlg(
        CServerAcquireErrorMsg,
        Format('%s.', [Msg]),
        mtError,
        [mbOK],
        0
      );
    end
  );

  if GetInAnalysis then begin
    OnAnalyzeError('Analysis failed because the server was stopped');
  end;

  if RestartSuccessful then begin
    MessageDlg('The DelphiLint server has been restarted.', mtInformation, [mbOK], 0);
  end;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;
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

end.
