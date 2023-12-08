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
unit DelphiLint.Analyzer;

interface

uses
    DelphiLint.Server
  , DelphiLint.Data
  , System.Generics.Collections
  , DelphiLint.Events
  , System.SyncObjs
  , DelphiLint.Context
  ;

type
  TAnalyzerImpl = class(TInterfacedObject, IAnalyzer)
  private
    FServerThread: TLintServerThread;
    FActiveIssues: TObjectDictionary<string, TObjectList<TLiveIssue>>;
    FFileAnalyses: TDictionary<string, TFileAnalysisHistory>;
    FRules: TObjectDictionary<string, TRule>;
    FCurrentAnalysis: TCurrentAnalysis;
    FOnAnalysisStarted: TEventNotifier<TArray<string>>;
    FOnAnalysisComplete: TEventNotifier<TArray<string>>;
    FOnAnalysisFailed: TEventNotifier<TArray<string>>;
    FServerTerminateEvent: TEvent;

    procedure OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure SaveIssues(Issues: TObjectList<TLintIssue>; IssuesHaveMetadata: Boolean = False);
    function TryRefreshRules: Boolean;
    procedure RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);

    procedure TriggerServerTerminateEvent(Sender: TObject);

    function FilterNonProjectFiles(const InFiles: TArray<string>; const BaseDir: string): TArray<string>;

    procedure AnalyzeFiles(Options: TAnalyzeOptions; const DownloadPlugin: Boolean = True);
    procedure AnalyzeFilesWithProjectOptions(const Files: TArray<string>; const ProjectFile: string);

  protected
    function GetOnAnalysisStarted: TEventNotifier<TArray<string>>;
    function GetOnAnalysisComplete: TEventNotifier<TArray<string>>;
    function GetOnAnalysisFailed: TEventNotifier<TArray<string>>;
    function GetCurrentAnalysis: TCurrentAnalysis;
    function GetInAnalysis: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeActiveFile;
    procedure AnalyzeOpenFiles;

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

procedure TAnalyzerImpl.AnalyzeActiveFile;
var
  ProjectFile: string;
  SourceEditor: IIDESourceEditor;
begin
  if not TryGetProjectFile(ProjectFile) then begin
    TaskMessageDlg(
      'DelphiLint cannot analyze the active file.',
      'There is no open Delphi project.',
      mtWarning,
      [mbOK],
      0);
  end
  else if not TryGetCurrentSourceEditor(SourceEditor) then begin
    TaskMessageDlg(
      'DelphiLint cannot analyze the active file.',
      'There are no open files that can be analyzed.',
      mtWarning,
      [mbOK],
      0);
  end
  else begin
    if LintContext.Settings.ClientSaveBeforeAnalysis then begin
      Log.Debug('Saving file before analysis');
      SourceEditor.Module.Save(True);
    end;
    AnalyzeFilesWithProjectOptions([SourceEditor.FileName, ProjectFile], ProjectFile);
    Exit;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.AnalyzeOpenFiles;
var
  ProjectFile: string;
  Modules: TArray<IIDEModule>;
  Files: TArray<string>;
  Module: IIDEModule;
begin
  if TryGetProjectFile(ProjectFile) then begin
    Modules := DelphiLint.Utils.GetOpenSourceModules;

    if LintContext.Settings.ClientSaveBeforeAnalysis then begin
      for Module in Modules do begin
        try
          Module.Save(True);
        except
          on E: Exception do begin
            Log.Warn('Module %s could not be saved', [Module.FileName]);
          end;
        end;
      end;
    end;

    Files := TArrayUtils.Map<IIDEModule, string>(
      Modules,
      function(Module: IIDEModule): string
      begin
        Result := Module.FileName;
      end);
    SetLength(Files, Length(Files) + 1);
    Files[Length(Files) - 1] := ProjectFile;

    if Length(Files) = 1 then begin
      TaskMessageDlg(
        'DelphiLint cannot analyze all open files.',
        'There are no open files that can be analyzed.',
        mtWarning,
        [mbOK],
        0);
      Exit;
    end;

    AnalyzeFilesWithProjectOptions(Files, ProjectFile);
  end
  else begin
    TaskMessageDlg(
      'DelphiLint cannot analyze all open files.',
      'There is no open Delphi project.',
      mtWarning,
      [mbOK],
      0);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.AnalyzeFilesWithProjectOptions(const Files: TArray<string>; const ProjectFile: string);
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
      Files
    );
    AnalyzeOptions.ProjectPropertiesPath := ProjectOptions.ProjectPropertiesPath;

    if ProjectOptions.AnalysisConnectedMode then begin
      AnalyzeOptions.Sonar := TSonarProjectOptions.Create(
        ProjectOptions.SonarHostUrl,
        ProjectOptions.SonarHostToken,
        ProjectOptions.SonarHostProjectKey
      );
    end;

    AnalyzeFiles(AnalyzeOptions, ProjectOptions.SonarHostDownloadPlugin);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TAnalyzerImpl.AnalyzeFiles(Options: TAnalyzeOptions; const DownloadPlugin: Boolean = True);
var
  Server: TLintServer;
begin
  if GetInAnalysis then begin
    Log.Info('An analysis has been requested, but it will be ignored as there is another in progress');
    Exit;
  end;

  Options.InputFiles := FilterNonProjectFiles(Options.InputFiles, Options.BaseDir);
  FCurrentAnalysis := TCurrentAnalysis.Create(Options.InputFiles);
  FOnAnalysisStarted.Notify(Options.InputFiles);

  Server := FServerThread.AcquireServer;
  try
    try
      Server.Analyze(Options, OnAnalyzeResult, OnAnalyzeError, DownloadPlugin);
    except
      on E: ELintServerError do begin
        TaskMessageDlg('The DelphiLint server encountered an error.', Format('%s.', [E.Message]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    FServerThread.ReleaseServer;
  end;
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
  FActiveIssues := TObjectDictionary<string, TObjectList<TLiveIssue>>.Create;
  FCurrentAnalysis := nil;
  FFileAnalyses := TDictionary<string, TFileAnalysisHistory>.Create;
  FOnAnalysisStarted := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisComplete := TEventNotifier<TArray<string>>.Create;
  FOnAnalysisFailed := TEventNotifier<TArray<string>>.Create;
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
  FreeAndNil(FOnAnalysisStarted);
  FreeAndNil(FOnAnalysisComplete);
  FreeAndNil(FOnAnalysisFailed);
  FreeAndNil(FCurrentAnalysis);
  inherited;
end;

//______________________________________________________________________________________________________________________

function OrderIssuesByRange(const Left: TLiveIssue; const Right: TLiveIssue): Integer;
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

function TAnalyzerImpl.GetOnAnalysisComplete: TEventNotifier<TArray<string>>;
begin
  Result := FOnAnalysisComplete;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetOnAnalysisFailed: TEventNotifier<TArray<string>>;
begin
  Result := FOnAnalysisFailed;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetOnAnalysisStarted: TEventNotifier<TArray<string>>;
begin
  Result := FOnAnalysisStarted;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetCurrentAnalysis: TCurrentAnalysis;
begin
  Result := FCurrentAnalysis;
end;

//______________________________________________________________________________________________________________________

function TAnalyzerImpl.GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>;
var
  SanitizedName: string;
  Issue: TLiveIssue;
  ResultList: TList<TLiveIssue>;
begin
  SanitizedName := NormalizePath(FileName);
  if FActiveIssues.ContainsKey(SanitizedName) then begin
    if Line = -1 then begin
      Result := FActiveIssues[SanitizedName].ToArray;
      TArray.Sort<TLiveIssue>(Result, TComparer<TLiveIssue>.Construct(OrderIssuesByRange));
    end
    else begin
      ResultList := TList<TLiveIssue>.Create;
      try
        for Issue in FActiveIssues[SanitizedName] do begin
          if (Line >= Issue.StartLine) and (Line <= Issue.EndLine) then begin
            ResultList.Add(Issue);
          end;
        end;

        ResultList.Sort(TComparer<TLiveIssue>.Construct(OrderIssuesByRange));
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
      Paths: TArray<string>;
    begin
      for Path in FCurrentAnalysis.Paths do begin
        RecordAnalysis(Path, False, 0);
      end;

      Paths := FCurrentAnalysis.Paths;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisFailed.Notify(Paths);

      TaskMessageDlg('DelphiLint encountered a problem during analysis.', Message + '.', mtWarning, [mbOK], 0);
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
      Paths: TArray<string>;
    begin
      try
        SaveIssues(Issues, HasMetadata);
      finally
        FreeAndNil(Issues);
      end;

      Paths := FCurrentAnalysis.Paths;
      FreeAndNil(FCurrentAnalysis);
      FOnAnalysisComplete.Notify(Paths);
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
var
  Issue: TLintIssue;
  LiveIssue: TLiveIssue;
  SanitizedPath: string;
  NewIssues: TDictionary<string, TObjectList<TLiveIssue>>;
  FileContents: TDictionary<string, TArray<string>>;
  RelevantLines: TArray<string>;
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

      if Assigned(Issue.Range) then begin
        RelevantLines := Copy(
          FileContents[SanitizedPath],
          Issue.Range.StartLine - 1,
          Issue.Range.EndLine - Issue.Range.StartLine + 1);
      end
      else begin
        SetLength(RelevantLines, 0);
      end;

      LiveIssue := TLiveIssue.Create(Issue, RelevantLines, IssuesHaveMetadata);
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

function TAnalyzerImpl.TryRefreshRules: Boolean;
var
  Server: TLintServer;
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
        ProjectOptions.SonarHostToken,
        ProjectOptions.SonarHostProjectKey
      );
      DownloadPlugin := ProjectOptions.SonarHostDownloadPlugin;
    end;

    Server := FServerThread.AcquireServer;
    try
      Server.RetrieveRules(
        SonarOptions,
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
        DownloadPlugin);
    finally
      FServerThread.ReleaseServer;
    end;

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
begin
  try
    FServerThread.AcquireServer;
    try
      FServerThread.RefreshServer;
    finally
      FServerThread.ReleaseServer;
    end;
  except
    on E: ELintServerError do begin
      TaskMessageDlg(
        'The DelphiLint server encountered a problem while restarting.',
        Format('%s.', [E.Message]),
        mtError,
        [mbOK],
        0);
    end;
  end;

  if GetInAnalysis then begin
    OnAnalyzeError('Analysis failed because the server was restarted');
  end;

  MessageDlg('The DelphiLint server has been restarted.', mtInformation, [mbOK], 0);
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
