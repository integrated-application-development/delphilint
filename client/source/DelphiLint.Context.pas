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
  private
    FServer: TLintServer;
    FServerTerminated: Boolean;
    FActiveIssues: TObjectDictionary<string, TObjectList<TLiveIssue>>;
    FFileAnalyses: TDictionary<string, TFileAnalysisHistory>;
    FRules: TObjectDictionary<string, TRule>;
    FCurrentAnalysis: TCurrentAnalysis;
    FOnAnalysisStarted: TEventNotifier<TArray<string>>;
    FOnAnalysisComplete: TEventNotifier<TArray<string>>;
    FOnAnalysisFailed: TEventNotifier<TArray<string>>;

    procedure OnAnalyzeResult(Issues: TObjectList<TLintIssue>);
    procedure OnAnalyzeError(Message: string);
    procedure OnServerTerminated(Sender: TObject);
    procedure SaveIssues(Issues: TObjectList<TLintIssue>);
    procedure EnsureServerInited;
    function GetInitedServer: TLintServer;
    procedure RefreshRules;
    procedure RecordAnalysis(Path: string; Success: Boolean; IssuesFound: Integer);
    function GetInAnalysis: Boolean;

    function FilterNonProjectFiles(const InFiles: TArray<string>; const BaseDir: string): TArray<string>;

    procedure AnalyzeFiles(
      const Files: TArray<string>;
      const BaseDir: string;
      const SonarHostUrl: string = '';
      const ProjectKey: string = '';
      const ApiToken: string = '');
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
  , System.SysUtils
  , System.StrUtils
  , System.Generics.Defaults
  , DelphiLint.Settings
  , Vcl.Dialogs
  , System.Hash
  , System.SyncObjs
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
    MessageDlg('There are no open analyzable files.', TMsgDlgType.mtError, [mbOK], 0);
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
      MessageDlg('There are no open analyzable files.', TMsgDlgType.mtError, [mbOK], 0);
      Exit;
    end;

    AnalyzeFilesWithProjectOptions(Files, ProjectFile);
  end
  else begin
    MessageDlg('Could not analyze file - please open a Delphi project first.', TMsgDlgType.mtError, [mbOK], 0);
    Exit;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.AnalyzeFilesWithProjectOptions(const Files: TArray<string>; const ProjectFile: string);
var
  ProjectOptions: TLintProjectOptions;
begin
  ProjectOptions := TLintProjectOptions.Create(ProjectFile);
  try
    AnalyzeFiles(
      Files,
      IfThen(ProjectOptions.ProjectBaseDir <> '', ProjectOptions.ProjectBaseDir, TPath.GetDirectoryName(ProjectFile)),
      ProjectOptions.SonarHostUrl,
      ProjectOptions.ProjectKey,
      ProjectOptions.SonarHostToken);
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
  const ApiToken: string = '');
var
  Server: TLintServer;
  IncludedFiles: TArray<string>;
begin
  if InAnalysis then begin
    Log.Info('Already in analysis.');
    Exit;
  end;

  IncludedFiles := FilterNonProjectFiles(Files, BaseDir);
  FCurrentAnalysis := TCurrentAnalysis.Create(IncludedFiles);
  FOnAnalysisStarted.Notify(IncludedFiles);

  Server := GetInitedServer;
  if Assigned(Server) then begin
    Server.Analyze(
      BaseDir,
      IncludedFiles,
      OnAnalyzeResult,
      OnAnalyzeError,
      SonarHostUrl,
      ProjectKey,
      ApiToken);
  end
  else begin
    FOnAnalysisFailed.Notify(IncludedFiles);
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
        Log.Info(
          'Excluding non-project file ' + FileName +
          ' from analysis. Please set a custom base directory if this file should have been included.');
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
  FreeAndNil(FFileAnalyses);
  FreeAndNil(FOnAnalysisStarted);
  FreeAndNil(FOnAnalysisComplete);
  FreeAndNil(FOnAnalysisFailed);
  FreeAndNil(FCurrentAnalysis);

  inherited;
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

      ShowMessage(Message);
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

procedure TLintContext.UpdateIssueLine(FilePath: string; OriginalLine, NewLine: Integer);
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

  if not TryGetProjectFile(ProjectFile) then begin
    Log.Info('Not in a project, aborting refresh');
    Exit;
  end;

  Server := GetInitedServer;

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
      end,
      ProjectOptions.SonarHostToken);

    if RulesRetrieved.WaitFor(3000) <> TWaitResult.wrSignaled then begin
      TimedOut := True;
      Log.Info('Rule retrieval timed out');
    end;
  finally
    FreeAndNil(RulesRetrieved);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintContext.RestartServer;
begin
  FreeAndNil(FServer);
  Sleep(500);
  try
    GetInitedServer;
  except
    on E: Exception do begin
      MessageDlg(
        'DelphiLint server restart encountered error: ' + E.Message,
        TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK],
        0);
      FreeAndNil(FServer);
    end;
  end;

  MessageDlg('DelphiLint server restarted successfully.', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
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

constructor TLiveIssue.Create(Issue: TLintIssue; FileLines: TArray<string>);
begin
  FRuleKey := Issue.RuleKey;
  FMessage := Issue.Message;
  FFilePath := Issue.FilePath;
  FStartLine := Issue.Range.StartLine;
  FEndLine := Issue.Range.EndLine;
  FStartLineOffset := Issue.Range.StartLineOffset;
  FEndLineOffset := Issue.Range.EndLineOffset;
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

procedure TLiveIssue.UpdateTether(LineNum: Integer; LineText: string);
var
  Delta: Integer;
begin
  if not FTethered then begin
    Exit;
  end;

  Delta := StartLine - LineNum;
  if (Delta >= 0) and (Delta < FLines.Count) then begin
    if (FLines[Delta] <> LineText) then begin
      Log.Info(Format('Issue at line %d untethered: <%s> -> <%s>', [LineNum, FLines[Delta], LineText]));
      FTethered := False;
    end;
  end
  else begin
    Log.Info(Format(
      'Attempted to tether by providing line %d for issue at issue on lines %d-%d',
      [IntToStr(StartLine), IntToStr(EndLine), IntToStr(LineNum)]));
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
