unit DelphiLintTest.MockContext;

interface

uses
    System.Generics.Collections
  , DelphiLint.Context
  , DelphiLint.Events
  , DelphiLint.Data
  , DelphiLint.Settings
  , DelphiLint.ProjectOptions
  , DelphiLintTest.MockUtils
  ;

type
  TAnalyzerCallType = (
    azcAnalyzeActiveFile,
    azcAnalyzeOpenFiles,
    azcRestartServer,
    azcUpdateIssueLine
  );

  TMockAnalyzer = class(THookedObject<TAnalyzerCallType>, IAnalyzer)
  private
    FOnAnalysisStarted: TEventNotifier<TArray<string>>;
    FOnAnalysisComplete: TEventNotifier<TArray<string>>;
    FOnAnalysisFailed: TEventNotifier<TArray<string>>;

    FFileHistories: TDictionary<string, TFileAnalysisHistory>;
    FFileStatuses: TDictionary<string, TFileAnalysisStatus>;
    FIssues: TObjectDictionary<string, TObjectDictionary<Integer, TLiveIssue>>;
    FRules: TObjectDictionary<string, TRule>;
    FCurrentAnalysis: TCurrentAnalysis;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MockFileHistory(Path: string; History: TFileAnalysisHistory);
    procedure MockFileIssues(Path: string; Issues: TObjectList<TLiveIssue>); overload;
    procedure MockFileIssues(Path: string; Issues: TObjectDictionary<Integer, TLiveIssue>); overload;
    procedure MockFileIssue(Path: string; Line: Integer; Issue: TLiveIssue); overload;
    procedure MockFileStatus(Path: string; Status: TFileAnalysisStatus);
    procedure MockRule(RuleKey: string; Rule: TRule);
    procedure MockCurrentAnalysis(CurrentAnalysis: TCurrentAnalysis);

    function GetOnAnalysisStarted: TEventNotifier<TArray<string>>;
    function GetOnAnalysisComplete: TEventNotifier<TArray<string>>;
    function GetOnAnalysisFailed: TEventNotifier<TArray<string>>;
    function GetCurrentAnalysis: TCurrentAnalysis;
    function GetInAnalysis: Boolean;
    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>;
    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer);

    procedure AnalyzeActiveFile;
    procedure AnalyzeOpenFiles;
    procedure RestartServer;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
  end;

  TMockLintContext = class(TInterfacedObject, ILintContext)
  private
    FAnalyzer: IAnalyzer;
    FLogger: ILogger;
    FIDEServices: IIDEServices;
    FPlugin: IPlugin;
    FSettings: TLintSettings;
    FProjectOptionsList: TObjectDictionary<string, TLintProjectOptions>;

    procedure Init;
    procedure Deinit;
  protected
    function GetAnalyzer: IAnalyzer;
    function GetLogger: ILogger;
    function GetIDEServices: IIDEServices;
    function GetPlugin: IPlugin;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MockProjectOptions(ProjectFile: string; ProjectOptions: TLintProjectOptions);
    procedure Reset;

    function GetSettings: TLintSettings;
    function GetProjectOptions(ProjectFile: string): TLintProjectOptions;
  end;

function MockContext: TMockLintContext;

implementation

uses
    System.SysUtils
  , DelphiLint.Utils
  ;

type
  TNoOpLogger = class(TInterfacedObject, ILogger)
  public
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
  end;

//______________________________________________________________________________________________________________________

function MockContext: TMockLintContext;
begin
  Result := TMockLintContext(LintContext);
end;

//______________________________________________________________________________________________________________________

constructor TMockAnalyzer.Create;
begin
  inherited;

  FFileHistories := TDictionary<string, TFileAnalysisHistory>.Create;
  FFileStatuses := TDictionary<string, TFileAnalysisStatus>.Create;
  FIssues := TObjectDictionary<string, TObjectDictionary<Integer, TLiveIssue>>.Create;
  FRules := TObjectDictionary<string, TRule>.Create;
end;

//______________________________________________________________________________________________________________________

destructor TMockAnalyzer.Destroy;
begin
  FreeAndNil(FFileHistories);
  FreeAndNil(FFileStatuses);
  FreeAndNil(FIssues);
  FreeAndNil(FRules);
  FreeAndNil(FCurrentAnalysis);

  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.AnalyzeActiveFile;
begin
  NotifyEvent(azcAnalyzeActiveFile);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.AnalyzeOpenFiles;
begin
  NotifyEvent(azcAnalyzeOpenFiles);
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetAnalysisStatus(Path: string): TFileAnalysisStatus;
begin
  Path := NormalizePath(Path);
  if not FFileStatuses.ContainsKey(Path) then begin
    raise EMockError.CreateFmt('Status for file ''%s'' has not been mocked', [Path]);
  end;

  Result := FFileStatuses[Path];
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetCurrentAnalysis: TCurrentAnalysis;
begin
  Result := FCurrentAnalysis;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetInAnalysis: Boolean;
begin
  Result := Assigned(FCurrentAnalysis);
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetIssues(FileName: string; Line: Integer): TArray<TLiveIssue>;
var
  ReturnIssues: TList<TLiveIssue>;
  LineNum: Integer;
begin
  FileName := NormalizePath(FileName);

  if FIssues.ContainsKey(FileName) then begin
    ReturnIssues := TList<TLiveIssue>.Create;
    try
      if Line = -1 then begin
        for LineNum in FIssues[FileName].Keys do begin
          ReturnIssues.AddRange(FIssues[FileName][LineNum]);
        end;
      end
      else if FIssues[FileName].ContainsKey(Line) then begin
        ReturnIssues.AddRange(FIssues[FileName][Line]);
      end;

      Result := ReturnIssues.ToArray;
    finally
      FreeAndNil(ReturnIssues);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetOnAnalysisComplete: TEventNotifier<TArray<string>>;
begin
  Result := FOnAnalysisComplete;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetOnAnalysisFailed: TEventNotifier<TArray<string>>;
begin
  Result := FOnAnalysisFailed;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetOnAnalysisStarted: TEventNotifier<TArray<string>>;
begin
  Result := FOnAnalysisStarted;
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.GetRule(RuleKey: string; AllowRefresh: Boolean): TRule;
begin
  if not FRules.ContainsKey(RuleKey) then begin
    raise EMockError.CreateFmt('Rule ''%s'' has not been mocked', [RuleKey]);
  end;

  Result := FRules[RuleKey];
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockCurrentAnalysis(CurrentAnalysis: TCurrentAnalysis);
begin
  FreeAndNil(FCurrentAnalysis);
  FCurrentAnalysis := CurrentAnalysis;
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileHistory(Path: string; History: TFileAnalysisHistory);
begin
  Path := NormalizePath(Path);
  FFileHistories.AddOrSetValue(Path, History);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileIssue(Path: string; Line: Integer; Issue: TLiveIssue);
begin
  Path := NormalizePath(Path);
  FIssues.AddOrSetValue(
    Path,
    TObjectDictionary<Integer, TLiveIssue>.Create([TPair<Integer, TLiveIssue>.Create(Line, Issue)]));
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileIssues(Path: string; Issues: TObjectList<TLiveIssue>);
var
  I: Integer;
begin
  try
    Path := NormalizePath(Path);
    FIssues.AddOrSetValue(Path, TObjectDictionary<Integer, TLiveIssue>.Create);

    for I := 0 to Issues.Count - 1 do begin
      FIssues[Path].AddOrSetValue(Issues[I].StartLine, Issues.ExtractAt(I));
    end;
  finally
    FreeAndNil(Issues);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileIssues(Path: string; Issues: TObjectDictionary<Integer, TLiveIssue>);
begin
  Path := NormalizePath(Path);
  FIssues.AddOrSetValue(Path, Issues);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockFileStatus(Path: string; Status: TFileAnalysisStatus);
begin
  Path := NormalizePath(Path);
  FFileStatuses.AddOrSetValue(Path, Status);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.MockRule(RuleKey: string; Rule: TRule);
begin
  FRules.AddOrSetValue(RuleKey, Rule);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.RestartServer;
begin
  NotifyEvent(azcRestartServer);
end;

//______________________________________________________________________________________________________________________

function TMockAnalyzer.TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean;
begin
  Path := NormalizePath(Path);
  Result := FFileHistories.TryGetValue(Path, History);
end;

//______________________________________________________________________________________________________________________

procedure TMockAnalyzer.UpdateIssueLine(FilePath: string; OriginalLine, NewLine: Integer);
begin
  NotifyEvent(azcUpdateIssueLine, [FilePath, OriginalLine, NewLine]);
end;

//______________________________________________________________________________________________________________________

constructor TMockLintContext.Create;
begin
  inherited;
  Init;
end;

//______________________________________________________________________________________________________________________

destructor TMockLintContext.Destroy;
begin
  Deinit;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.Init;
begin
  FProjectOptionsList := TObjectDictionary<string, TLintProjectOptions>.Create;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.Deinit;
begin
  FAnalyzer := nil;
  FPlugin := nil;
  FreeAndNil(FSettings);
  FLogger := nil;
  FIDEServices := nil;
  FreeAndNil(FProjectOptionsList);
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetAnalyzer: IAnalyzer;
begin
  if not Assigned(FAnalyzer) then begin
    raise EMockError.Create('Analyzer has not been mocked');
  end;

  Result := FAnalyzer;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetIDEServices: IIDEServices;
begin
  if not Assigned(FIDEServices) then begin
    raise EMockError.Create('IDE services have not been mocked');
  end;

  Result := FIDEServices;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetLogger: ILogger;
begin
  Result := TNoOpLogger.Create;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetPlugin: IPlugin;
begin
  if not Assigned(FPlugin) then begin
    raise EMockError.Create('Plugin has not been mocked');
  end;

  Result := FPlugin;
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetProjectOptions(ProjectFile: string): TLintProjectOptions;
begin
  ProjectFile := NormalizePath(ProjectFile);
  if not FProjectOptionsList.ContainsKey(ProjectFile) then begin
    raise EMockError.CreateFmt('Project options for file ''%s'' have not been mocked', [ProjectFile]);
  end;

  Result := FProjectOptionsList[ProjectFile];
end;

//______________________________________________________________________________________________________________________

function TMockLintContext.GetSettings: TLintSettings;
begin
  if not Assigned(FSettings) then begin
    raise EMockError.Create('Settings have not been mocked');
  end;

  Result := FSettings;
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.MockProjectOptions(ProjectFile: string; ProjectOptions: TLintProjectOptions);
begin
  FProjectOptionsList.AddOrSetValue(ProjectFile, ProjectOptions);
end;

//______________________________________________________________________________________________________________________

procedure TMockLintContext.Reset;
begin
  Deinit;
  Init;
end;

//______________________________________________________________________________________________________________________

procedure TNoOpLogger.Info(const Msg: string; const Args: array of const);
begin
  // No op
end;

procedure TNoOpLogger.Info(const Msg: string);
begin
  // No op
end;

//______________________________________________________________________________________________________________________

initialization
  SetLintContext(TMockLintContext.Create);

end.
