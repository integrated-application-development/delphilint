unit DelphiLint.ContextTypes;

interface

uses
    DelphiLint.Events
  , DelphiLint.Data
  ;

type
  TLogger = class abstract(TObject)
  public
    procedure Info(const Msg: string); overload; virtual; abstract;
    procedure Info(const Msg: string; const Args: array of const); overload; virtual; abstract;
  end;

  TAnalyzer = class abstract(TObject)
  protected
    function GetOnAnalysisStarted: TEventNotifier<TArray<string>>; virtual; abstract;
    function GetOnAnalysisComplete: TEventNotifier<TArray<string>>; virtual; abstract;
    function GetOnAnalysisFailed: TEventNotifier<TArray<string>>; virtual; abstract;
    function GetCurrentAnalysis: TCurrentAnalysis; virtual; abstract;
    function GetInAnalysis: Boolean; virtual; abstract;
  public
    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLiveIssue>; overload; virtual; abstract;
    function GetRule(RuleKey: string; AllowRefresh: Boolean = True): TRule; virtual; abstract;

    procedure UpdateIssueLine(FilePath: string; OriginalLine: Integer; NewLine: Integer); virtual; abstract;

    procedure AnalyzeActiveFile; virtual; abstract;
    procedure AnalyzeOpenFiles; virtual; abstract;
    procedure RestartServer; virtual; abstract;

    function GetAnalysisStatus(Path: string): TFileAnalysisStatus; virtual; abstract;
    function TryGetAnalysisHistory(Path: string; out History: TFileAnalysisHistory): Boolean; virtual; abstract;

    property OnAnalysisStarted: TEventNotifier<TArray<string>> read GetOnAnalysisStarted;
    property OnAnalysisComplete: TEventNotifier<TArray<string>> read GetOnAnalysisComplete;
    property OnAnalysisFailed: TEventNotifier<TArray<string>> read GetOnAnalysisFailed;
    property CurrentAnalysis: TCurrentAnalysis read GetCurrentAnalysis;
    property InAnalysis: Boolean read GetInAnalysis;
  end;

implementation

end.
