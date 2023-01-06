unit DelphiLintIDE;

interface

uses
    System.SysUtils
  , ToolsAPI
  , DelphiLintServer
  , Vcl.Dialogs
  , Vcl.Graphics
  , WinAPI.Windows
  , System.Classes
  , System.Generics.Collections
  , DelphiLintData     
  , DelphiLintLogger
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

  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLintIssue>; overload;

    procedure AnalyzeActiveProject;

    property Server: TLintServer read FServer;
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

//______________________________________________________________________________________________________________________

procedure Register;

function LintIDE: TLintIDE;

implementation

uses
    System.StrUtils
  , DelphiLintFileUtils
  , System.Generics.Defaults
  ;

var
  G_LintIDE: TLintIDE;

//______________________________________________________________________________________________________________________

procedure TLintIDE.AnalyzeActiveProject;
var
  AllFiles: TArray<string>;
  ProjectFile: string;
  MainFile: string;
  PasFiles: TArray<string>;
begin
  if not FAnalyzing then begin
    FAnalyzing := True;
    FOutputLog.Clear;
    FOutputLog.Info('Analysis in progress...');

    DelphiLintFileUtils.ExtractFiles(AllFiles, ProjectFile, MainFile, PasFiles);

    FLastAnalyzedFiles.Clear;
    FLastAnalyzedFiles.AddStrings(AllFiles);

    LintIDE.Server.Analyze(
      DelphiLintFileUtils.GetProjectDirectory(MainFile),
      AllFiles,
      LintIDE.OnAnalyzeResult,
      LintIDE.OnAnalyzeError);
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
end;

//______________________________________________________________________________________________________________________

constructor TLintIDE.Create;
begin
  inherited;
  FActiveIssues := TDictionary<string, TList<TLintIssue>>.Create;
  FServer := TLintServer.Create('{URL REMOVED}');
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
        ToWindowsPath(DelphiLintFileUtils.GetProjectDirectory + '\' + Issue.FilePath),
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
  SanitizedName: string;
  Issue: TLintIssue;
  ResultList: TList<TLintIssue>;
begin
  SanitizedName := ToUnixPath(FileName);

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

initialization

finalization
  FreeAndNil(G_LintIDE);

end.
