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
  ;

type

//______________________________________________________________________________________________________________________

  TLintIDE = class(TObject)
  private
    FServer: TLintServer;
    FActiveIssues: TDictionary<string, TList<TLintIssue>>;

    function ToUnixPath(Path: string; Lower: Boolean = False): string;
    function ToWindowsPath(Path: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    function GetIssues(FileName: string; Line: Integer = -1): TArray<TLintIssue>; overload;
    procedure RefreshIssues(Issues: TArray<TLintIssue>);
    procedure GenerateIssueMessages;

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
  , DelphiLintLogger
  ;

var
  G_LintIDE: TLintIDE;

//______________________________________________________________________________________________________________________

procedure AnalyzeActiveFile;
const
  C_SampleBaseDir: string = '{PATH REMOVED}';
  C_SampleFiles: array of string = [
    'DelphiLintData.pas',
    'DelphiLintIDE.pas',
    'DelphiLintServer.pas',
    'DelphiLintLogger.pas',
    'DelphiLintClient.dpk',
    'DelphiLintClient.dproj'
  ];
begin
  LintIDE.Server.Analyze(
    C_SampleBaseDir,
    C_SampleFiles,
    LintIDE.RefreshIssues);
end;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  RegisterPackageWizard(TLintMenuItem.Create(
    'analyze',
    'Analyze Active File with DelphiLint',
    AnalyzeActiveFile
  ));
end;

//______________________________________________________________________________________________________________________

constructor TLintIDE.Create;
begin
  inherited;
  FActiveIssues := TDictionary<string, TList<TLintIssue>>.Create;
  FServer := TLintServer.Create('{URL REMOVED}');

  Log.Info('DelphiLint started.');
end;

//______________________________________________________________________________________________________________________

destructor TLintIDE.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FActiveIssues);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.GenerateIssueMessages;
var
  Issue: TLintIssue;
  FileName: string;
begin
  Log.Clear;

  for FileName in FActiveIssues.Keys do begin
    Log.Title(Format('[DelphiLint] %s (%d issues)', [FileName, FActiveIssues[FileName].Count]));

    for Issue in FActiveIssues[FileName] do begin
      Log.Info(
        Format('%s: %s', [Issue.RuleKey, Issue.Message]),
        ToWindowsPath('{PATH REMOVED}' + FileName),
        Issue.Range.StartLine,
        Issue.Range.StartLineOffset);
    end;
  end;
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
    end
    else begin
      ResultList := TList<TLintIssue>.Create;

      for Issue in FActiveIssues[SanitizedName] do begin
        if (Issue.Range.StartLine >= Line) and (Issue.Range.EndLine <= Line) then begin
          ResultList.Add(Issue);
        end;
      end;

      Result := ResultList.ToArray;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintIDE.RefreshIssues(Issues: TArray<TLintIssue>);
var
  Issue: TLintIssue;
  SanitizedPath: string;
begin
  FActiveIssues.Clear;

  Log.Info(Format('Processing %d issues.', [Length(Issues)]));

  for Issue in Issues do begin
    SanitizedPath := ToUnixPath(Issue.FilePath, True);

    if not FActiveIssues.ContainsKey(SanitizedPath) then begin
      FActiveIssues.Add(SanitizedPath, TList<TLintIssue>.Create);
    end;

    FActiveIssues[SanitizedPath].Add(Issue);
  end;

  GenerateIssueMessages;
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
