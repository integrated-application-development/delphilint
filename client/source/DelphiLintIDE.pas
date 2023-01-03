unit DelphiLintIDE;

interface

uses
    System.SysUtils
  , ToolsAPI
  , DelphiLintServer
  , Vcl.Dialogs
  ;

type

//______________________________________________________________________________________________________________________

  TLintIDE = class(TObject)
  private
    FServer: TLintServer;
  public
    constructor Create;
    destructor Destroy; override;

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
  , DelphiLintData
  ;

var
  G_LintIDE: TLintIDE;

//______________________________________________________________________________________________________________________

procedure AnalyzeActiveFile;
const
  C_SampleBaseDir: string = '{PATH REMOVED}';
  C_SampleFiles: array of string = [
    'Common/Delphi/DelphiExpectedBehaviour/DelphiExpectedBehaviour.dproj',
    'Common/Delphi/DelphiExpectedBehaviour/DelphiExpectedBehaviour.dpr',
    'Common/Delphi/DelphiExpectedBehaviour/DateTimeBehaviour.pas',
    'Common/Delphi/DelphiExpectedBehaviour/DelphiExpectedBehaviourTestSuite.pas',
    'Common/Delphi/DelphiExpectedBehaviour/MidasBug.pas',
    'Common/Delphi/DelphiExpectedBehaviour/OpenArrayBug.pas',
    'Common/Delphi/DelphiExpectedBehaviour/RecordFinalizationBehaviour.pas',
    'Common/Delphi/DelphiExpectedBehaviour/SoapBug.pas',
    'Common/Delphi/DelphiExpectedBehaviour/SystemMathBug.pas',
    'Common/Delphi/DelphiExpectedBehaviour/SystemRegularExpressionsBehaviour.pas',
    'Common/Delphi/DelphiExpectedBehaviour/SystemSysUtilsBug.pas'];
begin
  LintIDE.Server.Analyze(
    C_SampleBaseDir,
    C_SampleFiles,
    procedure(Issues: TArray<TLintIssue>)
    var
      Issue: TLintIssue;
    begin
      for Issue in Issues do begin
        ShowMessage(Format('[%s, %d:%d - %d:%d] %s (%s)', [
          Issue.FilePath,
          Issue.Range.StartLine,
          Issue.Range.StartLineOffset,
          Issue.Range.EndLine,
          Issue.Range.EndLineOffset,
          Issue.RuleKey,
          Issue.Message]));
      end;
    end);
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
  FServer := TLintServer.Create('{URL REMOVED}');
end;

//______________________________________________________________________________________________________________________

destructor TLintIDE.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
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
