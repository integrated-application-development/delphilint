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

  TDelphiLintIDE = class(TObject)
  private
    FServer: TDelphiLintServer;
  public
    constructor Create;
    destructor Destroy; override;

    property Server: TDelphiLintServer read FServer;
  end;

//______________________________________________________________________________________________________________________
  
  TDelphiLintMenuItem = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)  
  public type
    TMenuItemAction = reference to procedure;
  private
    FName: String;
    FCaption: String;
    FAction: TMenuItemAction;
  public
    constructor Create(Name: String; Caption: String; Action: TMenuItemAction);
  
    function GetIDString: String;
    function GetName: String;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: String;
  end;

//______________________________________________________________________________________________________________________

procedure Register;

function DelphiLintIDE: TDelphiLintIDE;

implementation

uses
    System.StrUtils
  , DelphiLintData
  ;

var
  G_DelphiLintIDE: TDelphiLintIDE;

//______________________________________________________________________________________________________________________

procedure Register;
const
  C_SampleBaseDir: String = '{PATH REMOVED}';
  C_SampleFiles: array of String = [
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
  RegisterPackageWizard(TDelphiLintMenuItem.Create(
    'analyze',
    'Analyze Active File with DelphiLint',
    procedure
    begin
      DelphiLintIDE.Server.Analyze(
        C_SampleBaseDir,
        C_SampleFiles,
        procedure(Issues: TArray<TDelphiLintIssue>)
        var
          Issue: TDelphiLintIssue;
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

    end
  ));
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintIDE.Create;
begin
  inherited;
  FServer := TDelphiLintServer.Create('{URL REMOVED}');
end;

//______________________________________________________________________________________________________________________

destructor TDelphiLintIDE.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

//______________________________________________________________________________________________________________________

function DelphiLintIDE: TDelphiLintIDE;
begin
  if not Assigned(G_DelphiLintIDE) then begin
    G_DelphiLintIDE := TDelphiLintIDE.Create;
  end;

  Result := G_DelphiLintIDE;
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintMenuItem.Create(Name: String; Caption: String; Action: TMenuItemAction);
begin
  FName := Name;
  FCaption := Caption;
  FAction := Action;
end;

//______________________________________________________________________________________________________________________

procedure TDelphiLintMenuItem.Execute;
begin
  FAction;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetIDString: String;
begin
  Result := 'DelphiLint|' + FName;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetMenuText: String;
begin
  Result := FCaption;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetName: String;
begin
  Result := FName;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(G_DelphiLintIDE);

end.
