unit DelphiLint.Context;

interface

uses
    DelphiLint.ContextTypes
  ;

type
  TSingleUseLogger = class(TLogger)
  public
    procedure Info(const Msg: string); overload; override;
    procedure Info(const Msg: string; const Args: array of const); overload; override;
  end;

  TLintContext = class abstract(TObject)
  protected
    function GetAnalyzer: TAnalyzer; virtual; abstract;
    function GetLogger: TLogger; virtual; abstract;
  public
    property Analyzer: TAnalyzer read GetAnalyzer;
    property Log: TLogger read GetLogger;
  end;

function LintContext: TLintContext;
function Analyzer: TAnalyzer;
function ContextValid: Boolean;
function Log: TLogger;

procedure SetLintContext(Context: TLintContext);

implementation

uses
    System.SysUtils
  ;

var
  GLintContext: TLintContext;
  GFinalized: Boolean = False;

//______________________________________________________________________________________________________________________

function LintContext: TLintContext;
begin
  Result := GLintContext;
end;

//______________________________________________________________________________________________________________________

function Analyzer: TAnalyzer;
begin
  Result := LintContext.Analyzer;
end;

//______________________________________________________________________________________________________________________

function Log: TLogger;
var
  Context: TLintContext;
begin
  Context := LintContext;
  if Assigned(Context) then begin
    Result := Context.Log;
  end
  else begin
    Result := TSingleUseLogger.Create;
  end;
end;

//______________________________________________________________________________________________________________________

procedure SetLintContext(Context: TLintContext);
begin
  FreeAndNil(GLintContext);
  GLintContext := Context;
end;

//______________________________________________________________________________________________________________________

function ContextValid: Boolean;
begin
  Result := not GFinalized;
end;

//______________________________________________________________________________________________________________________

procedure TSingleUseLogger.Info(const Msg: string; const Args: array of const);
begin
  Free;
end;

//______________________________________________________________________________________________________________________

procedure TSingleUseLogger.Info(const Msg: string);
begin
  Free;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  GFinalized := True;
  FreeAndNil(GLintContext);

end.
