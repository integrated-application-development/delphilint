unit DelphiLint.IDEContext;

interface

uses
    DelphiLint.ContextTypes
  , DelphiLint.Context
  ;

type
  TIDELintContext = class(TLintContext)
  private
    FAnalyzer: TAnalyzer;
    FLogger: TLogger;
  protected
    function GetAnalyzer: TAnalyzer; override;
    function GetLogger: TLogger; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , DelphiLint.Analyzer
  , DelphiLint.Logger
  ;

//______________________________________________________________________________________________________________________

destructor TIDELintContext.Destroy;
begin
  FreeAndNil(FAnalyzer);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetAnalyzer: TAnalyzer;
begin
  if not Assigned(FAnalyzer) then begin
    FAnalyzer := TAnalyzerImpl.Create;
  end;

  Result := FAnalyzer;
end;

//______________________________________________________________________________________________________________________

function TIDELintContext.GetLogger: TLogger;
var
  LogDir: string;
  LogPath: string;
begin
  if not Assigned(FLogger) then begin
    LogDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint\logs');
    TDirectory.CreateDirectory(LogDir);
    LogPath := TPath.Combine(LogDir, Format('delphilint-client_%s.log', [FormatDateTime('yyyymmdd_hhnnss', Now)]));
    FLogger := TFileLogger.Create(LogPath);
  end;

  Result := FLogger;
end;

//______________________________________________________________________________________________________________________

initialization
  SetLintContext(TIDELintContext.Create);

end.
