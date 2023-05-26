unit DelphiLint.Logger;

interface

uses
    ToolsAPI
  ;

type
  TLintLogger = class(TObject)
  private
    FLogMessageGroup: IOTAMessageGroup;

  public
    constructor Create(MessageGroupName: string);
    destructor Destroy; override;

    procedure Title(Msg: string);
    procedure Info(Msg: string); overload;
    procedure Info(Msg: string; FileName: string; Line: Integer; Column: Integer); overload;
    procedure Clear;
  end;

function Log: TLintLogger;

implementation

var
  G_Log: TLintLogger;

function Log: TLintLogger;
begin
  if not Assigned(G_Log) then begin
    G_Log := TLintLogger.Create('Log');
  end;

  Result := G_Log;
end;

{ TLintLogger }

procedure TLintLogger.Clear;
begin
  (BorlandIDEServices as IOTAMessageServices).ClearMessageGroup(FLogMessageGroup);
end;

constructor TLintLogger.Create(MessageGroupName: string);
begin
  FLogMessageGroup := (BorlandIDEServices as IOTAMessageServices).AddMessageGroup('DelphiLint - ' + MessageGroupName);
end;

destructor TLintLogger.Destroy;
begin
  (BorlandIDEServices as IOTAMessageServices).RemoveMessageGroup(FLogMessageGroup);
  inherited;
end;

procedure TLintLogger.Info(Msg: string);
begin
  Info(Msg, '', 0, 0);
end;

procedure TLintLogger.Info(Msg, FileName: string; Line, Column: Integer);
var
  Dummy: Pointer;
begin
  (BorlandIDEServices as IOTAMessageServices).AddToolMessage(
    FileName,
    Msg,
    'DelphiLint',
    Line,
    Column,
    nil,
    Dummy,
    FLogMessageGroup);
end;

procedure TLintLogger.Title(Msg: string);
begin
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Msg, FLogMessageGroup);
end;

end.
