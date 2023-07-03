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
unit DelphiLint.Logger;

interface

uses
    System.Classes
  , System.SyncObjs
  ;

type
  TLintLogger = class(TObject)
  private
    FIncludeTime: Boolean;
    FLogPath: string;
    FLock: TMutex;

    function GetMessagePrefix: string;

    procedure WriteLogFile(Msg: string);
  public
    constructor Create(MessageGroupName: string; IncludeTime: Boolean = True);
    destructor Destroy; override;

    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Info(Msg: string; FileName: string; Line: Integer; Column: Integer); overload;
    procedure Clear;
  end;

function Log: TLintLogger;

implementation

uses
    System.SysUtils
  , System.IOUtils
  ;

var
  G_Log: TLintLogger;

function Log: TLintLogger;
begin
  if not Assigned(G_Log) then begin
    G_Log := TLintLogger.Create('Log');
  end;

  Result := G_Log;
end;

//______________________________________________________________________________________________________________________

constructor TLintLogger.Create(MessageGroupName: string; IncludeTime: Boolean = True);
begin
  FLogPath := TPath.Combine(
    TPath.GetTempPath,
    Format('delphilint-client_%s.log', [FormatDateTime('yyyymmdd_hhnnss', Now)]));
  FIncludeTime := IncludeTime;
  FLock := TMutex.Create;
end;

//______________________________________________________________________________________________________________________

destructor TLintLogger.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TLintLogger.GetMessagePrefix: string;
begin
  if FIncludeTime then begin
    Result := FormatDateTime('hh:nn:ss.zzz', Now);
  end
  else begin
    Result := '';
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintLogger.Clear;
begin
end;

//______________________________________________________________________________________________________________________

procedure TLintLogger.Info(const Msg: string);
begin
  Info(Msg, '', 0, 0);
end;

//______________________________________________________________________________________________________________________

procedure TLintLogger.Info(Msg, FileName: string; Line, Column: Integer);
var
  Prefix: string;
begin
  Prefix := GetMessagePrefix;

  WriteLogFile(Format('[%s] %s', [Prefix, Msg]));
end;

//______________________________________________________________________________________________________________________

procedure TLintLogger.WriteLogFile(Msg: string);
begin
  FLock.Acquire;
  try
    TFile.AppendAllText(FLogPath, Msg + #13#10);
  finally
    FLock.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintLogger.Info(const Msg: string; const Args: array of const);
begin
  Info(Format(Msg, Args));
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(G_Log);

end.
