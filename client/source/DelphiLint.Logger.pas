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
    System.SyncObjs
  , DelphiLint.ContextTypes
  ;

type
  TFileLogger = class(TLogger)
  private
    FLogPath: string;
    FLock: TMutex;

    function GetMessagePrefix: string;

    procedure WriteLogFile(Msg: string);
  public
    constructor Create(LogPath: string);
    destructor Destroy; override;

    procedure Info(const Msg: string); overload; override;
    procedure Info(const Msg: string; const Args: array of const); overload; override;
    procedure Info(Msg: string; FileName: string; Line: Integer; Column: Integer); overload;
  end;

implementation

uses
    System.SysUtils
  , System.IOUtils
  ;

//______________________________________________________________________________________________________________________

constructor TFileLogger.Create(LogPath: string);
begin
  inherited Create;

  FLogPath := LogPath;
  FLock := TMutex.Create;
end;

//______________________________________________________________________________________________________________________

destructor TFileLogger.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TFileLogger.GetMessagePrefix: string;
begin
  Result := FormatDateTime('hh:nn:ss.zzz', Now);
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.Info(const Msg: string);
begin
  Info(Msg, '', 0, 0);
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.Info(Msg, FileName: string; Line, Column: Integer);
var
  Prefix: string;
begin
  Prefix := GetMessagePrefix;

  WriteLogFile(Format('[%s] %s', [Prefix, Msg]));
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.WriteLogFile(Msg: string);
begin
  FLock.Acquire;
  try
    TFile.AppendAllText(FLogPath, Msg + #13#10);
  finally
    FLock.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.Info(const Msg: string; const Args: array of const);
begin
  Info(Format(Msg, Args));
end;

end.
