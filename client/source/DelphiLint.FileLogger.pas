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
unit DelphiLint.FileLogger;

interface

uses
    System.SyncObjs
  , DelphiLint.Context
  ;

type
  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FLogPath: string;
    FLock: TMutex;

    function GetTimeStr: string;

    procedure WriteLogFile(Msg: string);
    procedure DoLog(Level: string; Msg: string);
  public
    constructor Create(LogPath: string);
    destructor Destroy; override;

    procedure Debug(const Msg: string); overload;
    procedure Debug(const Msg: string; const Args: array of const); overload;

    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;

    procedure Warn(const Msg: string); overload;
    procedure Warn(const Msg: string; const Args: array of const); overload;
  end;

implementation

uses
    System.SysUtils
  , System.Classes
  , System.IOUtils
  ;

//______________________________________________________________________________________________________________________

constructor TFileLogger.Create(LogPath: string);
begin
  inherited Create;

  FLogPath := LogPath;
  FLock := TMutex.Create;

  if not TFile.Exists(FLogPath) then begin
    FreeAndNil(TFile.Create(FLogPath));
  end;
end;

//______________________________________________________________________________________________________________________

destructor TFileLogger.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.DoLog(Level: string; Msg: string);
begin
  WriteLogFile(Format('%s [%s] %s', [GetTimeStr, Level, Msg]));
end;

//______________________________________________________________________________________________________________________

function TFileLogger.GetTimeStr: string;
begin
  Result := FormatDateTime('hh:nn:ss.zzz', Now);
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.WriteLogFile(Msg: string);
var
  Stream: TFileStream;
  MsgBytes: TBytes;
begin
  FLock.Acquire;
  try
    MsgBytes := TEncoding.UTF8.GetBytes(Msg + #13#10);
    Stream := TFileStream.Create(FLogPath, fmOpenWrite or fmShareDenyNone);
    Stream.Seek(0, soFromEnd);
    Stream.Write(MsgBytes, Length(MsgBytes));
  finally
    FreeAndNil(Stream);
    FLock.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.Info(const Msg: string);
begin
  DoLog('INFO', Msg);
end;

procedure TFileLogger.Info(const Msg: string; const Args: array of const);
begin
  Info(Format(Msg, Args));
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.Debug(const Msg: string);
begin
  DoLog('DBUG', Msg);
end;

procedure TFileLogger.Debug(const Msg: string; const Args: array of const);
begin
  Debug(Format(Msg, Args));
end;

//______________________________________________________________________________________________________________________

procedure TFileLogger.Warn(const Msg: string);
begin
  DoLog('WARN', Msg);
end;

procedure TFileLogger.Warn(const Msg: string; const Args: array of const);
begin
  Warn(Format(Msg, Args));
end;

//______________________________________________________________________________________________________________________

end.
