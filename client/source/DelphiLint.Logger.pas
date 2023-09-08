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
  , DelphiLint.Context
  ;

type
  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FLogPath: string;
    FLock: TMutex;

    function GetMessagePrefix: string;

    procedure WriteLogFile(Msg: string);
  public
    constructor Create(LogPath: string);
    destructor Destroy; override;

    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Info(Msg: string; FileName: string; Line: Integer; Column: Integer); overload;
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

procedure TFileLogger.Info(const Msg: string; const Args: array of const);
begin
  Info(Format(Msg, Args));
end;

end.
