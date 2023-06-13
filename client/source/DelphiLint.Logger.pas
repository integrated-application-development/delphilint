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
    ToolsAPI
  , DelphiLint.IDEUtils
  , DelphiLint.Events
  ;

type
  TLintLogger = class;

  TLoggerMessageNotifier = class(TMessageNotifierBase)
  private
    FOnMessageGroupDeleted: TEventNotifier<IOTAMessageGroup>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MessageGroupDeleted(const Group: IOTAMessageGroup); override;

    property OnMessageGroupDeleted: TEventNotifier<IOTAMessageGroup> read FOnMessageGroupDeleted;
  end;

  TLintLogger = class(TObject)
  private
    FLogMessageGroup: IOTAMessageGroup;
    FMessageNotifier: TLoggerMessageNotifier;

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

uses
    System.SysUtils
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

{ TLintLogger }

procedure TLintLogger.Clear;
begin
  (BorlandIDEServices as IOTAMessageServices).ClearMessageGroup(FLogMessageGroup);
end;

constructor TLintLogger.Create(MessageGroupName: string);
var
  MessageServices: IOTAMessageServices;
  NotifierIndex: Integer;
begin
  MessageServices := BorlandIDEServices as IOTAMessageServices;

  FLogMessageGroup := MessageServices.AddMessageGroup('DelphiLint - ' + MessageGroupName);
  FMessageNotifier := TLoggerMessageNotifier.Create;
  FMessageNotifier.OnMessageGroupDeleted.AddListener(
    procedure(const Group: IOTAMessageGroup) begin
      if Assigned(FLogMessageGroup) and (Group.Name = FLogMessageGroup.Name) then begin
        FLogMessageGroup := nil;
      end;
    end);
  NotifierIndex := MessageServices.AddNotifier(FMessageNotifier);
  FMessageNotifier.OnReleased.AddListener(
    procedure(const Notf: TNotifierBase) begin
      (BorlandIDEServices as IOTAMessageServices).RemoveNotifier(NotifierIndex);
    end);
  FMessageNotifier.OnOwnerFreed.AddListener(
    procedure(const Notf: TNotifierBase) begin
      FLogMessageGroup := nil;
    end);
end;

destructor TLintLogger.Destroy;
begin
  FMessageNotifier.Release;
  if Assigned(FLogMessageGroup) then begin
    (BorlandIDEServices as IOTAMessageServices).RemoveMessageGroup(FLogMessageGroup);
  end;
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

constructor TLoggerMessageNotifier.Create;
begin
  inherited;
  FOnMessageGroupDeleted := TEventNotifier<IOTAMessageGroup>.Create;
end;

destructor TLoggerMessageNotifier.Destroy;
begin
  FreeAndNil(FOnMessageGroupDeleted);
  inherited;
end;

procedure TLoggerMessageNotifier.MessageGroupDeleted(const Group: IOTAMessageGroup);
begin
  FOnMessageGroupDeleted.Notify(Group);
end;

initialization

finalization
  FreeAndNil(G_Log);

end.
