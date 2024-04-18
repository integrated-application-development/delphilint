{
DelphiLint Client
Copyright (C) 2024 Integrated Application Development

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLintTest.MockUtils;

interface

uses
    System.SysUtils
  , DelphiLint.Events
  ;

type
  EMockError = class(Exception)
  end;

  TMock = class(TObject)
  public
    class function Construct<I: IInterface; T: constructor, I>(out Raw: T): I;
  end;

  THookedEventInfo<T> = record
    Method: T;
    Args: TArray<TVarRec>;
  end;

  THookedObject<TEventType> = class(TInterfacedObject)
  private
    FOnCalled: TEventNotifier<THookedEventInfo<TEventType>>;
  protected
    procedure NotifyEvent(Discriminator: TEventType; Args: array of const); overload;
  public
    constructor Create;
    destructor Destroy; override;

    property OnCalled: TEventNotifier<THookedEventInfo<TEventType>> read FOnCalled;
  end;

implementation

//______________________________________________________________________________________________________________________

class function TMock.Construct<I, T>(out Raw: T): I;
begin
  Raw := T.Create;
  Result := Raw;
end;

//______________________________________________________________________________________________________________________

constructor THookedObject<TEventType>.Create;
begin
  inherited;
  FOnCalled := TEventNotifier<THookedEventInfo<TEventType>>.Create;
end;

//______________________________________________________________________________________________________________________

destructor THookedObject<TEventType>.Destroy;
begin
  FreeAndNil(FOnCalled);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure THookedObject<TEventType>.NotifyEvent(Discriminator: TEventType; Args: array of const);
var
  CallInfo: THookedEventInfo<TEventType>;
  Index: Integer;
begin
  CallInfo.Method := Discriminator;

  SetLength(CallInfo.Args, Length(Args));
  for Index := 0 to Length(Args) - 1 do begin
    CallInfo.Args[Index] := Args[Index];
  end;

  FOnCalled.Notify(CallInfo);
end;

//______________________________________________________________________________________________________________________

end.
