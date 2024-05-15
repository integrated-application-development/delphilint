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
unit DelphiLint.Events;

interface

uses
    System.Generics.Collections
  , System.SyncObjs
  ;

type
  TEventListener<T> = reference to procedure(const Arg: T);

  TEventNotifier<T> = class(TObject)
  private
    FListeners: TList<TEventListener<T>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(Listener: TEventListener<T>);
    procedure RemoveListener(Listener: TEventListener<T>);
    procedure Notify(const Arg: T);
  end;

  TThreadSafeEventNotifier<T> = class(TObject)
  private
    FNotifier: TEventNotifier<T>;
    FMutex: TMutex;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(Listener: TEventListener<T>);
    procedure RemoveListener(Listener: TEventListener<T>);
    procedure Notify(const Arg: T);
  end;

implementation

uses
    System.SysUtils
  ;

//______________________________________________________________________________________________________________________

constructor TEventNotifier<T>.Create;
begin
  inherited;
  FListeners := TList<TEventListener<T>>.Create;
end;

//______________________________________________________________________________________________________________________

destructor TEventNotifier<T>.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TEventNotifier<T>.AddListener(Listener: TEventListener<T>);
begin
  FListeners.Add(Listener);
end;

//______________________________________________________________________________________________________________________

procedure TEventNotifier<T>.RemoveListener(Listener: TEventListener<T>);
begin
  FListeners.Remove(Listener);
end;

//______________________________________________________________________________________________________________________

procedure TEventNotifier<T>.Notify(const Arg: T);
var
  Listener: TEventListener<T>;
begin
  for Listener in FListeners do begin
    Listener(Arg);
  end;
end;

//______________________________________________________________________________________________________________________

constructor TThreadSafeEventNotifier<T>.Create;
begin
  inherited;
  FNotifier := TEventNotifier<T>.Create;
  FMutex := TMutex.Create;
end;

//______________________________________________________________________________________________________________________

destructor TThreadSafeEventNotifier<T>.Destroy;
begin
  FMutex.Acquire;
  FreeAndNil(FMutex);
  FreeAndNil(FNotifier);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TThreadSafeEventNotifier<T>.AddListener(Listener: TEventListener<T>);
begin
  FMutex.Acquire;
  try
    FNotifier.AddListener(Listener);
  finally
    FMutex.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TThreadSafeEventNotifier<T>.RemoveListener(Listener: TEventListener<T>);
begin
  FMutex.Acquire;
  try
    FNotifier.RemoveListener(Listener);
  finally
    FMutex.Release;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TThreadSafeEventNotifier<T>.Notify(const Arg: T);
begin
  FMutex.Acquire;
  try
    FNotifier.Notify(Arg);
  finally
    FMutex.Release;
  end;
end;

//______________________________________________________________________________________________________________________

end.
