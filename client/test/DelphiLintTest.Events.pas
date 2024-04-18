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
unit DelphiLintTest.Events;

interface

uses
    DUnitX.TestFramework
  ;

type
  [TestFixture]
  TEventNotifierTest = class(TObject)
  public
    [Test]
    procedure TestNotifyObjectProc;
    [Test]
    procedure TestNotifyAnonymousProc;
    [Test]
    procedure TestNotifyTopLevelProc;
    [Test]
    procedure TestNotifyMultiple;
    [Test]
    procedure TestRemoveListener;
  end;

implementation

uses
    DelphiLint.Events
  , System.SysUtils
  ;

//______________________________________________________________________________________________________________________

procedure TEventNotifierTest.TestNotifyAnonymousProc;
const
  CMessage = 'Hello world';
var
  Notifier: TEventNotifier<string>;
  Notified: Boolean;
begin
  Notifier := TEventNotifier<string>.Create;
  try
    Notifier.AddListener(
      procedure(const Msg: string) begin
        Assert.AreEqual(CMessage, Msg);
        Notified := True;
      end
    );

    Notifier.Notify(CMessage);

    Assert.IsTrue(Notified);
  finally
    FreeAndNil(Notifier);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEventNotifierTest.TestNotifyMultiple;
const
  CMessage = 'Hello world';
var
  Notifier: TEventNotifier<string>;
  NotifiedA: Boolean;
  NotifiedB: Boolean;
  NotifiedC: Boolean;
begin
  Notifier := TEventNotifier<string>.Create;
  try
    Notifier.AddListener(
    procedure(const Msg: string) begin
      Assert.AreEqual(CMessage, Msg);
      NotifiedA := True;
    end);
    Notifier.AddListener(
    procedure(const Msg: string) begin
      Assert.AreEqual(CMessage, Msg);
      NotifiedB := True;
    end);
    Notifier.AddListener(
    procedure(const Msg: string) begin
      Assert.AreEqual(CMessage, Msg);
      NotifiedC := True;
    end);

    Assert.IsFalse(NotifiedA or NotifiedB or NotifiedC);

    Notifier.Notify(CMessage);

    Assert.IsTrue(NotifiedA and NotifiedB and NotifiedC);
  finally
    FreeAndNil(Notifier);
  end;
end;

//______________________________________________________________________________________________________________________

type
  TNotifiableDummy = class(TObject)
  private
    FMessage: string;
  public
    procedure Notify(const Msg: string);
    property Message: string read FMessage;
  end;

procedure TNotifiableDummy.Notify(const Msg: string);
begin
  FMessage := Msg;
end;

//______________________________________________________________________________________________________________________

procedure TEventNotifierTest.TestNotifyObjectProc;
const
  CMessage = 'Hello world';
var
  Notifier: TEventNotifier<string>;
  Dummy: TNotifiableDummy;
begin
  Notifier := TEventNotifier<string>.Create;
  try
    Dummy := TNotifiableDummy.Create;
    Notifier.AddListener(Dummy.Notify);
    Notifier.Notify(CMessage);
    Assert.AreEqual(CMessage, Dummy.Message);
  finally
    FreeAndNil(Dummy);
    FreeAndNil(Notifier);
  end;
end;

//______________________________________________________________________________________________________________________

type
  EMyTestError = type Exception;

procedure TestNotify(const Msg: string);
begin
  raise EMyTestError.Create(Msg);
end;

procedure TEventNotifierTest.TestNotifyTopLevelProc;
const
  CMessage = 'Hello world';
var
  Notifier: TEventNotifier<string>;
begin
  Notifier := TEventNotifier<string>.Create;
  try
    Notifier.AddListener(TestNotify);
    Assert.WillRaiseWithMessage(
      procedure begin
        Notifier.Notify(CMessage);
      end,
      EMyTestError,
      CMessage);
  finally
    FreeAndNil(Notifier);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEventNotifierTest.TestRemoveListener;
const
  CMessage = 'Hello world';
var
  Notifier: TEventNotifier<string>;
  Listener: TEventListener<string>;
  NotNotified: Boolean;
begin
  NotNotified := True;
  Listener := procedure(const Msg: string) begin
    NotNotified := False;
  end;

  Notifier := TEventNotifier<string>.Create;
  try
    Notifier.AddListener(Listener);
    Notifier.RemoveListener(Listener);
    Assert.IsTrue(NotNotified);

    Notifier.Notify(CMessage);
    Assert.IsTrue(NotNotified);
  finally
    FreeAndNil(Notifier);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TEventNotifierTest);

end.
