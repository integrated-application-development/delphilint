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
unit DelphiLintTest.Handlers;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.Data
  ;

type
  [TestFixture]
  THandlerTest = class(TObject)
  public
    [Test]
    procedure TestReleaseTriggersOnReleased;
  end;

  [TestFixture]
  TEditLineHandlerTest = class(TObject)
  public
    [Test]
    procedure TestCallbackCalledOnLineChanged;
  end;

  [TestFixture]
  TLineTrackerTest = class(TObject)
  public
    [Test]
    procedure TestTrackLine;
    [Test]
    procedure TestClearTracking;
    [Test]
    procedure TestAddsIDENotifierOnCreate;
    [Test]
    procedure TestRemovesIDENotifierOnDestroy;
    [Test]
    procedure TestNotifiesOnLineChanged;
    [Test]
    procedure TestNotifiesOnEditorClosed;
    [Test]
    procedure TestFilePath;
  end;

  [TestFixture]
  TEditorHandlerTest = class(TObject)
  private
    function BuildLiveIssue(
      FilePath: string;
      Range: TRange;
      IssueLines: TArray<string>;
      LinesMoved: Integer = 0
    ): TLiveIssue;
  public
    [Test]
    procedure TestIssueLineUpdatedWhenTrackedLineChanged;
    [Test]
    procedure TestActivatedViewDoesNotDoubleInitTracker;
    [Test]
    [TestCase('OnViewActivated', 'activated')]
    procedure TestNewViewInitsTracker(NewType: string);
  end;

implementation

uses
    System.SysUtils
  , DelphiLint.Handlers
  , DelphiLint.Context
  , DelphiLintTest.MockContext
  , DelphiLintTest.MockUtils
  ;

type
  TEmptyHandler = class(THandler)
  end;

  TMockedEditLineTracker = class(TInterfacedObject, IIDEEditLineTracker)
  private
    FNotifier: IIDEEditLineHandler;
    FFileName: string;
    FNotifierIndex: Integer;
    FOnCleared: TProc;
    FOnLineAdded: TProc<Integer, Integer>;
  public
    constructor Create;

    function GetFileName: string;
    procedure Clear;
    function AddNotifier(Notifier: IIDEEditLineHandler): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure AddLine(const Line: Integer; const Value: Integer);

    property Notifier: IIDEEditLineHandler read FNotifier;
    property NotifierIndex: Integer read FNotifierIndex;
    property OnLineAdded: TProc<Integer, Integer> read FOnLineAdded write FOnLineAdded;
    property OnCleared: TProc read FOnCleared write FOnCleared;
  end;

//______________________________________________________________________________________________________________________

procedure THandlerTest.TestReleaseTriggersOnReleased;
var
  Handler: IIDEHandler;
  Notified: Boolean;
begin
  Notified := False;
  Handler := TEmptyHandler.Create;
  Handler.OnReleased.AddListener(
    procedure(const Hndlr: IIDEHandler) begin
      Notified := True;
      Assert.AreEqual(Handler, Hndlr);
    end);

  Handler.Release;
  Assert.IsTrue(Notified);
end;

//______________________________________________________________________________________________________________________

procedure TEditLineHandlerTest.TestCallbackCalledOnLineChanged;
var
  CallbackCalled: Boolean;
  Handler: TEditLineHandler;
begin
  CallbackCalled := False;

  Handler := TEditLineHandler.Create(
    procedure (OldLine: Integer; NewLine: Integer; Data: Integer) begin
      CallbackCalled := True;
      Assert.AreEqual(1, OldLine);
      Assert.AreEqual(2, NewLine);
      Assert.AreEqual(3, Data);
    end);
  try
    Handler.OnLineChanged(1, 2, 3);
    Assert.IsTrue(CallbackCalled);
  finally
    FreeAndNil(Handler);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestClearTracking;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
  Cleared: Boolean;
begin
  Cleared := False;

  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  Mock.OnCleared := procedure begin
    Cleared := True;
  end;

  LineTracker := TLineTracker.Create(Mock);
  try
    Assert.IsFalse(Cleared);
    LineTracker.ClearTracking;
    Assert.IsTrue(Cleared);
  finally
    FreeAndNil(LineTracker);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestFilePath;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
begin
  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  LineTracker := TLineTracker.Create(Mock);
  try
    Assert.AreEqual(Mock.GetFileName, LineTracker.FilePath);
  finally
    FreeAndNil(LineTracker);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestAddsIDENotifierOnCreate;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
begin
  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  Assert.IsNull(Mock.Notifier);
  LineTracker := TLineTracker.Create(Mock);
  try
    Assert.IsNotNull(Mock.Notifier);
  finally
    FreeAndNil(LineTracker);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestRemovesIDENotifierOnDestroy;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
begin
  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  LineTracker := TLineTracker.Create(Mock);
  try
    Assert.IsNotNull(Mock.Notifier);
  finally
    FreeAndNil(LineTracker);
  end;
  Assert.IsNull(Mock.Notifier);
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestNotifiesOnEditorClosed;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
  Notified: Boolean;
begin
  Notified := False;

  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  LineTracker := TLineTracker.Create(Mock);
  try
    LineTracker.OnEditorClosed.AddListener(
      procedure (const Hndlr: TLineTracker) begin
        Notified := True;
      end);

    Mock.Notifier.OnOwnerFreed.Notify(Mock.Notifier);
    Assert.IsTrue(Notified);
  finally
    FreeAndNil(LineTracker);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestNotifiesOnLineChanged;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
  Notified: Boolean;
begin
  Notified := False;

  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  LineTracker := TLineTracker.Create(Mock);
  try
    LineTracker.OnLineChanged.AddListener(
      procedure (const ChangedLine: TChangedLine) begin
        Notified := True;
        Assert.AreEqual(3, ChangedLine.FromLine);
        Assert.AreEqual(2, ChangedLine.ToLine);
        Assert.AreEqual(LineTracker, ChangedLine.Tracker);
      end);

    Mock.Notifier.OnLineChanged(1, 2, 3);
    Assert.IsTrue(Notified);
  finally
    FreeAndNil(LineTracker);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLineTrackerTest.TestTrackLine;
var
  LineTracker: TLineTracker;
  Mock: TMockedEditLineTracker;
  Added: Boolean;
begin
  Added := False;

  TMock.Construct<IIDEEditLineTracker, TMockedEditLineTracker>(Mock);
  Mock.OnLineAdded := procedure(Line: Integer; Data: Integer) begin
    Added := True;
    Assert.AreEqual(5, Line);
    Assert.AreEqual(5, Data);
  end;

  LineTracker := TLineTracker.Create(Mock);
  try
    LineTracker.TrackLine(5);
    Assert.IsTrue(Added);
  finally
    FreeAndNil(LineTracker);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TMockedEditLineTracker.AddLine(const Line: Integer; const Value: Integer);
begin
  if Assigned(FOnLineAdded) then begin
    FOnLineAdded(Line, Value);
  end;
end;

//______________________________________________________________________________________________________________________

function TMockedEditLineTracker.AddNotifier(Notifier: IIDEEditLineHandler): Integer;
begin
  FNotifier := Notifier;
  Result := FNotifierIndex;
end;

//______________________________________________________________________________________________________________________

procedure TMockedEditLineTracker.Clear;
begin
  if Assigned(FOnCleared) then begin
    FOnCleared;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TMockedEditLineTracker.Create;
begin
  inherited;
  FFileName := Format('file%d', [Random(1000)]);
  FNotifierIndex := Random(100);
end;

//______________________________________________________________________________________________________________________

function TMockedEditLineTracker.GetFileName: string;
begin
  Result := FFileName;
end;

//______________________________________________________________________________________________________________________

procedure TMockedEditLineTracker.RemoveNotifier(Index: Integer);
begin
  if Index <> FNotifierIndex then begin
    raise Exception.CreateFmt(
      'Tried to remove notifier with index %d, but this notifier has index %d',
      [Index, FNotifierIndex]);
  end;

  FNotifier := nil;
end;

//______________________________________________________________________________________________________________________

function TEditorHandlerTest.BuildLiveIssue(
  FilePath: string;
  Range: TRange;
  IssueLines: TArray<string>;
  LinesMoved: Integer = 0
): TLiveIssue;
var
  IssueData: TLintIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', Range);
  try
  Result := TLiveIssue.Create(IssueData, IssueLines);
  Result.LinesMoved := LinesMoved;
  finally
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandlerTest.TestActivatedViewDoesNotDoubleInitTracker;
const
  CFileName = 'abc.pas';
var
  MockAnalyzer: TMockAnalyzer;
  MockView: TMockEditView;
  MockLineTracker: TMockEditLineTracker;
  Handler: TEditorHandler;
begin
  MockContext.MockAnalyzer(TMock.Construct<IAnalyzer, TMockAnalyzer>(MockAnalyzer));
  MockAnalyzer.MockFileIssues(CFileName, [
    BuildLiveIssue(CFileName, TRange.Create(5,1,5,16), ['abcdef']),
    BuildLiveIssue(CFileName, TRange.Create(7,1,9,16), ['abcdef', 'abcdef', 'abcdef'], 3)
  ]);

  TMock.Construct<IIDEEditView, TMockEditView>(MockView);
  MockView.MockedFileName := CFileName;
  TMock.Construct<IIDEEditLineTracker, TMockEditLineTracker>(MockLineTracker);
  MockLineTracker.MockedFileName := CFileName;
  MockView.MockedLineTracker := MockLineTracker;

  Handler := TEditorHandler.Create;
  try
    Handler.OnViewActivated(MockView);

    Assert.AreEqual(2, MockLineTracker.MockedLines.Count);
    Assert.AreEqual(5, MockLineTracker.MockedLines[5]);
    Assert.AreEqual(10, MockLineTracker.MockedLines[10]);

    MockLineTracker.Clear;
    Assert.AreEqual(0, MockLineTracker.MockedLines.Count);

    Handler.OnViewActivated(MockView);
    Assert.AreEqual(0, MockLineTracker.MockedLines.Count);
  finally
    FreeAndNil(Handler);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandlerTest.TestNewViewInitsTracker(NewType: string);
const
  CFileName = 'abc.pas';
var
  MockAnalyzer: TMockAnalyzer;
  MockView: TMockEditView;
  MockLineTracker: TMockEditLineTracker;
  Handler: TEditorHandler;
begin
  MockContext.MockAnalyzer(TMock.Construct<IAnalyzer, TMockAnalyzer>(MockAnalyzer));
  MockAnalyzer.MockFileIssues(CFileName, [
    BuildLiveIssue(CFileName, TRange.Create(5,1,5,16), ['abcdef']),
    BuildLiveIssue(CFileName, TRange.Create(7,1,9,16), ['abcdef', 'abcdef', 'abcdef'], 3)
  ]);

  TMock.Construct<IIDEEditView, TMockEditView>(MockView);
  MockView.MockedFileName := CFileName;
  TMock.Construct<IIDEEditLineTracker, TMockEditLineTracker>(MockLineTracker);
  MockLineTracker.MockedFileName := CFileName;
  MockView.MockedLineTracker := MockLineTracker;

  Handler := TEditorHandler.Create;
  try
    if NewType = 'activated' then begin
      Handler.OnViewActivated(MockView);
    end
    else if NewType = 'added' then begin
      Handler.OnViewAdded(MockView);
    end
    else begin
      Assert.Fail('Unknown view adding type');
    end;

    Assert.AreEqual(2, MockLineTracker.MockedLines.Count);
    Assert.AreEqual(5, MockLineTracker.MockedLines[5]);
    Assert.AreEqual(10, MockLineTracker.MockedLines[10]);
  finally
    FreeAndNil(Handler);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandlerTest.TestIssueLineUpdatedWhenTrackedLineChanged;
var
  MockAnalyzer: TMockAnalyzer;
  MockView: TMockEditView;
  MockLineTracker: TMockEditLineTracker;
  Handler: TEditorHandler;
  Updated: Boolean;
  LineHandler: IIDEEditLineHandler;
begin
  Updated := False;

  MockContext.MockAnalyzer(TMock.Construct<IAnalyzer, TMockAnalyzer>(MockAnalyzer));
  MockAnalyzer.OnCalled.AddListener(
    procedure (const EventInfo: THookedEventInfo<TAnalyzerCallType>) begin
      if EventInfo.Method = azcUpdateIssueLine then begin
        Updated := True;
        Assert.AreEqual('abc.pas', string(EventInfo.Args[0].VUnicodeString));
        Assert.AreEqual(20, EventInfo.Args[1].VInteger);
        Assert.AreEqual(25, EventInfo.Args[2].VInteger);
      end;
    end);

  TMock.Construct<IIDEEditView, TMockEditView>(MockView);
  MockView.MockedFileName := 'abc.pas';
  TMock.Construct<IIDEEditLineTracker, TMockEditLineTracker>(MockLineTracker);
  MockLineTracker.MockedFileName := 'abc.pas';
  MockView.MockedLineTracker := MockLineTracker;

  Handler := TEditorHandler.Create;
  try
    Handler.OnViewAdded(MockView);

    Assert.AreEqual(1, MockLineTracker.MockedNotifiers.Count);
    LineHandler := MockLineTracker.MockedNotifiers[0];

    LineHandler.OnLineChanged(24, 25, 20);

    Assert.IsTrue(Updated);
  finally
    FreeAndNil(Handler);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  Randomize;
  TDUnitX.RegisterTestFixture(THandlerTest);
  TDUnitX.RegisterTestFixture(TEditLineHandlerTest);
  TDUnitX.RegisterTestFixture(TLineTrackerTest);
  TDUnitX.RegisterTestFixture(TEditorHandlerTest);

end.
