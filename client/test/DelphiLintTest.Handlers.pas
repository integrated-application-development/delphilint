unit DelphiLintTest.Handlers;

interface

uses
    DUnitX.TestFramework
  ;

type
  [TestFixture]
  THandlerTest = class(TObject)
  public
    [TestCase]
    procedure TestReleaseTriggersOnReleased;
  end;

  [TestFixture]
  TEditLineHandlerTest = class(TObject)
  public
    [TestCase]
    procedure TestCallbackCalledOnLineChanged;
  end;

  [TestFixture]
  TLineTrackerTest = class(TObject)
  public
    [TestCase]
    procedure TestTrackLine;
    [TestCase]
    procedure TestClearTracking;
    [TestCase]
    procedure TestAddsIDENotifierOnCreate;
    [TestCase]
    procedure TestRemovesIDENotifierOnDestroy;
    [TestCase]
    procedure TestNotifiesOnLineChanged;
    [TestCase]
    procedure TestNotifiesOnEditorClosed;
    [TestCase]
    procedure TestFilePath;
  end;

implementation

uses
    System.SysUtils
  , DelphiLint.Handlers
  , DelphiLint.Context
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

procedure TMockedEditLineTracker.AddLine(const Line, Value: Integer);
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

initialization
  Randomize;
  TDUnitX.RegisterTestFixture(THandlerTest);
  TDUnitX.RegisterTestFixture(TEditLineHandlerTest);
  TDUnitX.RegisterTestFixture(TLineTrackerTest);

end.
