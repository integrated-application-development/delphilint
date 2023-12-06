unit DelphiLintTest.Server;

interface

uses
    DUnitX.TestFramework
  , IdIOHandler
  , IdTCPServer
  , IdContext
  , DelphiLint.Events
  ;

type
  TMockTcpServer = class(TObject)
  private
    FTcpServer: TIdTCPServer;
    FPort: Integer;
    FOnExecute: TEventNotifier<TIdIOHandler>;

    procedure Execute(Context: TIdContext);
  public
    constructor Create;
    destructor Destroy; override;

    property Port: Integer read FPort;
    property OnExecute: TEventNotifier<TIdIOHandler> read FOnExecute;
  end;

  [TestFixture]
  TServerTcpConnectionTest = class(TObject)
  private const
    CLocalhost = '127.0.0.1';
  public
    [TestCase]
    procedure TestSendPing;
    [TestCase]
    procedure TestReceivePing;
    [TestCase]
    procedure TestReceiveMultipleMessagesSequentially;
    [TestCase]
    procedure TestReportsConnectedAfterConnect;
    [TestCase]
    procedure TestReportsDisconnectedAfterDisconnect;
    [TestCase]
    procedure TestReceiveMessageTimeout;
    [TestCase]
    procedure TestConnectTimeout;
  end;

implementation

uses
    System.SysUtils
  , System.JSON
  , System.SyncObjs
  , System.TimeSpan
  , IdExceptionCore
  , IdGlobal
  , DelphiLint.Server
  ;

//______________________________________________________________________________________________________________________

constructor TMockTcpServer.Create;
begin
  inherited Create;

  FOnExecute := TEventNotifier<TIdIOHandler>.Create;

  FTcpServer := TIdTCPServer.Create;
  FTcpServer.OnExecute := Execute;
  FTcpServer.Bindings.Add;
  FTcpServer.Active := True;
  FPort := FTcpServer.Bindings[0].Port;
end;

//______________________________________________________________________________________________________________________

destructor TMockTcpServer.Destroy;
begin
  FTcpServer.StopListening;
  FTcpServer.Active := False;
  FreeAndNil(FTcpServer);
  FreeAndNil(FOnExecute);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TMockTcpServer.Execute(Context: TIdContext);
begin
  FOnExecute.Notify(Context.Connection.IOHandler);
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestSendPing;
var
  MockServer: TMockTcpServer;
  Connection: TLintServerTcpConnection;
  Event: TEvent;
  Msg: TTaggedMessage;
  ReceivedCategory: Byte;
  ReceivedId: Integer;
  ReceivedLength: Integer;
  ReceivedDecodedStr: string;
  FailReason: string;
begin
  MockServer := TMockTcpServer.Create;
  try
    Event := TEvent.Create;
    MockServer.OnExecute.AddListener(
      procedure (const Handler: TIdIOHandler)
      var
        Buffer: TIdBytes;
      begin
        try
          ReceivedCategory := Handler.ReadByte;
          ReceivedId := Handler.ReadInt32;
          ReceivedLength := Handler.ReadInt32;

          Handler.ReadBytes(Buffer, ReceivedLength);
          ReceivedDecodedStr := TEncoding.UTF8.GetString(TBytes(Buffer));
        except
          on E: Exception do begin
            FailReason := E.Message;
          end;
        end;

        if Assigned(Event) then begin
          Event.SetEvent;
        end;
      end);

    Connection := TLintServerTcpConnection.Create(CLocalhost, MockServer.Port);
    Msg := TTaggedMessage.Create(TLintMessage.Create(C_Ping, TJSONString.Create('ab£c def')), 95);
    Connection.SendMessage(Msg);

    Assert.AreEqual(wrSignaled, Event.WaitFor(1000), FailReason);
    Assert.AreEqual(C_Ping, Integer(ReceivedCategory));
    Assert.AreEqual(95, ReceivedId);
    Assert.AreEqual(11, ReceivedLength);
    Assert.AreEqual('"ab£c def"', ReceivedDecodedStr);
  finally
    FreeAndNil(Msg);
    FreeAndNil(Connection);
    FreeAndNil(MockServer);
    FreeAndNil(Event);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestConnectTimeout;
var
  TimeStarted: TDateTime;
  TimeStopped: TDateTime;
begin
  Assert.WillRaise(procedure begin
      try
        TimeStarted := Now;
        TLintServerTcpConnection.Create(CLocalhost, 4567);
      finally
        TimeStopped := Now;
      end;
    end,
    EIdConnectTimeout);
  Assert.IsTrue(TTimeSpan.Subtract(TimeStopped, TimeStarted).TotalMilliseconds > 1900, 'Waited longer than 1900ms');
  Assert.IsTrue(TTimeSpan.Subtract(TimeStopped, TimeStarted).TotalMilliseconds < 2200, 'Duration less than 2200ms');
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestReceiveMessageTimeout;
var
  MockServer: TMockTcpServer;
  Connection: TLintServerTcpConnection;
  TimeStarted: TDateTime;
  TimeStopped: TDateTime;
  Msg: TTaggedMessage;
begin
  MockServer := TMockTcpServer.Create;
  try
    Connection := TLintServerTcpConnection.Create(CLocalhost, MockServer.Port);

    TimeStarted := Now;
    Msg := Connection.ReceiveMessageWithTimeout(200);
    TimeStopped := Now;

    Assert.IsNull(Msg);
    Assert.IsTrue(TTimeSpan.Subtract(TimeStopped, TimeStarted).TotalMilliseconds < 300, 'Waited longer than 300ms');
    Assert.IsTrue(TTimeSpan.Subtract(TimeStopped, TimeStarted).TotalMilliseconds > 180, 'Waited less than 180ms');
  finally
    FreeAndNil(Connection);
    FreeAndNil(MockServer);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestReceiveMultipleMessagesSequentially;
var
  MockServer: TMockTcpServer;
  Connection: TLintServerTcpConnection;
  Msg: TTaggedMessage;
begin
  MockServer := TMockTcpServer.Create;
  try
    MockServer.OnExecute.AddListener(
      procedure (const Handler: TIdIOHandler)
      var
        Buffer: TBytes;
      begin
        Handler.Write(Byte(C_Ping));
        Handler.Write(Integer(1234));
        Handler.Write(Integer(14));
        Buffer := TEncoding.UTF8.GetBytes('"hi£jk lmnop"');
        Handler.Write(TIdBytes(Buffer));

        Handler.Write(Byte(C_Pong));
        Handler.Write(Integer(5678));
        Handler.Write(Integer(14));
        Buffer := TEncoding.UTF8.GetBytes('"     xyz    "');
        Handler.Write(TIdBytes(Buffer));
      end);

    Connection := TLintServerTcpConnection.Create(CLocalhost, MockServer.Port);

    Msg := Connection.ReceiveMessageWithTimeout(1000);

    Assert.IsNotNull(Msg);
    Assert.AreEqual(C_Ping, Integer(Msg.Message.Category));
    Assert.AreEqual(1234, Msg.Id);
    Assert.AreEqual('"hi£jk lmnop"', Msg.Message.Data.ToString);
    Assert.AreEqual('hi£jk lmnop', Msg.Message.Data.Value);

    FreeAndNil(Msg);
    Msg := Connection.ReceiveMessageWithTimeout(1000);

    Assert.IsNotNull(Msg);
    Assert.AreEqual(C_Pong, Integer(Msg.Message.Category));
    Assert.AreEqual(5678, Msg.Id);
    Assert.AreEqual('"     xyz    "', Msg.Message.Data.ToString);
    Assert.AreEqual('     xyz    ', Msg.Message.Data.Value);
  finally
    FreeAndNil(Msg);
    FreeAndNil(Connection);
    FreeAndNil(MockServer);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestReceivePing;
var
  MockServer: TMockTcpServer;
  Connection: TLintServerTcpConnection;
  Msg: TTaggedMessage;
begin
  MockServer := TMockTcpServer.Create;
  try
    MockServer.OnExecute.AddListener(
      procedure (const Handler: TIdIOHandler)
      var
        Buffer: TBytes;
      begin
        Handler.Write(Byte(C_Ping));
        Handler.Write(Integer(1234));
        Handler.Write(Integer(14));
        Buffer := TEncoding.UTF8.GetBytes('"hi£jk lmnop"');
        Handler.Write(TIdBytes(Buffer));
      end);

    Connection := TLintServerTcpConnection.Create(CLocalhost, MockServer.Port);

    Msg := Connection.ReceiveMessageWithTimeout(1000);

    Assert.IsNotNull(Msg);
    Assert.AreEqual(C_Ping, Integer(Msg.Message.Category));
    Assert.AreEqual(1234, Msg.Id);
    Assert.AreEqual('"hi£jk lmnop"', Msg.Message.Data.ToString);
    Assert.AreEqual('hi£jk lmnop', Msg.Message.Data.Value);
  finally
    FreeAndNil(Msg);
    FreeAndNil(Connection);
    FreeAndNil(MockServer);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestReportsConnectedAfterConnect;
var
  MockServer: TMockTcpServer;
  Connection: TLintServerTcpConnection;
begin
  MockServer := TMockTcpServer.Create;
  try
    Connection := TLintServerTcpConnection.Create(CLocalhost, MockServer.Port);
    Sleep(50);
    Assert.IsTrue(Connection.Connected);
  finally
    FreeAndNil(Connection);
    FreeAndNil(MockServer);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TServerTcpConnectionTest.TestReportsDisconnectedAfterDisconnect;
var
  MockServer: TMockTcpServer;
  Connection: TLintServerTcpConnection;
begin
  try
    MockServer := TMockTcpServer.Create;
    Connection := TLintServerTcpConnection.Create(CLocalhost, MockServer.Port);
    Sleep(50);
    FreeAndNil(MockServer);
    Sleep(50);
    Assert.IsFalse(Connection.Connected);
  finally
    FreeAndNil(MockServer);
    FreeAndNil(Connection);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TServerTcpConnectionTest);

end.
