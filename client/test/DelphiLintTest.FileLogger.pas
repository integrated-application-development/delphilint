unit DelphiLintTest.FileLogger;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.Context
  ;

type
  [TestFixture]
  TFileLoggerTest = class(TObject)
  private const
    CTimeStrRegex = '\d{2}:\d{2}:\d{2}\.\d{3}';
  private
    FLogPath: string;
    function BuildLogger: ILogger;
    function GetLog: TArray<string>;
  public
    [TearDown]
    procedure TearDown;
    [TestCase]
    procedure TestLogDebug;
    [TestCase]
    procedure TestLogInfo;
    [TestCase]
    procedure TestLogWarn;
    [TestCase]
    procedure TestLogDebugWithArgs;
    [TestCase]
    procedure TestLogInfoWithArgs;
    [TestCase]
    procedure TestLogWarnWithArgs;
    [TestCase]
    procedure TestLogMultiple;
  end;

implementation

uses
    System.IOUtils
  , System.SysUtils
  , DelphiLint.FileLogger
  ;

//______________________________________________________________________________________________________________________

function TFileLoggerTest.BuildLogger: ILogger;
begin
  if FLogPath <> '' then begin
    TFile.Delete(FLogPath);
  end;

  FLogPath := TPath.GetTempFileName;
  Result := TFileLogger.Create(FLogPath);
end;

//______________________________________________________________________________________________________________________

function TFileLoggerTest.GetLog: TArray<string>;
begin
  Result := TFile.ReadAllLines(FLogPath);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TearDown;
begin
  if FLogPath <> '' then begin
    TFile.Delete(FLogPath);
    FLogPath := '';
  end;
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogDebug;
const
  CMsg = 'This is my debug message';
var
  LogLines: TArray<string>;
begin
  BuildLogger.Debug(CMsg);

  LogLines := GetLog;
  Assert.AreEqual(1, Length(LogLines));
  Assert.IsMatch(CTimeStrRegex + ' \[DBUG\] ' + CMsg, LogLines[0]);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogDebugWithArgs;
const
  CFormatMsg = 'This is my %s message (%d)';
  CExpectedMsgRegex = 'This is my debug message \(123\)';
var
  LogLines: TArray<string>;
begin
  BuildLogger.Debug(CFormatMsg, ['debug', 123]);

  LogLines := GetLog;
  Assert.AreEqual(Length(LogLines), 1);
  Assert.IsMatch(CTimeStrRegex + ' \[DBUG\] ' + CExpectedMsgRegex, LogLines[0]);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogInfo;
const
  CMsg = 'This is my info message';
var
  LogLines: TArray<string>;
begin
  BuildLogger.Info(CMsg);

  LogLines := GetLog;
  Assert.AreEqual(Length(LogLines), 1);
  Assert.IsMatch(CTimeStrRegex + ' \[INFO\] ' + CMsg, LogLines[0]);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogInfoWithArgs;
const
  CFormatMsg = 'This is my %s message (%d)';
  CExpectedMsgRegex = 'This is my info message \(123\)';
var
  LogLines: TArray<string>;
begin
  BuildLogger.Info(CFormatMsg, ['info', 123]);

  LogLines := GetLog;
  Assert.AreEqual(Length(LogLines), 1);
  Assert.IsMatch(CTimeStrRegex + ' \[INFO\] ' + CExpectedMsgRegex, LogLines[0]);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogWarn;
const
  CMsg = 'This is my warning message';
var
  LogLines: TArray<string>;
begin
  BuildLogger.Warn(CMsg);

  LogLines := GetLog;
  Assert.AreEqual(Length(LogLines), 1);
  Assert.IsMatch(CTimeStrRegex + ' \[WARN\] ' + CMsg, LogLines[0]);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogWarnWithArgs;
const
  CFormatMsg = 'This is my %s message (%d)';
  CExpectedMsgRegex = 'This is my warning message \(123\)';
var
  LogLines: TArray<string>;
begin
  BuildLogger.Warn(CFormatMsg, ['warning', 123]);

  LogLines := GetLog;
  Assert.AreEqual(Length(LogLines), 1);
  Assert.IsMatch(CTimeStrRegex + ' \[WARN\] ' + CExpectedMsgRegex, LogLines[0]);
end;

//______________________________________________________________________________________________________________________

procedure TFileLoggerTest.TestLogMultiple;
var
  Logger: ILogger;
  LogLines: TArray<string>;
  I: Integer;
begin
  Logger := BuildLogger;
  Logger.Debug('Message 1');
  Logger.Debug('Message 2');
  Logger.Info('Message 3');
  Logger.Info('Message 4');
  Logger.Warn('Message 5');
  Logger.Warn('Message 6');

  LogLines := GetLog;
  for I := 0 to Length(LogLines) - 1 do begin
    Assert.IsMatch(CTimeStrRegex + ' \[(DBUG|INFO|WARN)\] ' + 'Message ' + IntToStr(I + 1), LogLines[I]);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TFileLoggerTest);

end.
