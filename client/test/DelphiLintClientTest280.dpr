program DelphiLintClientTest280;

{$R *.res}

{$IFDEF TESTGUI}
{$APPTYPE GUI}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  {$IFDEF TESTGUI}
  DUnitX.Loggers.GUI.VCL,
  {$ELSE}
  DUnitX.Loggers.Console,
  {$ENDIF }
  {$ENDIF }
  DUnitX.Loggers.XML.NUnit,
  DUnitX.TestFramework,
  DelphiLintTest.Events in 'DelphiLintTest.Events.pas',
  DelphiLintTest.Data in 'DelphiLintTest.Data.pas',
  DelphiLintTest.MockUtils in 'DelphiLintTest.MockUtils.pas',
  DelphiLintTest.Handlers in 'DelphiLintTest.Handlers.pas',
  DelphiLintTest.MockContext in 'DelphiLintTest.MockContext.pas',
  DelphiLintTest.Plugin in 'DelphiLintTest.Plugin.pas',
  DelphiLintTest.Utils in 'DelphiLintTest.Utils.pas',
  DelphiLintTest.Server in 'DelphiLintTest.Server.pas',
  DelphiLintTest.FileLogger in 'DelphiLintTest.FileLogger.pas',
  DelphiLintTest.HtmlGen in 'DelphiLintTest.HtmlGen.pas',
  DelphiLintTest.Settings in 'DelphiLintTest.Settings.pas',
  DelphiLintTest.LiveData in 'DelphiLintTest.LiveData.pas',
  DelphiLintTest.IssueActions in 'DelphiLintTest.IssueActions.pas',
  DelphiLintTest.Properties in 'DelphiLintTest.Properties.pas';

{$R *Additional.res}

{$IFDEF TESTINSIGHT}
begin
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
{$IFDEF TESTGUI}
begin
  DUnitX.Loggers.GUI.VCL.Run;
{$ELSE}
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger : ITestLogger;
begin
  //Check command line options, will exit if invalid
  TDUnitX.CheckCommandLine;
  try
    //Create the test runner
    Runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    Runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    Runner.FailsOnNoAsserts := True;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      Logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      Runner.AddLogger(Logger);
    end;
    //Generate an NUnit compatible XML File
    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);

    //Run tests
    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
{$ENDIF}
end.
