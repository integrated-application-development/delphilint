unit DelphiLintTest.Settings;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.Settings
  ;

type
  [TestFixture]
  TSettingsTest = class(TObject)
  private
    FTempSettingsPath: string;
    FSettings: TLintSettings;

    procedure SetSetting(Category: string; Name: string; Value: string);
    function GetSetting(Category: string; Name: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [TestCase]
    procedure TestSettingsDir;
    [TestCase]
    procedure TestServerJarOverride;
    [TestCase]
    procedure TestSonarDelphiJarOverride;
    [TestCase]
    procedure TestJavaExeOverride;
    [TestCase]
    procedure TestShowConsole;
    [TestCase]
    procedure TestExternalServer;
    [TestCase]
    procedure TestAutoShowToolWindow;
    [TestCase]
    procedure TestSaveBeforeAnalysis;
    [TestCase]
    procedure TestGetServerJarDefault;
    [TestCase]
    procedure TestGetSonarDelphiJarDefault;
    [TestCase]
    procedure TestGetJavaExeDefault;
    [TestCase]
    procedure TestSonarHostTokens;
    [TestCase]
    procedure TestSaveAndLoad;
  end;

implementation

uses
    System.IOUtils
  , System.SysUtils
  , System.IniFiles
  , Winapi.Windows
  , DelphiLint.Version
  ;

//______________________________________________________________________________________________________________________

function TSettingsTest.GetSetting(Category, Name: string): string;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FTempSettingsPath);
  try
    Result := IniFile.ReadString(Category, Name, '');
  finally
    FreeAndNil(IniFile);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.SetSetting(Category, Name, Value: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FTempSettingsPath);
  try
    IniFile.WriteString(Category, Name, Value);
  finally
    FreeAndNil(IniFile);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.Setup;
begin
  FTempSettingsPath := TPath.GetTempFileName;
  FSettings := TLintSettings.Create(FTempSettingsPath);
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TearDown;
begin
  TFile.Delete(FTempSettingsPath);
  FreeAndNil(FSettings);
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestAutoShowToolWindow;
const
  CCategory = 'Client';
  CName = 'AutoShowToolWindow';
begin
  Assert.AreEqual(FSettings.ClientAutoShowToolWindow, True);
  SetSetting(CCategory, CName, '0');
  FSettings.Load;
  Assert.AreEqual(FSettings.ClientAutoShowToolWindow, False);

  FSettings.ClientAutoShowToolWindow := True;
  FSettings.Save;
  Assert.AreEqual('1', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestExternalServer;
const
  CCategory = 'Debug';
  CName = 'ExternalServer';
begin
  Assert.AreEqual(FSettings.DebugExternalServer, False);

  SetSetting(CCategory, CName, '1');
  FSettings.Load;
  Assert.AreEqual(FSettings.DebugExternalServer, True);

  FSettings.DebugExternalServer := False;
  FSettings.Save;
  Assert.AreEqual('0', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestGetJavaExeDefault;
const
  CJavaHome = 'JAVA_HOME';
var
  JavaHome: string;
begin
  JavaHome := GetEnvironmentVariable(CJavaHome);
  SetEnvironmentVariable(PChar(CJavaHome), 'java_home_val');
  try
    Assert.AreEqual('java_home_val\bin\java.exe', FSettings.JavaExe);

    FSettings.ServerJavaExeOverride := 'abcd';
    Assert.AreEqual('abcd', FSettings.JavaExe);
  finally
    SetEnvironmentVariable(PChar(CJavaHome), PChar(JavaHome));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestGetServerJarDefault;
begin
  Assert.AreEqual(
    Format('%s\delphilint-server-%s.jar', [FSettings.SettingsDirectory, DelphiLintVersion]),
    FSettings.ServerJar);

  FSettings.ServerJarOverride := 'abcd';
  Assert.AreEqual('abcd', FSettings.ServerJar);
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestGetSonarDelphiJarDefault;
begin
  Assert.AreEqual(
    TPath.Combine(FSettings.SettingsDirectory, 'sonar-delphi-plugin.jar'),
    FSettings.SonarDelphiJar);

  FSettings.ServerSonarDelphiJarOverride := 'abcd';
  Assert.AreEqual('abcd', FSettings.SonarDelphiJar);
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestJavaExeOverride;
const
  CCategory = 'Resources';
  CName = 'JavaExeOverride';
begin
  Assert.AreEqual(FSettings.ServerJavaExeOverride, '');

  SetSetting(CCategory, CName, 'abcdefg');
  FSettings.Load;
  Assert.AreEqual(FSettings.ServerJavaExeOverride, 'abcdefg');

  FSettings.ServerJavaExeOverride := 'efgh';
  FSettings.Save;
  Assert.AreEqual('efgh', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestSaveAndLoad;
const
  CCategory = 'Client';
  CName = 'AutoShowToolWindow';
begin
  SetSetting(CCategory, CName, '0');
  Assert.AreEqual(FSettings.ClientAutoShowToolWindow, True);
  FSettings.Load;
  Assert.AreEqual(FSettings.ClientAutoShowToolWindow, False);

  SetSetting(CCategory, CName, '1');
  Assert.AreEqual(FSettings.ClientAutoShowToolWindow, False);
  FSettings.Load;
  Assert.AreEqual(FSettings.ClientAutoShowToolWindow, True);

  FSettings.ClientAutoShowToolWindow := False;
  Assert.AreEqual(GetSetting(CCategory, CName), '1');
  FSettings.Save;
  Assert.AreEqual(GetSetting(CCategory, CName), '0');
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestSaveBeforeAnalysis;
const
  CCategory = 'Client';
  CName = 'SaveBeforeAnalysis';
begin
  Assert.AreEqual(FSettings.ClientSaveBeforeAnalysis, True);

  SetSetting(CCategory, CName, '0');
  FSettings.Load;
  Assert.AreEqual(FSettings.ClientSaveBeforeAnalysis, False);

  FSettings.ClientSaveBeforeAnalysis := True;
  FSettings.Save;
  Assert.AreEqual('1', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestServerJarOverride;
const
  CCategory = 'Resources';
  CName = 'ServerJarOverride';
begin
  Assert.AreEqual(FSettings.ServerJarOverride, '');

  SetSetting(CCategory, CName, 'abcdefg');
  FSettings.Load;
  Assert.AreEqual(FSettings.ServerJarOverride, 'abcdefg');

  FSettings.ServerJarOverride := 'efgh';
  FSettings.Save;
  Assert.AreEqual('efgh', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestSettingsDir;
begin
  Assert.AreEqual(TPath.GetDirectoryName(FTempSettingsPath), FSettings.SettingsDirectory);
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestShowConsole;
const
  CCategory = 'Debug';
  CName = 'ShowConsole';
begin
  Assert.AreEqual(FSettings.DebugShowConsole, False);

  SetSetting(CCategory, CName, '1');
  FSettings.Load;
  Assert.AreEqual(FSettings.DebugShowConsole, True);

  FSettings.DebugShowConsole := False;
  FSettings.Save;
  Assert.AreEqual('0', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestSonarDelphiJarOverride;
const
  CCategory = 'Resources';
  CName = 'SonarDelphiJarOverride';
begin
  Assert.AreEqual(FSettings.ServerSonarDelphiJarOverride, '');

  SetSetting(CCategory, CName, 'abcdefg');
  FSettings.Load;
  Assert.AreEqual(FSettings.ServerSonarDelphiJarOverride, 'abcdefg');

  FSettings.ServerSonarDelphiJarOverride := 'efgh';
  FSettings.Save;
  Assert.AreEqual('efgh', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestSonarHostTokens;
const
  CCategory = 'SonarHost';
  CName = 'Tokens';
begin
  Assert.AreEqual(0, FSettings.SonarHostTokensMap.Count);

  SetSetting(CCategory, CName, 'project1@https://sonar.example.com=token1,project2@https://sonar.foo.bar=token2');
  FSettings.Load;
  Assert.AreEqual(2, FSettings.SonarHostTokensMap.Count);
  Assert.AreEqual(
    'token1',
    FSettings.SonarHostTokensMap[
      TSonarProjectIdentifier.Create('https://sonar.example.com', 'project1')]);
  Assert.AreEqual(
    'token2',
    FSettings.SonarHostTokensMap[
      TSonarProjectIdentifier.Create('https://sonar.foo.bar', 'project2')]);

  FSettings.SonarHostTokensMap.Add(TSonarProjectIdentifier.Create('https://foo.sonar.baz', 'project3'), 'token3');
  FSettings.Save;
  Assert.AreEqual(
    'project1@https://sonar.example.com=token1,' +
    'project2@https://sonar.foo.bar=token2,' +
    'project3@https://foo.sonar.baz=token3',
    GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TSettingsTest);
end.
