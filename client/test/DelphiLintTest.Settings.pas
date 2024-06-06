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
    [Test]
    procedure TestSettingsDir;
    [Test]
    procedure TestServerJarOverride;
    [Test]
    procedure TestJavaExeOverride;
    [Test]
    procedure TestShowConsole;
    [Test]
    procedure TestExternalServer;
    [Test]
    procedure TestAutoShowToolWindow;
    [Test]
    procedure TestSaveBeforeAnalysis;
    [Test]
    procedure TestGetServerJarDefault;
    [Test]
    procedure TestGetJavaExeDefault;
    [Test]
    procedure TestSonarHostTokens;
    [Test]
    procedure TestSonarHostTokensMigrationPath;
    [Test]
    procedure TestDisabledRules;
    [Test]
    procedure TestServerJvmOptions;
    [Test]
    procedure TestDefaultServerJvmOptionsUseSystemProxies;
    [Test]
    procedure TestDefaultServerJvmOptionsUseSystemVm;
    [Test]
    procedure TestSaveAndLoad;
  end;

implementation

uses
    System.IOUtils
  , System.SysUtils
  , System.IniFiles
  , System.StrUtils
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

procedure TSettingsTest.TestServerJvmOptions;
const
  CCategory = 'Server';
  CName = 'JvmOptions';
begin
  SetSetting(CCategory, CName, 'abcdefg');
  FSettings.Load;
  Assert.AreEqual(FSettings.ServerJvmOptions, 'abcdefg');

  FSettings.ServerJvmOptions := 'efgh';
  FSettings.Save;
  Assert.AreEqual('efgh', GetSetting(CCategory, CName));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestDefaultServerJvmOptionsUseSystemProxies;
begin
  Assert.Contains<string>(SplitString(FSettings.ServerJvmOptions, ' '), '-Djava.net.useSystemProxies=true');
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestDefaultServerJvmOptionsUseSystemVm;
begin
  Assert.Contains<string>(SplitString(FSettings.ServerJvmOptions, ' '), '-server');
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

procedure TSettingsTest.TestSonarHostTokens;
const
  CCategory = 'SonarHost';
  CName = 'Tokens';
begin
  Assert.AreEqual(0, FSettings.SonarHostTokensMap.Count);

  SetSetting(CCategory, CName + '_Size', '1');
  SetSetting(
    CCategory,
    CName + '_0',
    'project1@https://sonar.example.com=token1,project2@https://sonar.foo.bar=token2');
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
  Assert.AreEqual('1', GetSetting(CCategory, CName + '_Size'));
  Assert.AreEqual(
    'project1@https://sonar.example.com=token1,' +
      'project2@https://sonar.foo.bar=token2,' +
      'project3@https://foo.sonar.baz=token3',
    GetSetting(CCategory, CName + '_0'));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestSonarHostTokensMigrationPath;
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
  Assert.AreEqual('', GetSetting(CCategory, CName));
  Assert.AreEqual('1', GetSetting(CCategory, CName + '_Size'));
  Assert.AreEqual(
    'project1@https://sonar.example.com=token1,' +
      'project2@https://sonar.foo.bar=token2,' +
      'project3@https://foo.sonar.baz=token3',
    GetSetting(CCategory, CName + '_0'));
end;

//______________________________________________________________________________________________________________________

procedure TSettingsTest.TestDisabledRules;
const
  CCategory = 'Standalone';
  CName = 'DisabledRules';
begin
  Assert.AreEqual(0, FSettings.SonarHostTokensMap.Count);

  SetSetting(CCategory, CName + '_Size', '1');
  SetSetting(
    CCategory,
    CName + '_0',
    'community-delphi:FooBar,community-delphi:BazBar');
  FSettings.Load;
  Assert.AreEqual('community-delphi:FooBar,community-delphi:BazBar', FSettings.StandaloneDisabledRules);

  FSettings.StandaloneDisabledRules := 'community-delphi:FlimpFlarp';
  FSettings.Save;
  Assert.AreEqual('1', GetSetting(CCategory, CName + '_Size'));
  Assert.AreEqual('community-delphi:FlimpFlarp', GetSetting(CCategory, CName + '_0'));
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TSettingsTest);
end.
