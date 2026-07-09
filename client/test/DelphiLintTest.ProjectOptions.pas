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
unit DelphiLintTest.ProjectOptions;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.ProjectOptions
  ;

type
  [TestFixture]
  TProjectOptionsTest = class(TObject)
  private
    FTempDir: string;

    function CreateProjectOptions(ProjectKey: string; ReadProperties: Boolean): TLintProjectOptions;
    procedure WritePropertiesFile(Lines: TArray<string>);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEffectiveProjectKeyUsesOptionsKeyWhenSet;
    [Test]
    procedure TestEffectiveProjectKeyFallsBackToPropertiesFile;
    [Test]
    procedure TestEffectiveProjectKeyEmptyWhenNoPropertiesFile;
    [Test]
    procedure TestEffectiveProjectKeyEmptyWhenReadPropertiesDisabled;
    [Test]
    procedure TestEffectiveProjectKeyEmptyWhenNoKeyInPropertiesFile;
    [Test]
    procedure TestPropertiesFileParsingIgnoresCommentsAndWhitespace;
  end;

implementation

uses
    System.SysUtils
  , System.IOUtils
  ;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.Setup;
begin
  FTempDir := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName);
  TDirectory.CreateDirectory(FTempDir);
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TearDown;
begin
  TDirectory.Delete(FTempDir, True);
end;

//______________________________________________________________________________________________________________________

function TProjectOptionsTest.CreateProjectOptions(ProjectKey: string; ReadProperties: Boolean): TLintProjectOptions;
begin
  Result := TLintProjectOptions.Create(TPath.Combine(FTempDir, 'myproject.delphilint'));
  Result.SonarHostProjectKey := ProjectKey;
  Result.AnalysisReadProperties := ReadProperties;
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.WritePropertiesFile(Lines: TArray<string>);
begin
  TFile.WriteAllLines(TPath.Combine(FTempDir, 'sonar-project.properties'), Lines);
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TestEffectiveProjectKeyUsesOptionsKeyWhenSet;
var
  ProjectOptions: TLintProjectOptions;
begin
  WritePropertiesFile(['sonar.projectKey=properties-key']);

  ProjectOptions := CreateProjectOptions('options-key', True);
  try
    Assert.AreEqual('options-key', ProjectOptions.EffectiveSonarHostProjectKey);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TestEffectiveProjectKeyFallsBackToPropertiesFile;
var
  ProjectOptions: TLintProjectOptions;
begin
  WritePropertiesFile(['sonar.projectKey=properties-key']);

  ProjectOptions := CreateProjectOptions('', True);
  try
    Assert.AreEqual('properties-key', ProjectOptions.EffectiveSonarHostProjectKey);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TestEffectiveProjectKeyEmptyWhenNoPropertiesFile;
var
  ProjectOptions: TLintProjectOptions;
begin
  ProjectOptions := CreateProjectOptions('', True);
  try
    Assert.AreEqual('', ProjectOptions.EffectiveSonarHostProjectKey);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TestEffectiveProjectKeyEmptyWhenReadPropertiesDisabled;
var
  ProjectOptions: TLintProjectOptions;
begin
  WritePropertiesFile(['sonar.projectKey=properties-key']);

  ProjectOptions := CreateProjectOptions('', False);
  try
    Assert.AreEqual('', ProjectOptions.EffectiveSonarHostProjectKey);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TestEffectiveProjectKeyEmptyWhenNoKeyInPropertiesFile;
var
  ProjectOptions: TLintProjectOptions;
begin
  WritePropertiesFile(['sonar.sources=src', 'sonar.projectKeyOther=abc']);

  ProjectOptions := CreateProjectOptions('', True);
  try
    Assert.AreEqual('', ProjectOptions.EffectiveSonarHostProjectKey);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TProjectOptionsTest.TestPropertiesFileParsingIgnoresCommentsAndWhitespace;
var
  ProjectOptions: TLintProjectOptions;
begin
  WritePropertiesFile([
    '# sonar.projectKey=commented-key',
    '! sonar.projectKey=also-commented-key',
    '',
    'sonar.sources=src',
    '  sonar.projectKey  =  spaced-key  '
  ]);

  ProjectOptions := CreateProjectOptions('', True);
  try
    Assert.AreEqual('spaced-key', ProjectOptions.EffectiveSonarHostProjectKey);
  finally
    FreeAndNil(ProjectOptions);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TProjectOptionsTest);

end.
