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
unit DelphiLintTest.Properties;

interface

uses
    DUnitX.TestFramework
  ;

type
  [TestFixture]
  TPropertiesTest = class(TObject)
  public
    [Test]
    procedure TestLongStringField;
    [Test]
    procedure TestLongStringFieldCleansUpWhenShortened;
    [Test]
    procedure TestEmptyLongStringField;
    [Test]
    procedure TestLongStringFieldMigrationPath;
  end;

implementation

uses
    DelphiLint.Properties
  , System.SysUtils
  , System.IniFiles
  , System.IOUtils
  ;

//______________________________________________________________________________________________________________________

procedure TPropertiesTest.TestEmptyLongStringField;
const
  CSection = 'FooSection';
  CKey = 'BarKey';
  CSizeKey = 'BarKey_Size';
  CDefault = '<DEFAULT>';
var
  Field: TLongStringPropField;
  Ini: TIniFile;
  Path: String;
begin
  Path := TPath.GetTempFileName;
  TFile.WriteAllText(Path, '', TEncoding.ASCII); // Clear file
  Ini := TIniFile.Create(Path);
  Field := TLongStringPropField.Create(CSection, CKey);
  try
    Field.Value := '';
    Field.Save(Ini);
    Assert.IsFalse(Ini.ValueExists(CSection, CKey));
    Assert.AreEqual('1', Ini.ReadString(CSection, CSizeKey, CDefault));
    Assert.AreEqual('', Ini.ReadString(CSection, CKey + '_0', CDefault));
  finally
    FreeAndNil(Field);
    FreeAndNil(Ini);
    TFile.Delete(Path);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPropertiesTest.TestLongStringField;
const
  CSection = 'FooSection';
  CKey = 'BarKey';
  CSizeKey = 'BarKey_Size';
  CDefault = '<DEFAULT>';
var
  Field: TLongStringPropField;
  Ini: TIniFile;
  Path: String;
  Expected: String;
begin
  Path := TPath.GetTempFileName;
  TFile.WriteAllText(Path, '', TEncoding.ASCII); // Clear file
  Ini := TIniFile.Create(Path);
  Field := TLongStringPropField.Create(CSection, CKey);
  try
    Expected := StringOfChar('a', 1000);
    Ini.WriteString(CSection, CSizeKey, '1');
    Ini.WriteString(CSection, CKey + '_0', Expected);
    Field.Load(Ini);
    Assert.AreEqual<String>(Expected, Field.Value);

    Ini.WriteString(CSection, CKey + '_1', StringOfChar('b', 1000));
    Field.Load(Ini);
    Assert.AreEqual<String>(Expected, Field.Value);

    Ini.WriteString(CSection, CSizeKey, '2');
    Expected := Expected + StringOfChar('b', 1000);
    Field.Load(Ini);
    Assert.AreEqual<String>(Expected, Field.Value);

    Field.Value := StringOfChar('c', 6000);
    Field.Save(Ini);
    Expected := StringOfChar('c', 1024);
    Assert.AreEqual('6', Ini.ReadString(CSection, CSizeKey, CDefault));
    Assert.AreEqual(Expected, Ini.ReadString(CSection, CKey + '_0', CDefault));
    Assert.AreEqual(Expected, Ini.ReadString(CSection, CKey + '_1', CDefault));
    Assert.AreEqual(Expected, Ini.ReadString(CSection, CKey + '_2', CDefault));
    Assert.AreEqual(Expected, Ini.ReadString(CSection, CKey + '_3', CDefault));
    Assert.AreEqual(Expected, Ini.ReadString(CSection, CKey + '_4', CDefault));
    Assert.AreEqual(StringOfChar('c', 880), Ini.ReadString(CSection, CKey + '_5', CDefault));
  finally
    FreeAndNil(Field);
    FreeAndNil(Ini);
    TFile.Delete(Path);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPropertiesTest.TestLongStringFieldCleansUpWhenShortened;
const
  CSection = 'FooSection';
  CKey = 'BarKey';
  CSizeKey = 'BarKey_Size';
  CDefault = '<DEFAULT>';
var
  Field: TLongStringPropField;
  Ini: TIniFile;
  Path: String;
begin
  Path := TPath.GetTempFileName;
  TFile.WriteAllText(Path, '', TEncoding.ASCII); // Clear file
  Ini := TIniFile.Create(Path);
  Field := TLongStringPropField.Create(CSection, CKey);
  try
    Field.Value := StringOfChar('c', 6000);
    Field.Save(Ini);
    Assert.AreEqual(Ini.ReadString(CSection, CSizeKey, CDefault), '6');
    Assert.AreEqual(StringOfChar('c', 1024), Ini.ReadString(CSection, CKey + '_3', CDefault));

    Field.Value := StringOfChar('c', 3000);
    Field.Save(Ini);
    Assert.AreEqual(Ini.ReadString(CSection, CSizeKey, CDefault), '3');
    Assert.IsFalse(Ini.ValueExists(CSection, CKey + '_3'));
  finally
    FreeAndNil(Field);
    FreeAndNil(Ini);
    TFile.Delete(Path);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPropertiesTest.TestLongStringFieldMigrationPath;

const
  CSection = 'FooSection';
  CKey = 'BarKey';
  CSizeKey = 'BarKey_Size';
  CDefault = '<DEFAULT>';
var
  Field: TLongStringPropField;
  Ini: TIniFile;
  Path: String;
  Expected: String;
begin
  Path := TPath.GetTempFileName;
  TFile.WriteAllText(Path, '', TEncoding.ASCII); // Clear file
  Ini := TIniFile.Create(Path);
  Field := TLongStringPropField.Create(CSection, CKey);
  try
    Expected := StringOfChar('a', 1000);
    Ini.WriteString(CSection, CKey, Expected);
    Field.Load(Ini);
    Assert.AreEqual<String>(Expected, Field.Value);

    Field.Save(Ini);
    Assert.IsFalse(Ini.ValueExists(CSection, CKey));
    Assert.AreEqual('1', Ini.ReadString(CSection, CSizeKey, CDefault));
    Assert.AreEqual(Expected, Ini.ReadString(CSection, CKey + '_0', CDefault));
  finally
    FreeAndNil(Field);
    FreeAndNil(Ini);
    TFile.Delete(Path);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TPropertiesTest);
end.
