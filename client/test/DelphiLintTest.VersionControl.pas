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
unit DelphiLintTest.VersionControl;

interface

uses
    DUnitX.TestFramework
  ;

type
  [TestFixture]
  TVersionControlTest = class(TObject)
  public
    [Test]
    procedure TestParsesChangedFiles;
    [Test]
    procedure TestExcludesDeletedFiles;
    [Test]
    procedure TestIgnoresMalformedLines;
    [Test]
    procedure TestUnquotesQuotedPaths;
  end;

implementation

uses
    DelphiLint.VersionControl
  ;

//______________________________________________________________________________________________________________________

procedure TVersionControlTest.TestParsesChangedFiles;
var
  Files: TArray<string>;
begin
  Files := ParseGitStatusOutput(
    [
      ' M src/modified.pas',
      'M  src/staged.pas',
      'MM src/both.pas',
      'A  added.pas',
      '?? untracked.pas'
    ],
    'C:\repo');

  Assert.AreEqual(5, Length(Files));
  Assert.AreEqual('C:\repo\src\modified.pas', Files[0]);
  Assert.AreEqual('C:\repo\src\staged.pas', Files[1]);
  Assert.AreEqual('C:\repo\src\both.pas', Files[2]);
  Assert.AreEqual('C:\repo\added.pas', Files[3]);
  Assert.AreEqual('C:\repo\untracked.pas', Files[4]);
end;

//______________________________________________________________________________________________________________________

procedure TVersionControlTest.TestExcludesDeletedFiles;
var
  Files: TArray<string>;
begin
  Files := ParseGitStatusOutput(
    [
      ' D worktreedeleted.pas',
      'D  stagedeleted.pas',
      ' M kept.pas'
    ],
    'C:\repo');

  Assert.AreEqual(1, Length(Files));
  Assert.AreEqual('C:\repo\kept.pas', Files[0]);
end;

//______________________________________________________________________________________________________________________

procedure TVersionControlTest.TestIgnoresMalformedLines;
var
  Files: TArray<string>;
begin
  Files := ParseGitStatusOutput(
    [
      '',
      'M',
      ' M valid.pas'
    ],
    'C:\repo');

  Assert.AreEqual(1, Length(Files));
  Assert.AreEqual('C:\repo\valid.pas', Files[0]);
end;

//______________________________________________________________________________________________________________________

procedure TVersionControlTest.TestUnquotesQuotedPaths;
var
  Files: TArray<string>;
begin
  Files := ParseGitStatusOutput(
    [
      ' M "src/my \"quoted\" file.pas"',
      ' M "src/back\\slash.pas"'
    ],
    'C:\repo');

  Assert.AreEqual(2, Length(Files));
  Assert.AreEqual('C:\repo\src\my "quoted" file.pas', Files[0]);
  Assert.AreEqual('C:\repo\src\back\slash.pas', Files[1]);
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TVersionControlTest);

end.
