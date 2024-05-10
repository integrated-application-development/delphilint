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
unit DelphiLintTest.IssueActions;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.LiveData
  ;

type
  [TestFixture]
  TQuickFixFlowUtilsTest = class(TObject)
  private
    function BuildTextEdit(
      Replacement: string;
      StartLine: Integer;
      StartCol: Integer;
      EndLine: Integer;
      EndCol: Integer;
      TrueLine: Integer = 0
    ): TLiveTextEdit;
  public
    [Test]
    procedure TestCalculateReflowMetrics;
    [Test]
    procedure TestPreviousEditsShouldNotBeReflowed;
    [Test]
    procedure TestSingleLineEditReflow;
    [Test]
    procedure TestMultilineEditReflow;
    [Test]
    procedure TestRemoveLinesEditReflow;
    [Test]
    procedure TestNeighboringEditReflow;
  end;

implementation

uses
    DelphiLint.IssueActions
  , DelphiLint.Data
  , System.SysUtils
  , System.Generics.Collections
  ;

//______________________________________________________________________________________________________________________

function TQuickFixFlowUtilsTest.BuildTextEdit(
  Replacement: string;
  StartLine: Integer;
  StartCol: Integer;
  EndLine: Integer;
  EndCol: Integer;
  TrueLine: Integer = 0
): TLiveTextEdit;
var
  Data: TQuickFixTextEdit;
begin
  Data := TQuickFixTextEdit.Create(Replacement, TRange.Create(StartLine, StartCol, EndLine, EndCol));
  try
    Result := TLiveTextEdit.Create(Data, TrueLine);
  finally
    FreeAndNil(Data);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixFlowUtilsTest.TestCalculateReflowMetrics;
var
  TextEdit: TLiveTextEdit;
  AddedLines: Integer;
  AddedCols: Integer;
begin
  TextEdit := BuildTextEdit('abcd', 7, 6, 7, 8);
  try
    CalculateQuickFixReflowMetrics(TextEdit, AddedLines, AddedCols);
    Assert.AreEqual(0, AddedLines);
    Assert.AreEqual(2, AddedCols);
  finally
    FreeAndNil(TextEdit);
  end;

  TextEdit := BuildTextEdit('ab'#13#10'defg', 7, 6, 7, 8);
  try
    CalculateQuickFixReflowMetrics(TextEdit, AddedLines, AddedCols);
    Assert.AreEqual(1, AddedLines);
    Assert.AreEqual(-4, AddedCols);
  finally
    FreeAndNil(TextEdit);
  end;

  TextEdit := BuildTextEdit('ab'#13#10'defg', 7, 6, 9, 8);
  try
    CalculateQuickFixReflowMetrics(TextEdit, AddedLines, AddedCols);
    Assert.AreEqual(-1, AddedLines);
    Assert.AreEqual(-4, AddedCols);
  finally
    FreeAndNil(TextEdit);
  end;

  TextEdit := BuildTextEdit('', 7, 6, 9, 8);
  try
    CalculateQuickFixReflowMetrics(TextEdit, AddedLines, AddedCols);
    Assert.AreEqual(-2, AddedLines);
    Assert.AreEqual(-2, AddedCols);
  finally
    FreeAndNil(TextEdit);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixFlowUtilsTest.TestMultilineEditReflow;
var
  TextEdits: TObjectList<TLiveTextEdit>;
begin
  TextEdits := TObjectList<TLiveTextEdit>.Create;
  try
    TextEdits.Add(BuildTextEdit('foo'#13#10'barr'#13#10'bazzz', 0, 8, 0, 12));
    TextEdits.Add(BuildTextEdit('bar', 0, 14, 0, 18));
    TextEdits.Add(BuildTextEdit('baz', 0, 20, 2, 4));
    TextEdits.Add(BuildTextEdit('flarp', -1, 0, 1, 9));
    TextEdits.Add(BuildTextEdit('floop', -1, 0, 0, 15));

    ReflowQuickFixEdits(TextEdits, 0);

    Assert.AreEqual(2, TextEdits[1].RelativeStartLine);
    Assert.AreEqual(5 + (14 - 12), TextEdits[1].StartLineOffset);
    Assert.AreEqual(2, TextEdits[1].RelativeEndLine);
    Assert.AreEqual(5 + (18 - 12), TextEdits[1].EndLineOffset);

    Assert.AreEqual(2, TextEdits[2].RelativeStartLine);
    Assert.AreEqual(5 + (20 - 12), TextEdits[2].StartLineOffset);
    Assert.AreEqual(4, TextEdits[2].RelativeEndLine);
    Assert.AreEqual(4, TextEdits[2].EndLineOffset);

    Assert.AreEqual(-1, TextEdits[3].RelativeStartLine);
    Assert.AreEqual(0, TextEdits[3].StartLineOffset);
    Assert.AreEqual(3, TextEdits[3].RelativeEndLine);
    Assert.AreEqual(9, TextEdits[3].EndLineOffset);

    Assert.AreEqual(-1, TextEdits[4].RelativeStartLine);
    Assert.AreEqual(0, TextEdits[4].StartLineOffset);
    Assert.AreEqual(2, TextEdits[4].RelativeEndLine);
    Assert.AreEqual(5 + (15 - 12), TextEdits[4].EndLineOffset);
  finally
    FreeAndNil(TextEdits);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixFlowUtilsTest.TestNeighboringEditReflow;
var
  TextEdits: TObjectList<TLiveTextEdit>;
begin
  TextEdits := TObjectList<TLiveTextEdit>.Create;
  try
    TextEdits.Add(BuildTextEdit('foo', 0, 7, 0, 8));
    TextEdits.Add(BuildTextEdit('bar', 0, 8, 0, 12));
    TextEdits.Add(BuildTextEdit('baz', 0, 7, 0, 7));
    TextEdits.Add(BuildTextEdit('flarp', 0, 4, 0, 7));

    ReflowQuickFixEdits(TextEdits, 0);

    // Edits at the applied's EndLineOffset should be reflowed
    Assert.AreEqual(0, TextEdits[1].RelativeStartLine);
    Assert.AreEqual(10, TextEdits[1].StartLineOffset);
    Assert.AreEqual(0, TextEdits[1].RelativeEndLine);
    Assert.AreEqual(14, TextEdits[1].EndLineOffset);

    // Edits at the applied's StartLineOffset should not be reflowed
    Assert.AreEqual(0, TextEdits[2].RelativeStartLine);
    Assert.AreEqual(7, TextEdits[2].StartLineOffset);
    Assert.AreEqual(0, TextEdits[2].RelativeEndLine);
    Assert.AreEqual(7, TextEdits[2].EndLineOffset);

    // Edits ending at the applied's StartLineOffset should not be reflowed
    Assert.AreEqual(0, TextEdits[3].RelativeStartLine);
    Assert.AreEqual(4, TextEdits[3].StartLineOffset);
    Assert.AreEqual(0, TextEdits[3].RelativeEndLine);
    Assert.AreEqual(7, TextEdits[3].EndLineOffset);
  finally
    FreeAndNil(TextEdits);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixFlowUtilsTest.TestPreviousEditsShouldNotBeReflowed;
var
  TextEdits: TObjectList<TLiveTextEdit>;
begin
  TextEdits := TObjectList<TLiveTextEdit>.Create;
  try
    TextEdits.Add(BuildTextEdit('foo', 0, 7, 1, 5));
    TextEdits.Add(BuildTextEdit('bar', 0, 2, 0, 4));

    ReflowQuickFixEdits(TextEdits, 1);

    Assert.AreEqual(0, TextEdits[0].RelativeStartLine);
    Assert.AreEqual(7, TextEdits[0].StartLineOffset);
    Assert.AreEqual(1, TextEdits[0].RelativeEndLine);
    Assert.AreEqual(5, TextEdits[0].EndLineOffset);
  finally
    FreeAndNil(TextEdits);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixFlowUtilsTest.TestRemoveLinesEditReflow;
var
  TextEdits: TObjectList<TLiveTextEdit>;
begin
  TextEdits := TObjectList<TLiveTextEdit>.Create;
  try
    TextEdits.Add(BuildTextEdit('foo'#13#10'barr'#13#10'bazzz', 0, 8, 5, 12));
    TextEdits.Add(BuildTextEdit('bar', 5, 14, 5, 18));
    TextEdits.Add(BuildTextEdit('baz', 5, 20, 7, 4));
    TextEdits.Add(BuildTextEdit('flarp', -1, 0, 6, 9));
    TextEdits.Add(BuildTextEdit('floop', -1, 0, 5, 15));

    ReflowQuickFixEdits(TextEdits, 0);

    Assert.AreEqual(2, TextEdits[1].RelativeStartLine);
    Assert.AreEqual(5 + (14 - 12), TextEdits[1].StartLineOffset);
    Assert.AreEqual(2, TextEdits[1].RelativeEndLine);
    Assert.AreEqual(5 + (18 - 12), TextEdits[1].EndLineOffset);

    Assert.AreEqual(2, TextEdits[2].RelativeStartLine);
    Assert.AreEqual(5 + (20 - 12), TextEdits[2].StartLineOffset);
    Assert.AreEqual(4, TextEdits[2].RelativeEndLine);
    Assert.AreEqual(4, TextEdits[2].EndLineOffset);

    Assert.AreEqual(-1, TextEdits[3].RelativeStartLine);
    Assert.AreEqual(0, TextEdits[3].StartLineOffset);
    Assert.AreEqual(3, TextEdits[3].RelativeEndLine);
    Assert.AreEqual(9, TextEdits[3].EndLineOffset);

    Assert.AreEqual(-1, TextEdits[4].RelativeStartLine);
    Assert.AreEqual(0, TextEdits[4].StartLineOffset);
    Assert.AreEqual(2, TextEdits[4].RelativeEndLine);
    Assert.AreEqual(5 + (15 - 12), TextEdits[4].EndLineOffset);
  finally
    FreeAndNil(TextEdits);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TQuickFixFlowUtilsTest.TestSingleLineEditReflow;
var
  TextEdits: TObjectList<TLiveTextEdit>;
begin
  TextEdits := TObjectList<TLiveTextEdit>.Create;
  try
    TextEdits.Add(BuildTextEdit('foo', 0, 7, 0, 8));
    TextEdits.Add(BuildTextEdit('bar', 0, 9, 0, 12));
    TextEdits.Add(BuildTextEdit('baz', 0, 15, 2, 4));
    TextEdits.Add(BuildTextEdit('flarp', -1, 0, 1, 9));
    TextEdits.Add(BuildTextEdit('floop', -1, 0, 0, 9));

    ReflowQuickFixEdits(TextEdits, 0);

    Assert.AreEqual(0, TextEdits[1].RelativeStartLine);
    Assert.AreEqual(11, TextEdits[1].StartLineOffset);
    Assert.AreEqual(0, TextEdits[1].RelativeEndLine);
    Assert.AreEqual(14, TextEdits[1].EndLineOffset);

    Assert.AreEqual(0, TextEdits[2].RelativeStartLine);
    Assert.AreEqual(17, TextEdits[2].StartLineOffset);
    Assert.AreEqual(2, TextEdits[2].RelativeEndLine);
    Assert.AreEqual(4, TextEdits[2].EndLineOffset);

    Assert.AreEqual(-1, TextEdits[3].RelativeStartLine);
    Assert.AreEqual(0, TextEdits[3].StartLineOffset);
    Assert.AreEqual(1, TextEdits[3].RelativeEndLine);
    Assert.AreEqual(9, TextEdits[3].EndLineOffset);

    Assert.AreEqual(-1, TextEdits[4].RelativeStartLine);
    Assert.AreEqual(0, TextEdits[4].StartLineOffset);
    Assert.AreEqual(0, TextEdits[4].RelativeEndLine);
    Assert.AreEqual(11, TextEdits[4].EndLineOffset);
  finally
    FreeAndNil(TextEdits);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TQuickFixFlowUtilsTest);

end.
