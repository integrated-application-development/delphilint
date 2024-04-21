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
unit DelphiLintTest.LiveData;

interface

uses
    DUnitX.TestFramework
  ;

type
  [TestFixture]
  TLiveIssueTest = class(TObject)
  public
    [Test]
    procedure TestIssueWithNoRangeIsSetToEntireFirstLine;
    [Test]
    procedure TestWrongNumberOfAssociatedLinesRaisesRangeError;
    [Test]
    procedure TestTransfersMetadataWhenHasMetadata;
    [Test]
    procedure TestTransfersMetadataWhenNotHasMetadata;
    [Test]
    procedure TestUpdateTetherOnUnchangedLineStaysTethered;
    [Test]
    procedure TestUpdateTetherOnChangedLineIsUntethered;
    [Test]
    procedure TestUpdateTetherOnChangedRangeIsUntethered;
    [Test]
    procedure TestUpdateTetherOnUnchangedLineStaysTetheredMultiline;
    [Test]
    procedure TestUpdateTetherOnChangedLineIsUntetheredMultiline;
    [Test]
    procedure TestUpdateTetherOnChangedRangeIsUntetheredMultiline;
    [Test]
    procedure TestCannotRetether;
    [Test]
    procedure TestUpdateTetherOnChangedWhitespaceIsUntethered;
    [Test('Down', '3')]
    [Test('Up', '-5')]
    [Test('Zero', '0')]
    procedure TestMoveLine(Delta: Integer);
    [Test]
    procedure TestNewLineMoveSession;
  end;

implementation

uses
    System.SysUtils
  , DelphiLint.LiveData
  , DelphiLint.Data
  ;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestTransfersMetadataWhenHasMetadata;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create(
    'rk1',
    'msg',
    'abc.pas',
    nil,
    TIssueMetadata.Create(
      'myassignee',
      issReopened,
      'creationdate'
    ));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abc'], True);
    Assert.IsTrue(LiveIssue.HasMetadata);
    Assert.AreEqual(LiveIssue.Assignee, 'myassignee');
    Assert.AreEqual(LiveIssue.Status, issReopened);
    Assert.AreEqual(LiveIssue.CreationDate, 'creationdate');
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestTransfersMetadataWhenNotHasMetadata;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create(
    'rk1',
    'msg',
    'abc.pas',
    nil,
    TIssueMetadata.Create(
      'myassignee',
      issReopened,
      'creationdate'
    ));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abc'], False);
    Assert.IsFalse(LiveIssue.HasMetadata);
    Assert.AreEqual(LiveIssue.Assignee, 'myassignee');
    Assert.AreEqual(LiveIssue.Status, issReopened);
    Assert.AreEqual(LiveIssue.CreationDate, 'creationdate');
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestCannotRetether;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQr__u');
    Assert.IsFalse(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQrstu');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedLineIsUntethered;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQr__u');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedLineIsUntetheredMultiline;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssueImpl.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(7, 'QRst_____');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedRangeIsUntethered;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGH_KLMNOPQrstu');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedRangeIsUntetheredMultiline;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssueImpl.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6, 'I  LM__P');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnChangedWhitespaceIsUntethered;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abcd  GHJ LMN PQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcd    GHJ  LMNPQrstu');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnUnchangedLineStaysTethered;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 5, 16));
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abcdEFGHJKLMNOPQrstu']);
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(5, 'abcdEFGHJKLMNOPQrstu');
    Assert.IsTrue(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestUpdateTetherOnUnchangedLineStaysTetheredMultiline;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssueImpl.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6, 'IJKLMNOP');
    Assert.IsTrue(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestWrongNumberOfAssociatedLinesRaisesRangeError;
var
  IssueData: TLintIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(4, 0, 4, 26));
  try
    Assert.WillRaise(
      procedure begin
        FreeAndNil(TLiveIssueImpl.Create(IssueData, ['abc', 'def'], False));
      end
    );
  finally
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestIssueWithNoRangeIsSetToEntireFirstLine;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', nil);
  try
    LiveIssue := TLiveIssueImpl.Create(IssueData, ['abc'], False);
    Assert.AreEqual(LiveIssue.StartLine, 1);
    Assert.AreEqual(LiveIssue.EndLine, 1);
    Assert.AreEqual(LiveIssue.StartLineOffset, 0);
    Assert.AreEqual(LiveIssue.EndLineOffset, 0);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestMoveLine(Delta: Integer);
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssueImpl.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );

    Assert.AreEqual(LiveIssue.StartLine, 5);
    Assert.AreEqual(LiveIssue.EndLine, 7);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 5);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 7);
    LiveIssue.LinesMoved := Delta;
    Assert.AreEqual(LiveIssue.StartLine, 5 + Delta);
    Assert.AreEqual(LiveIssue.EndLine, 7 + Delta);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 5);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 7);

    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6 + Delta, 'IJKLMNOP');
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(6 + Delta, 'abc');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLiveIssueTest.TestNewLineMoveSession;
var
  IssueData: TLintIssue;
  LiveIssue: ILiveIssue;
begin
  IssueData := TLintIssue.Create('rk1', 'msg', 'abc.pas', TRange.Create(5, 4, 7, 2));
  try
    LiveIssue := TLiveIssueImpl.Create(
      IssueData,
      [
        'abcdEFGH',
        'IJKLMNOP',
        'QRstuvxy'
      ]
    );

    Assert.AreEqual(LiveIssue.StartLine, 5);
    Assert.AreEqual(LiveIssue.EndLine, 7);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 5);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 7);
    LiveIssue.LinesMoved := 5;
    LiveIssue.NewLineMoveSession;
    Assert.AreEqual(LiveIssue.StartLine, 10);
    Assert.AreEqual(LiveIssue.EndLine, 12);
    Assert.AreEqual(LiveIssue.OriginalStartLine, 10);
    Assert.AreEqual(LiveIssue.OriginalEndLine, 12);

    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(11, 'IJKLMNOP');
    Assert.IsTrue(LiveIssue.Tethered);
    LiveIssue.UpdateTether(11, 'abc');
    Assert.IsFalse(LiveIssue.Tethered);
  finally
    FreeAndNil(LiveIssue);
    FreeAndNil(IssueData);
  end;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TLiveIssueTest);

end.
