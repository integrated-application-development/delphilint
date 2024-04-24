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
unit DelphiLintTest.Utils;

interface

uses
    DUnitX.TestFramework
  , DelphiLintTest.MockContext
  ;

type
  [TestFixture]
  TUtilsTest = class(TObject)
  private
    procedure MockIDEServices(out IDEServices: TMockIDEServices);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]

    [TestCase('AlreadyAbsolute', 'C:\ABC\def,C:\abc,C:\ABC\def')]
    procedure TestToAbsolutePath(RelativePath: string; BaseDir: string; Expected: string);
    [Test]
    [TestCase('Prenormalized', 'c:/abc/def/ghi.txt,c:/abc/def/ghi.txt')]
    procedure TestNormalizePath(Input: string; Expected: string);
    [Test]
    procedure TestTryGetCurrentSourceEditorReturnsCurrentSourceEditor;
    [Test]
    procedure TestTryGetCurrentSourceEditorReturnsFalseWhenNoModule;
    [Test]
    procedure TestTryGetProjectFileReturnsProjectFile;
    [Test]
    procedure TestTryGetProjectFileReturnsFalseWhenNoProject;
    [Test]
    procedure TestGetOpenSourceModulesGetsOnlyPasFiles;
    [Test]
    procedure TestGetAllFilesReturnsEmptyWhenNotInProject;
    [Test]
    procedure TestGetAllFilesFiltersOutNonDelphiFiles;
    [Test]
    [TestCase('LowerCase', 'abc.pas')]
    procedure TestIsPasFileIsTrueFor(Path: string);
    [Test]
    [TestCase('Dpr', 'abc.dpr')]
    procedure TestIsPasFileIsFalseFor(Path: string);
    [Test]
    [TestCase('Dpr', 'abc.dpr')]
    procedure TestIsMainFileIsTrueFor(Path: string);
    [Test]
    [TestCase('Pas', 'abc.pas')]
    procedure TestIsMainFileIsFalseFor(Path: string);
    [Test]
    [TestCase('Pas', 'abc.pas')]
    procedure TestIsDelphiSourceIsTrueFor(Path: string);
    [Test]
    [TestCase('Dproj', 'abc.dproj')]
    procedure TestIsDelphiSourceIsFalseFor(Path: string);
    [Test]
    [TestCase('Dproj', 'abc.dproj')]
    procedure TestIsProjectFileIsTrueFor(Path: string);
    [Test]
    [TestCase('Pas', 'abc.pas')]
    procedure TestIsProjectFileIsFalseFor(Path: string);
    [Test]
    procedure TestTryGetProjectDirectoryReturnsFalseWhenNoProject;
    [Test]
    procedure TestTryGetProjectDirectoryWithReadOptionsOffGetsProjectFileDirectory;
    [Test]
    procedure TestTryGetProjectDirectoryWithReadOptionsOnGetsAbsoluteAnalysisBaseDir;
  end;

  [TestFixture]
  TArrayUtilsTest = class(TObject)
  public
    [Test]
    procedure TestMapSimpleTypes;
    [Test]
    procedure TestMapDoesNotFree;
    [Test]
    procedure TestReduceSimpleTypes;
    [Test]
    procedure TestReduceDoesNotFree;
    [Test]
    procedure TestMax;
    [Test]
    procedure TestMaxWithDefault;
    [Test]
    procedure TestReduceNoElementsRaisesException;
    [Test]
    procedure TestReduceNoElementsWithDefaultReturnsDefault;
  end;

implementation

uses
    System.Generics.Collections
  , System.SysUtils
  , DelphiLint.Utils
  , DelphiLint.Context
  , DelphiLint.ProjectOptions
  ;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.MockIDEServices(out IDEServices: TMockIDEServices);
begin
  IDEServices := TMockIDEServices.Create;
  MockContext.MockIDEServices(IDEServices);
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.Setup;
begin
  MockContext.Reset;
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TearDown;
begin
  MockContext.Reset;
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestGetAllFilesFiltersOutNonDelphiFiles;
var
  IDEServices: TMockIDEServices;
  Project: TMockProject;
  MockedFiles: TList<string>;
  AllFiles: TArray<string>;
begin
  MockIDEServices(IDEServices);

  Project := TMockProject.Create;
  IDEServices.MockActiveProject(Project);

  MockedFiles := TList<string>.Create;
  Project.MockedFileList := MockedFiles;
  MockedFiles.Add('abc.pas');
  MockedFiles.Add('def.dfm');
  MockedFiles.Add('ghi.dpk');
  MockedFiles.Add('jkl.dpr');
  MockedFiles.Add('mno.dproj');
  MockedFiles.Add('pqr.txt');

  AllFiles := GetAllFiles;

  Assert.AreEqual(4, Length(AllFiles));
  Assert.AreEqual('abc.pas', AllFiles[0]);
  Assert.AreEqual('ghi.dpk', AllFiles[1]);
  Assert.AreEqual('jkl.dpr', AllFiles[2]);
  Assert.AreEqual('mno.dproj', AllFiles[3]);
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestGetAllFilesReturnsEmptyWhenNotInProject;
var
  IDEServices: TMockIDEServices;
  AllFiles: TArray<string>;
begin
  MockIDEServices(IDEServices);

  AllFiles := GetAllFiles;
  Assert.AreEqual(0, Length(AllFiles));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestGetOpenSourceModulesGetsOnlyPasFiles;

  function MockModule(FileName: string): TMockModule;
  begin
    Result := TMockModule.Create;
    Result.MockedFileName := FileName;
  end;

var
  OpenSourceModules: TArray<IIDEModule>;
  IDEServices: TMockIDEServices;
  Modules: TList<IIDEModule>;
begin
  MockIDEServices(IDEServices);

  Modules := TList<IIDEModule>.Create;
  IDEServices.MockModules(Modules);

  Modules.Add(MockModule('abc.pas'));
  Modules.Add(MockModule('def.dpr'));
  Modules.Add(MockModule('ghi.dpk'));
  Modules.Add(MockModule('jkl.pas'));

  OpenSourceModules := GetOpenSourceModules;
  Assert.AreEqual(2, Length(OpenSourceModules));
  Assert.AreEqual('abc.pas', OpenSourceModules[0].FileName);
  Assert.AreEqual('jkl.pas', OpenSourceModules[1].FileName);
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsDelphiSourceIsFalseFor(Path: string);
begin
  Assert.IsFalse(IsDelphiSource(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsDelphiSourceIsTrueFor(Path: string);
begin
  Assert.IsTrue(IsDelphiSource(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsMainFileIsFalseFor(Path: string);
begin
  Assert.IsFalse(IsMainFile(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsMainFileIsTrueFor(Path: string);
begin
  Assert.IsTrue(IsMainFile(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsPasFileIsFalseFor(Path: string);
begin
  Assert.IsFalse(IsPasFile(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsPasFileIsTrueFor(Path: string);
begin
  Assert.IsTrue(IsPasFile(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsProjectFileIsFalseFor(Path: string);
begin
  Assert.IsFalse(IsProjectFile(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestIsProjectFileIsTrueFor(Path: string);
begin
  Assert.IsTrue(IsProjectFile(Path));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestNormalizePath(Input: string; Expected: string);
begin
  Assert.AreEqual(Expected, NormalizePath(Input));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestToAbsolutePath(RelativePath: string; BaseDir: string; Expected: string);
begin
  Assert.AreEqual(Expected, ToAbsolutePath(RelativePath, BaseDir));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetCurrentSourceEditorReturnsCurrentSourceEditor;
var
  IDEServices: TMockIDEServices;
  Editor: IIDESourceEditor;
  Module: TMockModule;
  MockEditor: TMockSourceEditor;
begin
  MockIDEServices(IDEServices);

  Module := TMockModule.Create;
  IDEServices.MockCurrentModule(Module);

  MockEditor := TMockSourceEditor.Create;
  Module.MockedSourceEditor := MockEditor;

  Assert.IsTrue(TryGetCurrentSourceEditor(Editor));
  Assert.AreSame(MockEditor, Editor);
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetCurrentSourceEditorReturnsFalseWhenNoModule;
var
  IDEServices: TMockIDEServices;
  Editor: IIDESourceEditor;
begin
  MockIDEServices(IDEServices);
  Assert.IsFalse(TryGetCurrentSourceEditor(Editor));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetProjectDirectoryReturnsFalseWhenNoProject;
var
  IDEServices: TMockIDEServices;
  ProjectDir: string;
begin
  MockIDEServices(IDEServices);
  Assert.IsFalse(TryGetProjectDirectory(ProjectDir));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetProjectDirectoryWithReadOptionsOffGetsProjectFileDirectory;
var
  IDEServices: TMockIDEServices;
  Project: TMockProject;
  ProjectDir: string;
begin
  MockIDEServices(IDEServices);

  Project := TMockProject.Create;
  IDEServices.MockActiveProject(Project);

  Project.MockedFileList := TList<string>.Create;
  Project.MockedFileList.Add('C:\abc\def\ghi.dproj');

  Assert.IsTrue(TryGetProjectDirectory(ProjectDir, False));
  Assert.AreEqual('C:\abc\def', ProjectDir);
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetProjectDirectoryWithReadOptionsOnGetsAbsoluteAnalysisBaseDir;
const
  CProjectFile = 'C:\abc\def\ghi.dproj';
  CProjectOptionsFile = 'C:\rst\uvw\mno\options.txt';
  CProjectDirRelative = '..\xyz';
  CProjectDir = 'C:\rst\uvw\xyz';
var
  IDEServices: TMockIDEServices;
  Project: TMockProject;
  ProjectDir: string;
  ProjectOptions: TLintProjectOptions;
begin
  MockIDEServices(IDEServices);
  ProjectOptions := TLintProjectOptions.Create(CProjectOptionsFile, True);
  MockContext.MockProjectOptions(CProjectFile, ProjectOptions);
  ProjectOptions.AnalysisBaseDir := CProjectDirRelative;

  Project := TMockProject.Create;
  IDEServices.MockActiveProject(Project);

  Project.MockedFileList := TList<string>.Create;
  Project.MockedFileList.Add(CProjectFile);

  Assert.IsTrue(TryGetProjectDirectory(ProjectDir, True));
  Assert.AreEqual(CProjectDir, ProjectDir);
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetProjectFileReturnsFalseWhenNoProject;
var
  IDEServices: TMockIDEServices;
  ProjectFile: string;
begin
  MockIDEServices(IDEServices);
  Assert.IsFalse(TryGetProjectFile(ProjectFile));
end;

//______________________________________________________________________________________________________________________

procedure TUtilsTest.TestTryGetProjectFileReturnsProjectFile;
var
  IDEServices: TMockIDEServices;
  Project: TMockProject;
  ProjectFile: string;
begin
  MockIDEServices(IDEServices);

  Project := TMockProject.Create;
  IDEServices.MockActiveProject(Project);

  Project.MockedFileList := TList<string>.Create;
  Project.MockedFileList.Add('abc.pas');
  Project.MockedFileList.Add('def.dfm');
  Project.MockedFileList.Add('ghi.dpk');
  Project.MockedFileList.Add('jkl.dpr');
  Project.MockedFileList.Add('mno.dproj');
  Project.MockedFileList.Add('pqr.txt');

  Assert.IsTrue(TryGetProjectFile(ProjectFile));
  Assert.AreEqual('mno.dproj', ProjectFile);
end;

//______________________________________________________________________________________________________________________

type
  TTestObject = class(TObject)
  private
    FOnDestroy: TProc;
  public
    constructor Create(OnDestroy: TProc);
    destructor Destroy; override;
  end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestMapDoesNotFree;
var
  MyObj: TTestObject;
  Freed: Boolean;
begin
  Freed := False;
  MyObj := TTestObject.Create(
    procedure begin
      Freed := True;
    end
  );
  try
    TArrayUtils.Map<TObject, Pointer>(
      [MyObj],
      function(Obj: TObject): Pointer
      begin
        Result := @Obj;
      end);
    Assert.IsFalse(Freed);
  finally
    FreeAndNil(MyObj);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestMapSimpleTypes;
var
  MappedArray: TArray<string>;
begin
  MappedArray := TArrayUtils.Map<Integer, string>([1,3,5,9,11], IntToStr);
  Assert.AreEqual(5, Length(MappedArray));
  Assert.AreEqual('1', MappedArray[0]);
  Assert.AreEqual('3', MappedArray[1]);
  Assert.AreEqual('5', MappedArray[2]);
  Assert.AreEqual('9', MappedArray[3]);
  Assert.AreEqual('11', MappedArray[4]);
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestMax;
begin
  Assert.AreEqual(64, TArrayUtils.Max<Integer>([1,32,4,8,64,16,2]));
  Assert.AreEqual('dza', TArrayUtils.Max<string>(['aaa','bbb','dza','ccc','ddd']));
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestMaxWithDefault;
begin
  Assert.AreEqual(4, TArrayUtils.Max<Integer>([1,2,4,3], 5));
  Assert.AreEqual(5, TArrayUtils.Max<Integer>([], 5));
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestReduceDoesNotFree;
var
  MyObj: TTestObject;
  Freed: Boolean;
begin
  Freed := False;
  MyObj := TTestObject.Create(
    procedure begin
      Freed := True;
    end
  );
  try
    TArrayUtils.Reduce<TObject, Pointer>(
      [MyObj],
      function(Acc: Pointer; Current: TObject): Pointer
      begin
        Result := @Current;
      end);
    Assert.IsFalse(Freed);
  finally
    FreeAndNil(MyObj);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestReduceNoElementsRaisesException;
begin
  Assert.WillRaise(
    procedure begin
      TArrayUtils.Reduce<Integer, Integer>(
        [],
        function (Acc: Integer; Element: Integer): Integer
        begin
          Result := Acc + Element;
        end
      );
    end,
    ERangeError);
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestReduceNoElementsWithDefaultReturnsDefault;
var
  Val: Integer;
begin
  Val := TArrayUtils.Reduce<Integer, Integer>(
    [],
    function (Acc: Integer; Element: Integer): Integer
    begin
      Result := Acc + Element;
    end,
    50);

  Assert.AreEqual(50, Val);
end;

//______________________________________________________________________________________________________________________

procedure TArrayUtilsTest.TestReduceSimpleTypes;
var
  ReducedStr: string;
begin
  ReducedStr := TArrayUtils.Reduce<Integer, string>(
    [1,3,5,9,11],
    function(Acc: string; Element: Integer): string
    begin
      Result := Acc;

      if Result <> '' then begin
        Result := Result + ',';
      end;

      Result := Result + IntToStr(Element);
    end);

  Assert.AreEqual('1,3,5,9,11', ReducedStr);
end;

//______________________________________________________________________________________________________________________

constructor TTestObject.Create(OnDestroy: TProc);
begin
  inherited Create;
  FOnDestroy := OnDestroy;
end;

//______________________________________________________________________________________________________________________

destructor TTestObject.Destroy;
begin
  FOnDestroy;
  inherited;
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TUtilsTest);
  TDUnitX.RegisterTestFixture(TArrayUtilsTest);

end.
