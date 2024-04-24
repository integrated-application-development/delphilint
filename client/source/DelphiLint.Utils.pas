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
unit DelphiLint.Utils;

interface

uses
    System.SysUtils
  , System.TimeSpan
  , DelphiLint.Context
  ;

// Project utils
function ToAbsolutePath(const RelativePath: string; const BaseDir: string): string;
function NormalizePath(const Path: string): string;
function GetAllFiles: TArray<string>;
function IsPasFile(const Path: string): Boolean;
function IsMainFile(const Path: string): Boolean;
function IsDelphiSource(const Path: string): Boolean;
function IsProjectFile(const Path: string): Boolean;
function GetOpenSourceModules: TArray<IIDEModule>;
function TryGetProjectFile(out ProjectFile: string): Boolean;
function IsFileInProjectDirectory(const Path: string): Boolean;
function IsFileInProject(const Path: string): Boolean;
function TryGetProjectDirectory(out ProjectDir: string; ReadOptions: Boolean = True): Boolean; overload;

// ToolsAPI utils
function TryGetCurrentSourceEditor(out Editor: IIDESourceEditor): Boolean;
function GetDelphiVersion: string;

// General utils
function TimeSpanToAgoString(TimeSpan: TTimeSpan): string;

type
  TArrayUtils = class(TObject)
  public
    class function Map<X, Y>(Arr: TArray<X>; Mapper: TFunc<X, Y>): TArray<Y>; static;
    class function Reduce<X, Y>(Arr: TArray<X>; Accumulator: TFunc<Y, X, Y>): Y; overload; static;
    class function Reduce<X, Y>(Arr: TArray<X>; Accumulator: TFunc<Y, X, Y>; StartValue: Y): Y; overload; static;
    class function Max<X>(Arr: TArray<X>): X; overload; static;
    class function Max<X>(Arr: TArray<X>; DefaultValue: X): X; overload; static;
  end;

  TWrapper<T> = class(TObject)
  private
    FRaw: T;
  public
    constructor Create(Raw: T);
    function Get: T;

    class function WrapArray(Arr: TArray<T>): TArray<TWrapper<T>>;
  end;

implementation

uses
    System.IOUtils
  , System.Classes
  , System.StrUtils
  , System.Generics.Defaults
  , DelphiLint.ProjectOptions
  , Winapi.ShLwApi
  , System.Generics.Collections
  , System.Math
  ;

//______________________________________________________________________________________________________________________

function ToAbsolutePath(const RelativePath: string; const BaseDir: string): string;
var
  Buffer: array[0..512] of Char;
begin
  Result := TPath.Combine(BaseDir, RelativePath);
  if PathCanonicalize(@Buffer[0], PChar(Result)) then begin
    Result := Buffer;
  end;
end;

//______________________________________________________________________________________________________________________

function NormalizePath(const Path: string): string;
begin
  Result := StringReplace(LowerCase(Path), '\', '/', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

function TryGetCurrentSourceEditor(out Editor: IIDESourceEditor): Boolean;
var
  Module: IIDEModule;
begin
  Result := False;
  Module := LintContext.IDEServices.GetCurrentModule;
  if Assigned(Module) then begin
    Editor := Module.GetSourceEditor;
    Result := Assigned(Editor);
  end;
end;

//______________________________________________________________________________________________________________________

function GetDelphiVersion: string;
var
  ProductVersion: string;
begin
  ProductVersion := LintContext.IDEServices.ExpandRootMacro('$(ProductVersion)');

  if ProductVersion = '22.0' then begin
    Result := 'VER350';
  end
  else if ProductVersion = '21.0' then begin
    Result := 'VER340';
  end
  else if ProductVersion = '20.0' then begin
    Result := 'VER330';
  end
  else begin
    Result := 'VER360';
  end;
end;

//______________________________________________________________________________________________________________________

function GetOpenSourceModules: TArray<IIDEModule>;
var
  Index: Integer;
  Module: IIDEModule;
  Files: TList<IIDEModule>;
begin
  Files := TList<IIDEModule>.Create;
  try
    for Index := 0 to LintContext.IDEServices.GetModuleCount - 1 do begin
      Module := LintContext.IDEServices.GetModule(Index);

      if IsPasFile(Module.FileName) then begin
        Files.Add(Module);
      end;
    end;

    Result := Files.ToArray;
  finally
    FreeAndNil(Files);
  end;
end;

//______________________________________________________________________________________________________________________

function TryGetProjectDirectory(out ProjectDir: string; ReadOptions: Boolean = True): Boolean;
var
  ProjectFile: string;
  ProjectOptions: TLintProjectOptions;
begin
  Result := False;

  if TryGetProjectFile(ProjectFile) then begin
    Result := True;

    if ReadOptions then begin
      try
        ProjectOptions := LintContext.GetProjectOptions(ProjectFile);
        ProjectDir := ProjectOptions.AnalysisBaseDirAbsolute;
      finally
        FreeAndNil(ProjectOptions);
      end;

      if ProjectDir <> '' then begin
        Exit;
      end;
    end;

    ProjectDir := TPath.GetDirectoryName(ProjectFile);
  end;
end;

//______________________________________________________________________________________________________________________

function IsMainFile(const Path: string): Boolean;
begin
  Result := EndsText('.dpk', Path) or EndsText('.dpr', Path);
end;

//______________________________________________________________________________________________________________________

function IsPasFile(const Path: string): Boolean;
begin
  Result := EndsText('.pas', Path);
end;

//______________________________________________________________________________________________________________________

function IsDelphiSource(const Path: string): Boolean;
begin
  Result := IsPasFile(Path) or IsMainFile(Path);
end;

//______________________________________________________________________________________________________________________

function IsProjectFile(const Path: string): Boolean;
begin
  Result := EndsText('.dproj', Path) or EndsText('.groupproj', Path);
end;

//______________________________________________________________________________________________________________________

function GetAllFiles: TArray<string>;
var
  Project: IIDEProject;
  FileList: TStringList;
  I: Integer;
begin
  Project := LintContext.IDEServices.GetActiveProject;

  if not Assigned(Project) then begin
    Exit;
  end;

  FileList := TStringList.Create;
  Project.GetCompleteFileList(FileList);

  for I := FileList.Count - 1 downto 0 do begin
    if not (IsDelphiSource(FileList[I]) or IsProjectFile(FileList[I])) then begin
      FileList.Delete(I);
    end;
  end;

  Result := FileList.ToStringArray;
end;

//______________________________________________________________________________________________________________________

function TryGetProjectFile(out ProjectFile: string): Boolean;
var
  AllFiles: TArray<string>;
  FilePath: string;
begin
  Result := False;

  AllFiles := GetAllFiles;
  for FilePath in AllFiles do begin
    if IsProjectFile(FilePath) then begin
     ProjectFile := FilePath;
     Result := True;
     Exit;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function IsFileInProjectDirectory(const Path: string): Boolean;
var
  ProjectDir: string;
begin
  Result := TryGetProjectDirectory(ProjectDir) and StartsText(NormalizePath(ProjectDir), NormalizePath(Path));
end;

//______________________________________________________________________________________________________________________

function IsFileInProject(const Path: string): Boolean;
begin
  Result := (Path <> '') and IsDelphiSource(Path) and FileExists(Path) and IsFileInProjectDirectory(Path);
end;

//______________________________________________________________________________________________________________________

class function TArrayUtils.Map<X, Y>(Arr: TArray<X>; Mapper: TFunc<X, Y>): TArray<Y>;
var
  I: Integer;
begin
  SetLength(Result, Length(Arr));

  for I := 0 to Length(Arr) - 1 do begin
    Result[I] := Mapper(Arr[I]);
  end;
end;

//______________________________________________________________________________________________________________________

class function TArrayUtils.Reduce<X, Y>(Arr: TArray<X>; Accumulator: TFunc<Y, X, Y>): Y;
begin
  if Length(Arr) = 0 then begin
    raise ERangeError.Create('Can''t reduce empty array without default');
  end;

  Result := Reduce<X, Y>(Arr, Accumulator, Default(Y));
end;

//______________________________________________________________________________________________________________________

class function TArrayUtils.Reduce<X, Y>(Arr: TArray<X>; Accumulator: TFunc<Y, X, Y>; StartValue: Y): Y;
var
  I: Integer;
begin
  Result := StartValue;

  for I := 0 to Length(Arr) - 1 do begin
    Result := Accumulator(Result, Arr[I]);
  end;
end;

//______________________________________________________________________________________________________________________

class function TArrayUtils.Max<X>(Arr: TArray<X>): X;
begin
  if Length(Arr) = 0 then begin
    raise ERangeError.Create('Can''t get maximum of empty array without default');
  end;

  Result := Max<X>(Arr, Default(X));
end;

//______________________________________________________________________________________________________________________

class function TArrayUtils.Max<X>(Arr: TArray<X>; DefaultValue: X): X;
var
  Comparer: IComparer<X>;
begin
  Comparer := TComparer<X>.Default;

  if Length(Arr) > 0 then begin
    Result := Reduce<X, X>(
      Arr,
      function (Highest: X; Current: X): X begin
        Result := Highest;
        if Comparer.Compare(Current, Highest) > 0 then begin
          Result := Current;
        end;
      end
    );
  end
  else begin
    Result := DefaultValue;
  end;
end;

//______________________________________________________________________________________________________________________

function TimeSpanToAgoString(TimeSpan: TTimeSpan): string;

  function AgoInt(Num: Integer; TimeUnit: string): string;
  begin
    Result := Format(
      '%d %s ago',
      [Num, IfThen(Num = 1, TimeUnit, TimeUnit + 's')]
    );
  end;

  function Ago(Num: Double; TimeUnit: string): string;
  begin
    Result := AgoInt(Floor(Num), TimeUnit);
  end;

begin
  if TimeSpan.TotalHours < 1 then begin
    Result := Ago(TimeSpan.TotalMinutes, 'minute');
  end
  else if TimeSpan.TotalDays < 1 then begin
    Result := Ago(TimeSpan.TotalHours, 'hour');
  end
  else if TimeSpan.TotalDays < 365 then begin
    Result := Ago(TimeSpan.TotalDays, 'day');
  end
  else begin
    Result := Ago(TimeSpan.TotalDays / 365, 'year');
  end;
end;

//______________________________________________________________________________________________________________________

constructor TWrapper<T>.Create(Raw: T);
begin
  FRaw := Raw;
end;

//______________________________________________________________________________________________________________________

function TWrapper<T>.Get: T;
begin
  Result := FRaw;
end;

//______________________________________________________________________________________________________________________

class function TWrapper<T>.WrapArray(Arr: TArray<T>): TArray<TWrapper<T>>;
var
  Index: Integer;
begin
  SetLength(Result, Length(Arr));

  for Index := 0 to Length(Arr) - 1 do begin
    Result[Index] := TWrapper<T>.Create(Arr[Index]);
  end;
end;

end.
