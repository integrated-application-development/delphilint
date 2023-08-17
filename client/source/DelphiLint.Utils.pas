{
DelphiLint Client for RAD Studio
Copyright (C) 2023 Integrated Application Development

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
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
    class function Map<X, Y>(Arr: TArray<X>; Mapper: TFunc<X, Y>): TArray<Y>; static;
  end;

implementation

uses
    System.IOUtils
  , System.Classes
  , System.StrUtils
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

  if ProductVersion = '21.0' then begin
    Result := 'VER340';
  end
  else if ProductVersion = '20.0' then begin
    Result := 'VER330';
  end
  else if ProductVersion = '19.0' then begin
    Result := 'VER320';
  end
  else if ProductVersion = '18.0' then begin
    Result := 'VER310';
  end
  else begin
    Result := 'VER350';
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
begin
  Result := False;

  if TryGetProjectFile(ProjectFile) then begin
    Result := True;

    if ReadOptions then begin
      ProjectDir := TLintProjectOptions.Create(ProjectFile).AnalysisBaseDirAbsolute;

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

end.
