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
    ToolsAPI
  ;


// Project utils
function NormalizePath(const Path: string): string;
function GetAllFiles: TArray<string>;
function IsPasFile(const Path: string): Boolean;
function IsMainFile(const Path: string): Boolean;
function IsDelphiSource(const Path: string): Boolean;
function IsProjectFile(const Path: string): Boolean;
procedure ExtractFiles(out AllFiles: TArray<string>; out ProjectFile: string; out MainFile: string; out PasFiles: TArray<string>);
function GetOpenSourceFiles: TArray<string>;
function TryGetProjectFile(out ProjectFile: string): Boolean;
function IsFileInProjectDirectory(const Path: string): Boolean;
function IsFileInProject(const Path: string): Boolean;
function TryGetProjectDirectory(out ProjectDir: string; ReadOptions: Boolean = True): Boolean; overload;

// ToolsAPI utils
function TryGetCurrentSourceEditor(out Editor: IOTASourceEditor): Boolean;

implementation

uses
    System.IOUtils
  , System.Classes
  , System.StrUtils
  , System.SysUtils
  , DelphiLint.ProjectOptions
  ;

//______________________________________________________________________________________________________________________

function NormalizePath(const Path: string): string;
begin
  Result := StringReplace(LowerCase(Path), '\', '/', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

function TryGetCurrentSourceEditor(out Editor: IOTASourceEditor): Boolean;
var
  Module: IOTAModule;
  I: Integer;
begin
  Result := False;
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(Module) then begin
    for I := 0 to Module.ModuleFileCount - 1 do begin
      if Module.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, Editor) = S_OK then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function GetOpenSourceFiles: TArray<string>;
var
  Index: Integer;
  Module: IOTAModule;
  ModuleServices: IOTAModuleServices;
  Files: TStringList;
begin
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);

  Files := TStringList.Create;
  try
    for Index := 0 to ModuleServices.ModuleCount - 1 do begin
      Module := ModuleServices.Modules[Index];

      if IsPasFile(Module.FileName) then begin
        Files.Add(Module.FileName);
      end;
    end;

    Result := Files.ToStringArray;
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
      ProjectDir := TLintProjectOptions.Create(ProjectFile).ProjectBaseDir;

      if ProjectDir <> '' then begin
        Exit;
      end;
    end;

    ProjectDir := TPath.GetDirectoryName(ProjectFile);
  end;
end;

//______________________________________________________________________________________________________________________

function GetPasFiles(Files: TArray<string>): TArray<string>;
var
  PasFiles: TStringList;
  FilePath: string;
begin
  PasFiles := TStringList.Create;

  for FilePath in Files do begin
    if IsPasFile(FilePath) then begin
      PasFiles.Add(FilePath);
    end;
  end;

  Result := PasFiles.ToStringArray;
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
  Project: IOTAProject;
  FileList: TStringList;
  I: Integer;
begin
  Project := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;

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

procedure ExtractFiles(out AllFiles: TArray<string>; out ProjectFile: string; out MainFile: string; out PasFiles: TArray<string>);
var
  FilePath: string;
  PasFilesList: TStringList;
begin
  AllFiles := GetAllFiles;
  PasFilesList := TStringList.Create;

  for FilePath in AllFiles do begin
    if IsPasFile(FilePath) then begin
      PasFilesList.Add(FilePath);
    end
    else if IsMainFile(FilePath) then begin
      MainFile := FilePath;
    end
    else if IsProjectFile(FilePath) then begin
     ProjectFile := FilePath;
    end;
  end;

  PasFiles := PasFilesList.ToStringArray;
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

end.
