unit DelphiLint.Utils;

interface

uses
    ToolsAPI
  ;


function GetProjectDirectory(ReadOptions: Boolean = True): string; overload;
function GetAllFiles: TArray<string>;
function GetProjectFile: string;
function IsPasFile(Path: string): Boolean;
function IsMainFile(Path: string): Boolean;
function IsDelphiSource(Path: string): Boolean;
function IsProjectFile(Path: string): Boolean;
procedure ExtractFiles(out AllFiles: TArray<string>; out ProjectFile: string; out MainFile: string; out PasFiles: TArray<string>);
function GetCurrentSourceEditor: IOTASourceEditor;
procedure RefreshEditorWindows;
function NormalizePath(const Path: string): string;
function GetOpenSourceFiles: TArray<string>;

implementation

uses
    System.IOUtils
  , System.Classes
  , System.StrUtils
  , System.SysUtils
  , DelphiLint.ProjectOptions
  ;

//______________________________________________________________________________________________________________________

procedure RefreshEditorWindows;
begin
  (BorlandIDEServices as IOTAEditorServices).TopView.GetEditWindow.Form.Repaint;
end;

//______________________________________________________________________________________________________________________

function NormalizePath(const Path: string): string;
begin
  Result := StringReplace(LowerCase(Path), '\', '/', [rfReplaceAll]);
end;

//______________________________________________________________________________________________________________________

function GetCurrentSourceEditor: IOTASourceEditor;
var
  Module: IOTAModule;
  I: Integer;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(Module) then begin
    for I := 0 to Module.ModuleFileCount - 1 do begin
      if Module.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, Result) = S_OK then begin
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

function GetProjectDirectory(ReadOptions: Boolean = True): string;
var
  ProjectFile: string;
begin
  ProjectFile := GetProjectFile;

  if ReadOptions then begin
    Result := TLintProjectOptions.Create(ProjectFile).ProjectBaseDir;

    if Result <> '' then begin
      Exit;
    end;
  end;

  Result := TPath.GetDirectoryName(ProjectFile);
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

function IsMainFile(Path: string): Boolean;
begin
  Result := EndsText('.dpk', Path) or EndsText('.dpr', Path);
end;

//______________________________________________________________________________________________________________________

function IsPasFile(Path: string): Boolean;
begin
  Result := EndsText('.pas', Path);
end;

//______________________________________________________________________________________________________________________

function IsDelphiSource(Path: string): Boolean;
begin
  Result := IsPasFile(Path) or IsMainFile(Path);
end;

//______________________________________________________________________________________________________________________

function IsProjectFile(Path: string): Boolean;
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

function GetProjectFile: string;
var
  AllFiles: TArray<string>;
  FilePath: string;
begin
  AllFiles := GetAllFiles;

  for FilePath in AllFiles do begin
    if IsProjectFile(FilePath) then begin
     Result := FilePath;
     Exit;
    end;
  end;
end;

end.
