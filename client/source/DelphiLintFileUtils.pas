unit DelphiLintFileUtils;

interface

function GetProjectDirectory: string;
function GetProjectFile: string;
function GetMainFile: string;
function GetAllFiles: TArray<string>;
function IsMainFile(Path: string): Boolean;
function IsDelphiSource(Path: string): Boolean;
function IsProjectFile(Path: string): Boolean;

implementation

uses
    ToolsAPI
  , System.Classes
  , System.StrUtils
  , System.IOUtils
  , DelphiLintLogger
  ;

//______________________________________________________________________________________________________________________

function GetProjectDirectory: string;
begin
  Result := TPath.GetDirectoryName(GetMainFile);
end;

//______________________________________________________________________________________________________________________

function GetProjectFile: string;
var
  AllFiles: TArray<string>;
  FilePath: string;
begin
  AllFiles := GetAllFiles;
            
  Result := '';
  for FilePath in AllFiles do begin
    if isProjectFile(FilePath) then begin
      Result := FilePath;
    end;
  end;
end;   

//______________________________________________________________________________________________________________________

function GetMainFile: string;
var
  AllFiles: TArray<string>;
  FilePath: string;
begin
  AllFiles := GetAllFiles;
                 
  Result := '';
  for FilePath in AllFiles do begin
    if IsMainFile(FilePath) then begin
      Result := FilePath;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function IsMainFile(Path: string): Boolean;
begin
  Result := EndsText('.dpk', Path) or EndsText('.dpr', Path);
end;

//______________________________________________________________________________________________________________________

function IsDelphiSource(Path: string): Boolean;
begin
  Result := EndsText('.pas', Path) or IsMainFile(Path);
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

  FileList := TStringList.Create;
  Project.GetCompleteFileList(FileList);

  for I := FileList.Count - 1 downto 0 do begin
    if not (IsDelphiSource(FileList[I]) or IsProjectFile(FileList[I])) then begin
      FileList.Delete(I);
    end;
  end;

  Result := FileList.ToStringArray;
end;

end.
