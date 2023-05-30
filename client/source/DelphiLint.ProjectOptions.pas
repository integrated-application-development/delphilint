unit DelphiLint.ProjectOptions;

interface

type
  TLintProjectOptions = class(TObject)
  private
    FOptionsFile: string;

    function GetProjectKey: string;
    function GetSonarHostUrl: string;
    function GetProjectBaseDir: string;

    function StrFromIni(const Section: string; const Identifier: string): string;
  public
    constructor Create(ProjectFilePath: string);

    property ProjectKey: string read GetProjectKey;
    property SonarHostUrl: string read GetSonarHostUrl;
    property ProjectBaseDir: string read GetProjectBaseDir;
  end;

implementation

uses
    System.IniFiles
  , System.SysUtils
  ;

{ TProjectOptions }

constructor TLintProjectOptions.Create(ProjectFilePath: string);
begin
  FOptionsFile := ChangeFileExt(ProjectFilePath, '.delphilint');
end;

function TLintProjectOptions.GetProjectBaseDir: string;
begin
  Result := StrFromIni('Project', 'BaseDir');
end;

function TLintProjectOptions.GetProjectKey: string;
begin
  Result := StrFromIni('Project', 'Key');
end;

function TLintProjectOptions.GetSonarHostUrl: string;
begin
  Result := StrFromIni('SonarHost', 'Url');
end;

function TLintProjectOptions.StrFromIni(const Section, Identifier: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FOptionsFile);
  try
    Result := Ini.ReadString(Section, Identifier, '');
  finally
    FreeAndNil(Ini);
  end;
end;

end.
