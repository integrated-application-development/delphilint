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
