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
    function GetSonarHostToken: string;
    function GetProjectPropertiesPath: string;

    procedure SetProjectKey(Value: string);
    procedure SetSonarHostUrl(Value: string);
    procedure SetProjectBaseDir(Value: string);
    procedure SetSonarHostToken(Value: string);
    procedure SetProjectPropertiesPath(Value: string);

    function StrFromIni(const Section: string; const Identifier: string): string;
    procedure StrToIni(const Section: string; const Identifier: string; const Value: string);
  public
    constructor Create(ProjectFilePath: string);

    property ProjectKey: string read GetProjectKey write SetProjectKey;
    property SonarHostUrl: string read GetSonarHostUrl write SetSonarHostUrl;
    property ProjectBaseDir: string read GetProjectBaseDir write SetProjectBaseDir;
    property SonarHostToken: string read GetSonarHostToken write SetSonarHostToken;
    property ProjectPropertiesPath: string read GetProjectPropertiesPath write SetProjectPropertiesPath;
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

function TLintProjectOptions.GetProjectPropertiesPath: string;
begin
  Result := StrFromIni('Project', 'PropertiesPath');
end;

function TLintProjectOptions.GetSonarHostUrl: string;
begin
  Result := StrFromIni('SonarHost', 'Url');
end;

function TLintProjectOptions.GetSonarHostToken: string;
begin
  Result := StrFromIni('SonarHost', 'Token');
end;

procedure TLintProjectOptions.SetProjectBaseDir(Value: string);
begin
  StrToIni('Project', 'BaseDir', Value);
end;

procedure TLintProjectOptions.SetProjectKey(Value: string);
begin
  StrToIni('Project', 'Key', Value);
end;

procedure TLintProjectOptions.SetProjectPropertiesPath(Value: string);
begin
  StrToIni('Project', 'PropertiesPath', Value);
end;

procedure TLintProjectOptions.SetSonarHostToken(Value: string);
begin
  StrToIni('SonarHost', 'Token', Value);
end;

procedure TLintProjectOptions.SetSonarHostUrl(Value: string);
begin
  StrToIni('SonarHost', 'Url', Value);
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

procedure TLintProjectOptions.StrToIni(const Section, Identifier, Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FOptionsFile);
  try
    if Value = '' then begin
      Ini.DeleteKey(Section, Identifier);
    end
    else begin
      Ini.WriteString(Section, Identifier, Value);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

end.
