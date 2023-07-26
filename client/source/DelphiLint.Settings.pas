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
unit DelphiLint.Settings;

interface

uses
    DelphiLint.Properties
  ;

type
  TLintSettings = class(TPropertiesFile)
  private
    FSettingsDir: string;

    constructor Create;

    function GetServerJar(Index: Integer): string;
    function GetSonarDelphiJar(Index: Integer): string;

    function GetDefaultServerJavaExe: string;
  protected
    function RegisterFields: TArray<TPropFieldBase>; override;
  public
    property ServerJarOverride: string index 0 read GetValueStr write SetValueStr;
    property SonarDelphiJarOverride: string index 1 read GetValueStr write SetValueStr;
    property ServerJavaExe: string index 2 read GetValueStr write SetValueStr;
    property ServerShowConsole: Boolean index 3 read GetValueBool write SetValueBool;
    property ServerAutoLaunch: Boolean index 4 read GetValueBool write SetValueBool;
    property ClientAutoShowToolWindow: Boolean index 5 read GetValueBool write SetValueBool;

    property ServerJar: string index 0 read GetServerJar;
    property SonarDelphiJar: string index 1 read GetSonarDelphiJar;
    property SettingsDirectory: string read FSettingsDir;
  end;

function LintSettings: TLintSettings;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , DelphiLint.Version
  ;

var
  GLintSettings: TLintSettings;

//______________________________________________________________________________________________________________________

function LintSettings: TLintSettings;
begin
  if not Assigned(GLintSettings) then begin
    GLintSettings := TLintSettings.Create;
  end;
  Result := GLintSettings;
end;

//______________________________________________________________________________________________________________________

constructor TLintSettings.Create;
begin
  FSettingsDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint');
  inherited Create(TPath.Combine(FSettingsDir, 'delphilint.ini'));

  Load;
  Save;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.RegisterFields: TArray<TPropFieldBase>;
begin
  Result := [
    // 0
    TStringPropField.Create('Resources', 'ServerJarOverride', ''),
    // 1
    TStringPropField.Create('Resources', 'SonarDelphiJarOverride', ''),
    // 2
    TCustomStringPropField.Create('Resources', 'JavaExe', GetDefaultServerJavaExe),
    // 3
    TBoolPropField.Create('Server', 'ShowConsole', False),
    // 4
    TBoolPropField.Create('Server', 'AutoLaunch', True),
    // 5
    TBoolPropField.Create('Client', 'AutoShowToolWindow', True)
  ];
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetServerJar(Index: Integer): string;
begin
  Result := GetValueStr(Index);
  if Result = '' then begin
    Result := TPath.Combine(FSettingsDir, Format('delphilint-server-%s.jar', [DelphiLintVersion]));
  end;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetSonarDelphiJar(Index: Integer): string;
begin
  Result := GetValueStr(Index);
  if Result = '' then begin
    Result := TPath.Combine(FSettingsDir, 'sonar-delphi-plugin.jar');
  end;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetDefaultServerJavaExe: string;
var
  JavaHome: string;
begin
  Result := '';

  JavaHome := GetEnvironmentVariable('JAVA_HOME');
  if JavaHome <> '' then begin
    Result := Format('%s\bin\java.exe', [JavaHome]);
  end;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(GLintSettings);

end.
