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

    function GetDefaultServerJar: string;
    function GetDefaultSonarDelphiJar: string;
    function GetDefaultServerJavaExe: string;
    function GetDefaultClientDarkMode: Boolean;
  protected
    function RegisterFields: TArray<TPropFieldBase>; override;
  public
    property ServerJar: string index 0 read GetValueStr write SetValueStr;
    property ServerShowConsole: Boolean index 1 read GetValueBool write SetValueBool;
    property SonarDelphiJar: string index 2 read GetValueStr write SetValueStr;
    property ServerJavaExe: string index 3 read GetValueStr write SetValueStr;
    property ServerStartDelay: Integer index 4 read GetValueInt write SetValueInt;
    property ServerAutoLaunch: Boolean index 5 read GetValueBool write SetValueBool;
    property ClientDarkMode: Boolean index 6 read GetValueBool write SetValueBool;

    property SettingsDirectory: string read FSettingsDir;
  end;

function LintSettings: TLintSettings;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , ToolsAPI
  , Vcl.Themes
  , Vcl.Graphics
  , Winapi.Windows
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
    TCustomStringPropField.Create('Server', 'Jar', GetDefaultServerJar),
    // 1
    TBoolPropField.Create('Server', 'ShowConsole', False),
    // 2
    TCustomStringPropField.Create('SonarDelphi', 'Jar', GetDefaultSonarDelphiJar),
    // 3
    TCustomStringPropField.Create('Server', 'JavaExe', GetDefaultServerJavaExe),
    // 4
    TIntPropField.Create('Server', 'StartDelay', 1000),
    // 5
    TBoolPropField.Create('Server', 'AutoLaunch', True),
    // 6
    TCustomBoolPropField.Create('Client', 'DarkMode', GetDefaultClientDarkMode)
  ];
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetDefaultClientDarkMode: Boolean;
var
  BgColor: TColor;
  Color: LongInt;
begin
  BgColor := (BorlandIDEServices as IOTAIDEThemingServices).StyleServices.GetStyleColor(scGenericBackground);
  Color := ColorToRGB(BgColor);

  Result := ((GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) < 384);
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetDefaultServerJar: string;
begin
  Result := TPath.Combine(FSettingsDir, 'delphilint-server.jar');
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

function TLintSettings.GetDefaultSonarDelphiJar: string;
begin
  Result := TPath.Combine(FSettingsDir, 'sonar-delphi-plugin.jar');
end;

//______________________________________________________________________________________________________________________

end.
