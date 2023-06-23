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

type TLintSettings = class(TObject)
private var
  FServerJar: string;
  FServerPort: Integer;
  FServerJavaExe: string;
  FServerShowConsole: Boolean;
  FServerAutoLaunch: Boolean;
  FSonarDelphiJar: string;
  FServerStartDelay: Integer;
  FClientDarkMode: Boolean;

  FSettingsDir: string;
  FSettingsFile: string;
private
  constructor Create;

  function InDarkMode: Boolean;

public
  procedure Save;
  procedure Reload;

  property ServerJar: string read FServerJar;
  property ServerPort: Integer read FServerPort;
  property ServerShowConsole: Boolean read FServerShowConsole;
  property SonarDelphiJar: string read FSonarDelphiJar;
  property ServerJavaExe: string read FServerJavaExe;
  property ServerStartDelay: Integer read FServerStartDelay;
  property ServerAutoLaunch: Boolean read FServerAutoLaunch;
  property ClientDarkMode: Boolean read FClientDarkMode;
end;

function LintSettings: TLintSettings;

implementation

uses
    System.IniFiles
  , System.SysUtils
  , System.IOUtils
  , ToolsAPI
  , Vcl.Themes
  , Vcl.Graphics
  , Winapi.Windows
  ;

var
  G_LintSettings: TLintSettings;

//______________________________________________________________________________________________________________________

function LintSettings: TLintSettings;
begin
  if not Assigned(G_LintSettings) then begin
    G_LintSettings := TLintSettings.Create;
  end;
  Result := G_LintSettings;
end;

//______________________________________________________________________________________________________________________

constructor TLintSettings.Create;
begin
  FSettingsDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint');
  FSettingsFile := TPath.Combine(FSettingsDir, 'delphilint.ini');

  Reload;
  Save;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettings.Reload;
var
  Ini: TIniFile;
  JavaHome: string;
begin
  Ini := TIniFile.Create(FSettingsFile);
  try
    FServerJar := Ini.ReadString('Server', 'Jar', TPath.Combine(FSettingsDir, 'delphilint-server.jar'));
    FServerPort := Ini.ReadInteger('Server', 'Port', 14000);
    FServerShowConsole := Ini.ReadBool('Server', 'ShowConsole', False);
    FServerStartDelay := Ini.ReadInteger('Server', 'StartDelay', 1000);
    FServerAutoLaunch := Ini.ReadBool('Server', 'AutoLaunch', True);
    FSonarDelphiJar := Ini.ReadString('SonarDelphi', 'Jar', TPath.Combine(FSettingsDir, 'sonar-delphi-plugin.jar'));
    FClientDarkMode := Ini.ReadBool('Client', 'DarkMode', InDarkMode);

    FServerJavaExe := Ini.ReadString('Server', 'JavaExe', '');

    if FServerJavaExe = '' then begin
      JavaHome := GetEnvironmentVariable('JAVA_HOME');

      if JavaHome <> '' then begin
        FServerJavaExe := Format('%s\bin\java.exe', [JavaHome]);
      end;
    end;


  finally
    FreeAndNil(Ini);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettings.Save;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FSettingsFile);
  try
    Ini.WriteString('Server', 'Jar', FServerJar);
    Ini.WriteInteger('Server', 'Port', FServerPort);
    Ini.WriteBool('Server', 'ShowConsole', FServerShowConsole);
    Ini.WriteInteger('Server', 'StartDelay', FServerStartDelay);
    Ini.WriteBool('Server', 'AutoLaunch', FServerAutoLaunch);
    Ini.WriteString('SonarDelphi', 'Jar', FSonarDelphiJar);
    Ini.WriteString('Server', 'JavaExe', FServerJavaExe);
    Ini.WriteBool('Client', 'DarkMode', FClientDarkMode);
  finally
    FreeAndNil(Ini);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.InDarkMode: Boolean;
var
  BgColor: TColor;
  Color: Longint;
begin
  BgColor := (BorlandIDEServices as IOTAIDEThemingServices).StyleServices.GetStyleColor(scGenericBackground);
  Color := ColorToRGB(BgColor);

  Result := ((GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) < 384);
end;

end.
