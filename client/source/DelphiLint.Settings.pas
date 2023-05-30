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

  FSettingsDir: string;
  FSettingsFile: string;
private
  constructor Create;

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
end;

function LintSettings: TLintSettings;

implementation

uses
    System.IniFiles
  , System.SysUtils
  , System.IOUtils
  ;

var
  G_LintSettings: TLintSettings;

function LintSettings: TLintSettings;
begin
  if not Assigned(G_LintSettings) then begin
    G_LintSettings := TLintSettings.Create;
  end;
  Result := G_LintSettings;
end;

{ TLintSettings }

constructor TLintSettings.Create;
begin
  FSettingsDir := TPath.Combine(TPath.GetHomePath, 'DelphiLint');
  FSettingsFile := TPath.Combine(FSettingsDir, 'delphilint.ini');

  Reload;
  Save;
end;

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
  finally
    FreeAndNil(Ini);
  end;
end;

end.
