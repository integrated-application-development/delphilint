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
    System.Generics.Collections
  , DelphiLint.Properties
  ;

type
  TSonarProjectIdentifier = record
    Host: string;
    ProjectKey: string;

    constructor Create(Host: string; ProjectKey: string);
  end;

  TLintSettings = class(TPropertiesFile)
  private
    FSettingsDir: string;
    FTokensMap: TDictionary<TSonarProjectIdentifier, string>;

    function GetServerJar(Index: Integer): string;
    function GetJavaExe(Index: Integer): string;
    function GetDefaultJavaExe: string;
    function GetSonarDelphiVersion(Index: Integer): string;
    function GetDefaultSonarDelphiVersion: string;
    procedure SyncTokenMap;
    procedure SyncTokenString;
  protected
    function RegisterFields: TArray<TPropFieldBase>; override;

    property SonarHostTokens: string index 6 read GetValueStr write SetValueStr;
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    procedure Save; override;
    procedure Load; override;

    function GetSonarHostToken(Host: string; ProjectKey: string): string;

    property ServerJarOverride: string index 0 read GetValueStr write SetValueStr;
    property ServerJavaExeOverride: string index 1 read GetValueStr write SetValueStr;
    property DebugShowConsole: Boolean index 2 read GetValueBool write SetValueBool;
    property DebugExternalServer: Boolean index 3 read GetValueBool write SetValueBool;
    property ClientAutoShowToolWindow: Boolean index 4 read GetValueBool write SetValueBool;
    property ClientSaveBeforeAnalysis: Boolean index 5 read GetValueBool write SetValueBool;
    // SonarHostTokens index 6
    property ServerSonarDelphiVersionOverride: string index 7 read GetValueStr write SetValueStr;

    property ServerJar: string index 0 read GetServerJar;
    property JavaExe: string index 1 read GetJavaExe;
    property DefaultJavaExe: string read GetDefaultJavaExe;
    property SonarDelphiVersion: string index 7 read GetSonarDelphiVersion;
    property DefaultSonarDelphiVersion: string read GetDefaultSonarDelphiVersion;
    property SettingsDirectory: string read FSettingsDir;
    property SonarHostTokensMap: TDictionary<TSonarProjectIdentifier, string> read FTokensMap;
  end;

implementation

uses
    System.SysUtils
  , System.IOUtils
  , System.StrUtils
  , System.Generics.Defaults
  , System.Hash
  , DelphiLint.Version
  , DelphiLint.Context
  ;

type
  TSonarProjectIdentifierComparer = class(TEqualityComparer<TSonarProjectIdentifier>)
    function Equals(const Left: TSonarProjectIdentifier; const Right: TSonarProjectIdentifier): Boolean; override;
    function GetHashCode(const Value: TSonarProjectIdentifier): Integer; override;
  end;

//______________________________________________________________________________________________________________________

constructor TLintSettings.Create(Path: string);
begin
  FSettingsDir := TPath.GetDirectoryName(Path);
  FTokensMap := TDictionary<TSonarProjectIdentifier, string>.Create(TSonarProjectIdentifierComparer.Create);

  inherited Create(Path);

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
    TStringPropField.Create('Resources', 'JavaExeOverride', ''),
    // 2
    TBoolPropField.Create('Debug', 'ShowConsole', False),
    // 3
    TBoolPropField.Create('Debug', 'ExternalServer', False),
    // 4
    TBoolPropField.Create('Client', 'AutoShowToolWindow', True),
    // 5
    TBoolPropField.Create('Client', 'SaveBeforeAnalysis', True),
    // 6
    TStringPropField.Create('SonarHost', 'Tokens', True),
    // 7
    TStringPropField.Create('Server', 'SonarDelphiVersionOverride', '')
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

function TLintSettings.GetDefaultJavaExe: string;
var
  JavaHome: string;
begin
  JavaHome := GetEnvironmentVariable('JAVA_HOME');
  if JavaHome <> '' then begin
    Result := Format('%s\bin\java.exe', [JavaHome]);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetJavaExe(Index: Integer): string;
begin
  Result := GetValueStr(Index);
  if Result = '' then begin
    Result := GetDefaultJavaExe;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetDefaultSonarDelphiVersion: string;
begin
  Result := '1.3.0';
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetSonarDelphiVersion(Index: Integer): string;
begin
  Result := GetValueStr(Index);
  if Result = '' then begin
    Result := GetDefaultSonarDelphiVersion;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettings.Load;
begin
  inherited;
  SyncTokenMap;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettings.Save;
begin
  SyncTokenString;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettings.SyncTokenMap;
var
  HostTokenPairs: TArray<string>;
  Pair: string;
  SplitPair: TArray<string>;
  SplitIdent: TArray<string>;
begin
  FTokensMap.Clear;

  Log.Info(SonarHostTokens);
  HostTokenPairs := SplitString(SonarHostTokens, ',');
  for Pair in HostTokenPairs do begin
    SplitPair := SplitString(Pair, '=');

    if Length(SplitPair) <> 2 then begin
      Log.Warn('Skipping invalid value ''%s'' in Sonar token mapping', [Pair]);
      Continue;
    end;

    SplitIdent := SplitString(SplitPair[0], '@');

    if Length(SplitPair) <> 2 then begin
      Log.Warn('Skipping invalid value for key ''%s'' in Sonar token mapping', [SplitPair[0]]);
      Continue;
    end;

    FTokensMap.AddOrSetValue(
      TSonarProjectIdentifier.Create(SplitIdent[1], SplitIdent[0]),
      SplitPair[1]);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettings.SyncTokenString;
var
  Ident: TSonarProjectIdentifier;
  TokenStr: string;
  SortedIdents: TArray<TSonarProjectIdentifier>;
begin
  TokenStr := '';

  SortedIdents := FTokensMap.Keys.ToArray;
  TArray.Sort<TSonarProjectIdentifier>(
    SortedIdents,
    TComparer<TSonarProjectIdentifier>.Construct(
      function(const Left: TSonarProjectIdentifier; const Right: TSonarProjectIdentifier): Integer
      begin
        if Left.ProjectKey <> Right.ProjectKey then begin
          Result := TComparer<string>.Default.Compare(Left.ProjectKey, Right.ProjectKey);
        end
        else begin
          Result := TComparer<string>.Default.Compare(Left.Host, Right.Host);
        end;
      end));

  for Ident in SortedIdents do begin
    TokenStr := Format('%s%s%s@%s=%s', [
      TokenStr,
      IfThen(TokenStr = '', '', ','),
      Ident.ProjectKey,
      Ident.Host,
      FTokensMap[Ident]
    ]);
  end;
  SonarHostTokens := TokenStr;
end;
//______________________________________________________________________________________________________________________

destructor TLintSettings.Destroy;
begin
  FreeAndNil(FTokensMap);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TLintSettings.GetSonarHostToken(Host: string; ProjectKey: string): string;
var
  Key: TSonarProjectIdentifier;
begin
  Key := TSonarProjectIdentifier.Create(Host, ProjectKey);

  Result := '';
  if FTokensMap.ContainsKey(Key) then begin
    Result := FTokensMap[Key];
  end;
end;

//______________________________________________________________________________________________________________________

constructor TSonarProjectIdentifier.Create(Host, ProjectKey: string);
begin
  Self.Host := Host;
  Self.ProjectKey := ProjectKey;
end;

//______________________________________________________________________________________________________________________

function TSonarProjectIdentifierComparer.Equals(const Left, Right: TSonarProjectIdentifier): Boolean;
begin
  Result := (Left.Host = Right.Host) and (Left.ProjectKey = Right.ProjectKey);
end;

//______________________________________________________________________________________________________________________

{$Q-}
function TSonarProjectIdentifierComparer.GetHashCode(const Value: TSonarProjectIdentifier): Integer;
begin
  Result := 17;
  Result := Result * 31 * THashBobJenkins.GetHashValue(Value.Host);
  Result := Result * 31 * THashBobJenkins.GetHashValue(Value.ProjectKey);
end;
{$Q+}

//______________________________________________________________________________________________________________________

end.
