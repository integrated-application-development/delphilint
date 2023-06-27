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

uses
    DelphiLint.Properties
  ;

type
  TLintProjectOptions = class(TPropertiesFile)
  protected
    function RegisterFields: TArray<TPropFieldBase>; override;
  public
    constructor Create(Path: string);

    property ProjectKey: string index 0 read GetValueStr write SetValueStr;
    property SonarHostUrl: string index 1 read GetValueStr write SetValueStr;
    property ProjectBaseDir: string index 2 read GetValueStr write SetValueStr;
    property SonarHostToken: string index 3 read GetValueStr write SetValueStr;
    property ProjectPropertiesPath: string index 4 read GetValueStr write SetValueStr;
  end;

implementation

uses
    System.IOUtils
  ;

//______________________________________________________________________________________________________________________

constructor TLintProjectOptions.Create(Path: string);
begin
  Path := TPath.ChangeExtension(Path, '.delphilint');
  inherited Create(Path);
  Load;
end;

//______________________________________________________________________________________________________________________

function TLintProjectOptions.RegisterFields: TArray<TPropFieldBase>;
begin
  Result := [
    // 0
    TStringPropField.Create('Project', 'Key'),
    // 1
    TStringPropField.Create('SonarHost', 'Url'),
    // 2
    TStringPropField.Create('Project', 'BaseDir'),
    // 3
    TStringPropField.Create('SonarHost', 'Token'),
    // 4
    TStringPropField.Create('Project', 'PropertiesPath')
  ];
end;

end.
