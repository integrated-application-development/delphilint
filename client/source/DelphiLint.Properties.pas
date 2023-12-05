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
unit DelphiLint.Properties;

interface

uses
    System.IniFiles
  ;

type
  TPropFieldBase = class abstract(TObject)
  private
    FSection: string;
    FKey: string;
    FValue: Variant;
    FDefaultValue: Variant;
  protected
    function GetFallback: Variant; virtual; abstract;
    function GetDefault: Variant;
  public
    constructor Create(Section: string; Key: string); overload;
    constructor Create(Section: string; Key: string; DefaultValue: Variant); overload;

    procedure Save(IniFile: TIniFile); virtual; abstract;
    procedure Load(IniFile: TIniFile); virtual; abstract;

    property Section: string read FSection;
    property Key: string read FKey;
    property Value: Variant read FValue write FValue;
  end;

  TStringPropField = class(TPropFieldBase)
  protected
    function GetFallback: Variant; override;
  public
    procedure Save(IniFile: TIniFile); override;
    procedure Load(IniFile: TIniFile); override;
  end;

  TBoolPropField = class(TPropFieldBase)
  protected
    function GetFallback: Variant; override;
  public
    procedure Save(IniFile: TIniFile); override;
    procedure Load(IniFile: TIniFile); override;
  end;

  TPropertiesFile = class(TInterfacedObject)
  private
    FPath: string;
    FFields: TArray<TPropFieldBase>;
  protected
    function RegisterFields: TArray<TPropFieldBase>; virtual; abstract;

    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; Value: Variant);

    function GetValueStr(Index: Integer): string;
    procedure SetValueStr(Index: Integer; Value: string);

    function GetValueInt(Index: Integer): Integer;
    procedure SetValueInt(Index: Integer; Value: Integer);

    function GetValueBool(Index: Integer): Boolean;
    procedure SetValueBool(Index: Integer; Value: Boolean);

  public
    constructor Create(Path: string);
    destructor Destroy; override;

    procedure Save;
    procedure Load;
  end;

implementation

uses
    System.Variants
  , System.SysUtils
  ;

//______________________________________________________________________________________________________________________

constructor TPropFieldBase.Create(Section, Key: string);
begin
  inherited Create;
  FSection := Section;
  FKey := Key;
  FDefaultValue := Null;
end;

//______________________________________________________________________________________________________________________

constructor TPropFieldBase.Create(Section, Key: string; DefaultValue: Variant);
begin
  inherited Create;
  FSection := Section;
  FKey := Key;
  FDefaultValue := DefaultValue;
end;

//______________________________________________________________________________________________________________________

function TPropFieldBase.GetDefault: Variant;
begin
  if VarIsNull(FDefaultValue) then begin
    Result := GetFallback;
  end
  else begin
    Result := FDefaultValue;
  end;
end;

//______________________________________________________________________________________________________________________

function TStringPropField.GetFallback: Variant;
begin
  Result := '';
end;

procedure TStringPropField.Load(IniFile: TIniFile);
begin
  FValue := IniFile.ReadString(FSection, FKey, GetDefault);
end;

procedure TStringPropField.Save(IniFile: TIniFile);
begin
  IniFile.WriteString(FSection, FKey, FValue);
end;

//______________________________________________________________________________________________________________________

function TBoolPropField.GetFallback: Variant;
begin
  Result := False;
end;

procedure TBoolPropField.Load(IniFile: TIniFile);
begin
  FValue := IniFile.ReadBool(FSection, FKey, GetDefault);
end;

procedure TBoolPropField.Save(IniFile: TIniFile);
begin
  IniFile.WriteBool(FSection, FKey, FValue);
end;

//______________________________________________________________________________________________________________________

constructor TPropertiesFile.Create(Path: string);
begin
  inherited Create;
  FPath := Path;
  FFields := RegisterFields;
end;

//______________________________________________________________________________________________________________________

destructor TPropertiesFile.Destroy;
var
  Field: TPropFieldBase;
begin
  for Field in FFields do begin
    FreeAndNil(Field);
  end;

  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TPropertiesFile.Load;
var
  IniFile: TIniFile;
  Field: TPropFieldBase;
begin
  IniFile := TIniFile.Create(FPath);
  try
    for Field in FFields do begin
      Field.Load(IniFile);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TPropertiesFile.Save;
var
  IniFile: TIniFile;
  Field: TPropFieldBase;
begin
  IniFile := TIniFile.Create(FPath);
  try
    for Field in FFields do begin
      Field.Save(IniFile);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

//______________________________________________________________________________________________________________________

function TPropertiesFile.GetValue(Index: Integer): Variant;
begin
  Result := FFields[Index].Value;
end;

function TPropertiesFile.GetValueBool(Index: Integer): Boolean;
begin
  Result := GetValue(Index);
end;

function TPropertiesFile.GetValueInt(Index: Integer): Integer;
begin
  Result := GetValue(Index);
end;

function TPropertiesFile.GetValueStr(Index: Integer): string;
begin
  Result := GetValue(Index);
end;

//______________________________________________________________________________________________________________________

procedure TPropertiesFile.SetValue(Index: Integer; Value: Variant);
begin
  FFields[Index].Value := Value;
end;

procedure TPropertiesFile.SetValueBool(Index: Integer; Value: Boolean);
begin
  SetValue(Index, Value);
end;

procedure TPropertiesFile.SetValueInt(Index, Value: Integer);
begin
  SetValue(Index, Value);
end;

procedure TPropertiesFile.SetValueStr(Index: Integer; Value: string);
begin
  SetValue(Index, Value);
end;

//______________________________________________________________________________________________________________________

end.
