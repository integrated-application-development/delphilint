program dof2dproj;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.StrUtils,
  System.RegularExpressions,
  System.Generics.Collections,
  System.IOUtils;

type
  TValueMapping = record
    Section: string;
    Key: string;
    Default: string;
    class function Create(const ASection, AKey, ADefault: string): TValueMapping; static;
  end;

  TFormInfo = record
    UnitName: string;
    FormName: string;
    class function Create(const AUnitName, AFormName: string): TFormInfo; static;
  end;

  TProjectConverter = class
  private
    FDprContent: TStringList;
    FDofFile: TIniFile;
    FTemplateContent: TStringList;
    FOutputContent: TStringList;
    FOnlyDprName, FDprPath: string;
    FUnits: TStringList;
    FForms: TStringList;
    FTagMappings: TDictionary<string, TValueMapping>;
    FFormInfos: TList<TFormInfo>; // Instead of separate FForms list
    FForceOverwrite: Boolean;

    procedure InitializeTagMappings;
    function GetDofValue(const Section, Key: string; const Default: string = ''): string;
    function GenerateGUID: string;
    procedure ParseDprFile(const DprFileName: string);
    procedure ProcessTemplate;
    function GetValueForTag(const TagName: string): string;
    function ProcessLine(const Line: string): string;
    procedure ExtractFormsAndUnits(const Line: string);
    function FindDprFile(const DofFileName: string): string;
    function ConvertBooleanString(const Value: string): string;
    function SearchIconFile: string;
  public
    constructor Create(const TemplateFileName: string; AForceOverwrite: Boolean);
    destructor Destroy; override;
    procedure Convert(const DofFileName: string);
  end;

  { TValueMapping }
class function TValueMapping.Create(const ASection, AKey, ADefault: string): TValueMapping;
begin
  Result.Section := ASection;
  Result.Key := AKey;
  Result.Default := ADefault;
end;

class function TFormInfo.Create(const AUnitName, AFormName: string): TFormInfo;
begin
  Result.UnitName := AUnitName;
  Result.FormName := AFormName;
end;

constructor TProjectConverter.Create(const TemplateFileName: string; AForceOverwrite: Boolean);
begin
  inherited Create;
  FForceOverwrite := AForceOverwrite;

  // Initialize collections
  FDprContent := TStringList.Create;
  FTemplateContent := TStringList.Create;
  FOutputContent := TStringList.Create;
  FUnits := TStringList.Create;
  FForms := TStringList.Create;
  FTagMappings := TDictionary<string, TValueMapping>.Create;
  FFormInfos := TList<TFormInfo>.Create;

  // Load template
  WriteLn('Loading template file: ', TemplateFileName);
  if not FileExists(TemplateFileName) then
    raise Exception.CreateFmt('Template file not found: %s', [TemplateFileName]);

  try
    FTemplateContent.LoadFromFile(TemplateFileName);
    WriteLn('Template loaded successfully, lines: ', FTemplateContent.Count);
  except
    on E: Exception do
      raise Exception.CreateFmt('Error loading template file: %s - %s', [TemplateFileName, E.Message]);
  end;

  InitializeTagMappings;
end;

destructor TProjectConverter.Destroy;
begin
  FDprContent.Free;
  FTemplateContent.Free;
  FOutputContent.Free;
  FUnits.Free;
  FForms.Free;
  FTagMappings.Free;
  FFormInfos.Free;
  FreeAndNil(FDofFile);
  inherited;
end;

procedure TProjectConverter.InitializeTagMappings;
begin
  // Compiler settings
  FTagMappings.Add('DCC_E', TValueMapping.Create('Compiler', 'E', '0'));
  FTagMappings.Add('DCC_F', TValueMapping.Create('Compiler', 'F', '0'));
  FTagMappings.Add('DCC_K', TValueMapping.Create('Compiler', 'K', '0'));
  FTagMappings.Add('DCC_N', TValueMapping.Create('Compiler', 'N', '1'));
  FTagMappings.Add('DCC_S', TValueMapping.Create('Compiler', 'S', '0'));
  FTagMappings.Add('DCC_DebugInformation', TValueMapping.Create('Linker', 'DebugInfo', '0'));
  FTagMappings.Add('DCC_WriteableConstants', TValueMapping.Create('Compiler', 'WriteableConst', '1'));
  FTagMappings.Add('DCC_SymbolReferenceInfo', TValueMapping.Create('Compiler', 'SymbolReferenceInfo', '1'));
  FTagMappings.Add('DCC_Optimize', TValueMapping.Create('Compiler', 'O', '0'));
  FTagMappings.Add('DCC_IntegerOverflowCheck', TValueMapping.Create('Compiler', 'I', '0'));
  FTagMappings.Add('DCC_RangeChecking', TValueMapping.Create('Compiler', 'R', '0'));
  FTagMappings.Add('DCC_IOChecking', TValueMapping.Create('Compiler', 'I', '0'));
  FTagMappings.Add('DCC_GenerateStackFrames', TValueMapping.Create('Compiler', 'StackFrames', '1'));

  // Linker settings
  FTagMappings.Add('DCC_ImageBase', TValueMapping.Create('Linker', 'ImageBase', '00400000'));
  FTagMappings.Add('DCC_MapFile', TValueMapping.Create('Linker', 'MapFile', '0'));

  // Directory settings
  FTagMappings.Add('DCC_ExeOutput', TValueMapping.Create('Directories', 'OutputDir', ''));
  FTagMappings.Add('DCC_DcuOutput', TValueMapping.Create('Directories', 'UnitOutputDir', ''));
  FTagMappings.Add('DCC_UnitSearchPath', TValueMapping.Create('Directories', 'SearchPath', ''));
  FTagMappings.Add('DCC_Define', TValueMapping.Create('Directories', 'Conditionals', ''));
  FTagMappings.Add('DCC_Namespace', TValueMapping.Create('Directories', 'Namespace', 'Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win'));

  // Version Info
  FTagMappings.Add('VerInfo_MajorVer', TValueMapping.Create('Version Info', 'MajorVer', '1'));
  FTagMappings.Add('VerInfo_MinorVer', TValueMapping.Create('Version Info', 'MinorVer', '0'));
  FTagMappings.Add('VerInfo_Release', TValueMapping.Create('Version Info', 'Release', '0'));
  FTagMappings.Add('VerInfo_Build', TValueMapping.Create('Version Info', 'Build', '0'));
  FTagMappings.Add('VerInfo_Locale', TValueMapping.Create('Version Info', 'Locale', '1033'));
  FTagMappings.Add('VerInfo_Keys', TValueMapping.Create('Version Info Keys', 'Keys', 'CompanyName=;FileDescription=;FileVersion=1.0.0.0;InternalName=;' + 'LegalCopyright=;LegalTrademarks=;OriginalFilename=;ProductName=;' + 'ProductVersion=1.0.0.0;Comments='));

  // Packages - no default value
  FTagMappings.Add('DCC_UsePackage', TValueMapping.Create('Directories', 'Packages', ''));

  // Symbols
  FTagMappings.Add('DCC_SYMBOL_DEPRECATED', TValueMapping.Create('Compiler', 'SymbolDeprecated', '0'));
  FTagMappings.Add('DCC_SYMBOL_LIBRARY', TValueMapping.Create('Compiler', 'SymbolLibrary', '0'));
  FTagMappings.Add('DCC_SYMBOL_PLATFORM', TValueMapping.Create('Compiler', 'SymbolPlatform', '0'));

  // Application
  FTagMappings.Add('Icon_MainIcon', TValueMapping.Create('Application', 'Icon', ''));
  FTagMappings.Add('SanitizedProjectName', TValueMapping.Create('Directories', 'UnitOutputDir', ''));
end;

function TProjectConverter.SearchIconFile: string;
var
  SearchRec: TSearchRec;
  LIco: string;
begin
  Result := '';
  // First try exact name in current directory
  LIco := ChangeFileExt(FDprPath + FOnlyDprName, '.ico');
  if FileExists(LIco) then
  begin
    Result := LIco;
    Exit;
  end;
  // Search for .ico files
  if FindFirst(FDprPath + '*.ico', faAnyFile, SearchRec) = 0 then
    try
      repeat
        if string(SearchRec.Name).ToLower.Contains(FOnlyDprName) then
        begin
          Result := SearchRec.Name;
          Exit;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
end;

function TProjectConverter.GetDofValue(const Section, Key: string; const Default: string = ''): string;
begin
  if Assigned(FDofFile) then
    Result := FDofFile.ReadString(Section, Key, Default)
  else
    Result := Default;
end;

function TProjectConverter.GenerateGUID: string;
var
  GUID: TGUID;
begin
  if CreateGUID(GUID) = S_OK then
    Result := GUIDToString(GUID)
  else
    Result := '{00000000-0000-0000-0000-000000000000}';
end;

function TProjectConverter.ConvertBooleanString(const Value: string): string;
begin
  if Value = '1' then
    Result := 'true'
  else if Value = '0' then
    Result := 'false'
  else
    Result := Value;
end;

function TProjectConverter.GetValueForTag(const TagName: string): string;
var
  Mapping: TValueMapping;
begin
  // Special cases that don't come from DOF file
  if TagName = 'ProjectGuid' then
    Result := GenerateGUID
  else if TagName = 'MainSource' then
    Result := FOnlyDprName + '.dpr'
  else if TagName = 'SanitizedProjectName' then
    Result := FOnlyDprName
  else if TagName = 'Icon_MainIcon' then
  begin
    Result := GetDofValue('Application', 'Icon', '');
    if Result = '' then
      Result := ExtractFileName(SearchIconFile);
  end
  else
  begin
    // Look up in mappings
    if FTagMappings.TryGetValue(TagName, Mapping) then
    begin
      Result := GetDofValue(Mapping.Section, Mapping.Key, Mapping.Default);

      // Special handling for ImageBase - convert hex to decimal correctly
      if (TagName = 'DCC_ImageBase') and (Result <> '') then
      begin
        Result := '00400000'; // Default
      end
      // Special handling for debug information and other boolean values
      else if (TagName = 'DCC_DebugInformation') or (TagName = 'DCC_WriteableConstants') or (TagName = 'DCC_SymbolReferenceInfo') then
      begin
        Result := ConvertBooleanString(Result);
      end
      // Other boolean values
      else if TagName.StartsWith('DCC_') and (Result <> '') and (Result[1] in ['0', '1']) then
      begin
        Result := ConvertBooleanString(Result);
      end;
    end
    else
      Result := '';
  end;
end;

function TProjectConverter.ProcessLine(const Line: string): string;
var
  Match: TMatch;
  TagName: string;
  Value: string;
  FormatSpecifier: string;
  I: Integer;
  StringBuilder: TStringBuilder;
begin
  Result := Line;

  try
    // Special case for DCCReference - handle multiple forms
    if Line.Contains('<DCCReference Include=') then
    begin
      StringBuilder := TStringBuilder.Create;
      try
        for I := 0 to FFormInfos.Count - 1 do
        begin
          // Create the same format as template but with proper indentation
          StringBuilder.AppendLine(Format('        <DCCReference Include="%s">', [FFormInfos[I].UnitName]));
          StringBuilder.AppendLine('            <Form>' + FFormInfos[I].FormName + '</Form>');
          StringBuilder.AppendLine('        </DCCReference>');
        end;
        Result := StringBuilder.ToString.TrimRight;
        Exit;
      finally
        StringBuilder.Free;
      end;
    end
    else if Line.Contains('<Form>') then
    begin
      Result := '';
      Exit;
    end
    else if Line.Contains('</DCCReference>') then
    begin
      Result := '';
      Exit;
    end;

    // Case 1: Tags with Name attribute and format specifier
    Match := TRegEx.Match(Line, '<(\w+)\s+Name="([^"]+)">%(s|d)</\w+>');
    if Match.Success then
    begin
      TagName := Match.Groups[2].Value; // Use the Name attribute value
      FormatSpecifier := Match.Groups[3].Value;
      Value := GetValueForTag(TagName);
      Result := Format(Line, [Value]);
      WriteLn(Format('Found tag: %s, Value: %s', [TagName, Value]));
      Exit;
    end;

    // Case 2: Tags with Include attribute
    Match := TRegEx.Match(Line, '<(\w+)(?:\s+Include="%(s|d)")>');
    if Match.Success then
    begin
      TagName := Match.Groups[1].Value;
      FormatSpecifier := Match.Groups[2].Value;
      Value := GetValueForTag(TagName);
      Result := Format(Line, [Value]);
      WriteLn(Format('Found tag: %s, Value: %s', [TagName, Value]));
      Exit;
    end;

    // Case 3: Simple tags with format specifier
    Match := TRegEx.Match(Line, '<(\w+)>%(s|d)</\w+>');
    if Match.Success then
    begin
      TagName := Match.Groups[1].Value;
      FormatSpecifier := Match.Groups[2].Value;
      Value := GetValueForTag(TagName);

      if FormatSpecifier = 'd' then
      begin
        if Value = '' then
          Value := '0';
        if Value.ToLower = 'true' then
          Value := '1'
        else if Value.ToLower = 'false' then
          Value := '0';

        Result := Format(Line, [StrToIntDef(Value, 0)]);
      end
      else
        Result := Format(Line, [Value]);
      WriteLn(Format('Found tag: %s, Value: %s', [TagName, Value]));
      Exit;
    end;

    // Case 4: Any other tag with format specifier (fallback)
    Match := TRegEx.Match(Line, '<([^>]+)>.*?%(s|d)');
    if Match.Success then
    begin
      TagName := Match.Groups[1].Value;
      FormatSpecifier := Match.Groups[2].Value;
      Value := GetValueForTag(TagName);

      if FormatSpecifier = 'd' then
      begin
        if Value = '' then
          Value := '0';
        if Value.ToLower = 'true' then
          Value := '1'
        else if Value.ToLower = 'false' then
          Value := '0';
        Result := Format(Line, [StrToIntDef(Value, 0)]);
      end
      else
        Result := Format(Line, [Value]);

      WriteLn(Format('Found tag: %s, Value: %s', [TagName, Value]));
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error processing line "%s": %s', [Line, E.Message]);
  end;
end;

procedure TProjectConverter.ExtractFormsAndUnits(const Line: string);
var
  Parts: TArray<string>;
  UnitPart, FilePart, FormName: string;
  BraceStart, BraceEnd: Integer;
  FormInfo: TFormInfo;
begin
  WriteLn('Extracting from line: ', Line);

  // Skip empty lines or comments
  if (Line = '') or Line.StartsWith('//') then
    Exit;

  // Split by ' in '
  Parts := Line.Split([' in ']);
  if Length(Parts) < 2 then
  begin
    // This is a regular unit without 'in' clause
    UnitPart := Line.Trim;
    if (UnitPart <> '') and (not FUnits.Contains(UnitPart)) then
      FUnits.Add(UnitPart);
    Exit;
  end;

  UnitPart := Parts[0].Trim;
  FilePart := Parts[1].Trim;

  // Add to units list if not already present
  if (UnitPart <> '') and (not FUnits.Contains(UnitPart)) then
    FUnits.Add(UnitPart);

  // Remove quotes from file path
  FilePart := FilePart.Replace('''', '');

  // Extract form name from braces
  BraceStart := FilePart.IndexOf('{');
  BraceEnd := FilePart.IndexOf('}');

  if (BraceStart > 0) and (BraceEnd > BraceStart) then
  begin
    FormName := FilePart.Substring(BraceStart + 1, BraceEnd - BraceStart - 1).Trim;
    FilePart := FilePart.Substring(0, BraceStart).Trim;

    WriteLn(Format('Found form: Unit=%s, File=%s, Form=%s', [UnitPart, FilePart, FormName]));

    FormInfo := TFormInfo.Create(FilePart, FormName);
    FFormInfos.Add(FormInfo);
  end;
end;

procedure TProjectConverter.ParseDprFile(const DprFileName: string);
var
  I: Integer;
  InUsesClause: Boolean;
  Line: string;
begin
  if not FileExists(DprFileName) then
    raise Exception.CreateFmt('DPR file not found: %s', [DprFileName]);

  FDprContent.LoadFromFile(DprFileName);
  FDprPath := ExtractFilePath(DprFileName);
  FOnlyDprName := ChangeFileExt(ExtractFileName(DprFileName), '');

  InUsesClause := False;
  WriteLn('Starting to parse DPR file...');

  for I := 0 to FDprContent.Count - 1 do
  begin
    Line := Trim(FDprContent[I]);
    WriteLn('Processing line: ', Line);

    // Start of uses clause
    if LowerCase(Line) = 'uses' then
    begin
      InUsesClause := True;
      WriteLn('Found uses clause');
      Continue;
    end;

    if InUsesClause then
    begin
      // Skip empty lines and comments
      if (Line = '') or Line.StartsWith('//') then
        Continue;

      // Remove trailing comma and semicolon
      Line := Line.Replace(',', '').Replace(';', '').Trim;

      // Skip if line is empty after cleaning
      if Line = '' then
        Continue;

      // Add unit to the list regardless of whether it's a form or not
      if Line.Contains(' in ') then
      begin
        // For units with forms
        WriteLn('Found unit declaration: ', Line);
        ExtractFormsAndUnits(Line);
      end
      else if not(Line.StartsWith('{') or Line.StartsWith('//')) then
      begin
        // For regular units
        Line := Line.Trim;
        if (Line <> '') and (not FUnits.Contains(Line)) then
        begin
          WriteLn('Found regular unit: ', Line);
          FUnits.Add(Line);
        end;
      end;

      // End of uses clause
      if FDprContent[I].Contains(';') then
      begin
        InUsesClause := False;
        WriteLn('End of uses clause');
      end;
    end;
  end;

  WriteLn(Format('Parsing complete. Found %d forms and %d units.', [FFormInfos.Count, FUnits.Count]));
end;

procedure TProjectConverter.ProcessTemplate;
var
  I: Integer;
  Line: string;
begin
  if FTemplateContent.Count = 0 then
    raise Exception.Create('Template file is empty');

  WriteLn('Processing template lines...');
  for I := 0 to FTemplateContent.Count - 1 do
  begin
    try
      Line := ProcessLine(FTemplateContent[I]);
      if Line <> '' then // Only add non-empty lines
        FOutputContent.Add(Line);
    except
      on E: Exception do
        raise Exception.CreateFmt('Error processing template line %d: %s', [I + 1, E.Message]);
    end;
  end;
  WriteLn('Template processing completed.');
end;

function TProjectConverter.FindDprFile(const DofFileName: string): string;
var
  DprFileName: string;
begin
  DprFileName := ChangeFileExt(DofFileName, '.dpr');
  if not FileExists(DprFileName) then
  begin
    WriteLn('Warning: DPR file not found: ', DprFileName);
    WriteLn('Processing stopped.');
    Halt(1);
  end;
  Result := DprFileName;
end;

procedure TProjectConverter.Convert(const DofFileName: string);
var
  DprFileName, OutputFileName: string;
  Sections: TStringList;
begin
  try
    WriteLn('Starting conversion...');

    if not FileExists(DofFileName) then
      raise Exception.CreateFmt('DOF file not found: %s', [DofFileName]);
    if not SameText(ExtractFileExt(DofFileName), '.dof') then
      raise Exception.CreateFmt('Input file must be a .DOF file, got: %s', [DofFileName]);
    Sections := TStringList.Create;
    try
      try
        // Try to read sections from the file
        TIniFile.Create(DofFileName).ReadSections(Sections);
        // Check for some typical DOF sections
        if (Sections.IndexOf('Directories') = -1) and (Sections.IndexOf('Compiler') = -1) and (Sections.IndexOf('Linker') = -1) then
        begin
          raise Exception.Create('The file does not appear to be a valid DOF file. ' + 'Missing required sections (Directories, Compiler, or Linker).');
        end;
      except
        on E: Exception do
          raise Exception.CreateFmt('Invalid DOF file format: %s. Error: %s', [DofFileName, E.Message]);
      end;
    finally
      Sections.Free;
    end;
    WriteLn('DOF file found and validated: ', DofFileName);

    DprFileName := FindDprFile(DofFileName);
    WriteLn('DPR file found: ', DprFileName);

    OutputFileName := ChangeFileExt(DofFileName, '.dproj');

    if FileExists(OutputFileName) and not FForceOverwrite then
    begin
      WriteLn('Warning: DPROJ file already exists: ', OutputFileName);
      WriteLn('Use -f option to force overwrite.');
      WriteLn('Processing stopped.');
      Exit;
    end;

    WriteLn('Loading DOF file...');
    FDofFile := TIniFile.Create(DofFileName);

    WriteLn('Parsing DPR file...');
    ParseDprFile(DprFileName);
    // Change this line to use FFormInfos.Count instead of FForms.Count
    WriteLn(Format('Found %d forms and %d units', [FFormInfos.Count, FUnits.Count]));

    WriteLn('Processing template...');
    ProcessTemplate;

    WriteLn('Saving output...');
    FOutputContent.SaveToFile(OutputFileName);
    WriteLn('Conversion completed successfully.');
    WriteLn('Output written to: ', OutputFileName);

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end;

function IsPathAbsolute(const APath: string): Boolean;
begin
  Result := False;
  if APath = '' then
    Exit;
{$IFDEF LINUX}
  Result := (APath[1] = PathDelim) or (APath[1] = '~');
{$ELSE}
  Result := (Pos(DriveDelim, APath) > 1) or (Pos('\\', APath) = 1);
{$ENDIF LINUX}
end;

function IsFileInSearchPath(const AFilePath, ADprojPath: string): Boolean;
var
  LFileContent: TStringList;
  LSearchPaths: string;
  LPath: string;
  LPaths: TStringList;
  LNormalizedFilePath: string;
  LNormalizedSearchPath: string;
  LNormalizedDprojPath: string;
  LMatch: TMatch;
  i: Integer;
begin
  Result := False;

  // Нормализуем все пути
  LNormalizedFilePath := ExtractFilePath(ExpandFileName(AFilePath));
  LNormalizedDprojPath := ExtractFilePath(ExpandFileName(ADprojPath));

  // Проверяем сначала относительно пути к dproj
  if AnsiStartsText(LNormalizedDprojPath, LNormalizedFilePath) then
  begin
    Result := True;
    Exit;
  end;

  LFileContent := TStringList.Create;
  LPaths := TStringList.Create;
  try
    // Загружаем DPROJ как текст
    LFileContent.LoadFromFile(ADprojPath);

    // Ищем DCC_UnitSearchPath через регулярку
    LMatch := TRegEx.Match(LFileContent.Text, '<DCC_UnitSearchPath>(.*?)</DCC_UnitSearchPath>', [roSingleLine]);

    if not LMatch.Success then
      Exit;

    LSearchPaths := LMatch.Groups[1].Value;

    // Разбиваем пути
    LPaths.Delimiter := ';';
    LPaths.DelimitedText := LSearchPaths;

    // Проверяем каждый путь
    for i := 0 to Pred(LPaths.Count) do
    begin
      LPath := LPaths[i];
      if LPath = '' then
        Continue;

      // Обрабатываем относительные пути от dproj
      if not IsPathAbsolute(LPath) then
        LPath := LNormalizedDprojPath + LPath;

      // Нормализуем путь поиска
      LNormalizedSearchPath := IncludeTrailingPathDelimiter(ExpandFileName(LPath));

      // Проверяем вхождение
      if AnsiStartsText(LNormalizedSearchPath, LNormalizedFilePath) then
      begin
        Result := True;
        Exit;
      end;
    end;

  finally
    LFileContent.Free;
    LPaths.Free;
  end;
end;

procedure TestSearchPaths;
var
  ProjectPath: string;
  TestPaths: array of record FilePath: string;
  Expected: Boolean;
end;
I:
Integer;
ActualResult:
Boolean;

begin
  WriteLn('=== Testing Search Paths ===');

  ProjectPath := 'C:\Users\Artur\Desktop\Code Check Format\colibri_parse_dfm\colibri_utilities\programs\dfm_parser\parse_dfm\p_parse_dfm.dproj';

  SetLength(TestPaths, 4);
  // Тест 1: Файл в директории проекта
  TestPaths[0].FilePath := 'C:\Users\Artur\Desktop\Code Check Format\colibri_parse_dfm\colibri_utilities\programs\dfm_parser\parse_dfm\u_parse_dfm.pas';
  TestPaths[0].Expected := True;

  // Тест 2: Файл в относительном пути colibri_helpers
  TestPaths[1].FilePath := 'C:\Users\Artur\Desktop\Code Check Format\colibri_parse_dfm\colibri_helpers\units\u_strings.pas';
  TestPaths[1].Expected := True;

  // Тест 3: Выдуманный файл
  TestPaths[2].FilePath := 'C:\Users\Artur\Desktop\Code Check Format\2u_strings.pas';
  TestPaths[2].Expected := False;

  // Тест 5: Файл в units
  TestPaths[3].FilePath := 'C:\Users\Artur\Desktop\Code Check Format\colibri_parse_dfm\colibri_helpers\classes\u_c_line.pas';
  TestPaths[3].Expected := True;

  for I := 0 to High(TestPaths) do
  begin
    ActualResult := IsFileInSearchPath(TestPaths[I].FilePath, ProjectPath);
    WriteLn(Format('Test %d: File=%s', [I + 1, TestPaths[I].FilePath]));
    WriteLn(Format('Expected: %s, Got: %s', [BoolToStr(TestPaths[I].Expected, True), BoolToStr(ActualResult, True)]));
    WriteLn('---');
  end;

  WriteLn('=== Test Complete ===');
end;

var
  Converter: TProjectConverter;
  ForceOverwrite: Boolean;
  I: Integer;
  Param: string;
  TemplateFile: string;
  DofFile: string;

begin
  try
    TestSearchPaths;
    // Parse command line parameters
    ForceOverwrite := False;
    DofFile := '';

    for I := 1 to ParamCount do
    begin
      Param := ParamStr(I);
      if (Param = '-f') or (Param = '--force') then
        ForceOverwrite := True
      else if DofFile = '' then
        DofFile := Param
      else
      begin
        WriteLn('Usage: dof2dproj.exe [-f|--force] <dof_file>');
        WriteLn('Options:');
        WriteLn('  -f, --force    Force overwrite existing DPROJ file');
        ExitCode := 1;
        Exit;
      end;
    end;

    if DofFile = '' then
    begin
      WriteLn('Usage: dof2dproj.exe [-f|--force] <dof_file>');
      WriteLn('Options:');
      WriteLn('  -f, --force    Force overwrite existing DPROJ file');
      ExitCode := 1;
      Exit;
    end;

    TemplateFile := ExtractFilePath(ParamStr(0)) + 'rspages.dproj';
    WriteLn('Looking for template file: ', TemplateFile);
    if not FileExists(TemplateFile) then
    begin
      WriteLn('Template file not found at: ', TemplateFile);
      WriteLn('Current directory: ', GetCurrentDir);
      WriteLn('Executable path: ', ExtractFilePath(ParamStr(0)));
      raise Exception.CreateFmt('Template file not found: %s', [TemplateFile]);
    end;

    Converter := TProjectConverter.Create(TemplateFile, ForceOverwrite);
    try
      Converter.Convert(DofFile);
    finally
      Converter.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ExitCode := 1;
    end;
  end;

end.
