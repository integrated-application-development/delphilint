{
DelphiLint Client
Copyright (C) 2024 Integrated Application Development

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.Resources;

interface

uses
    System.Generics.Collections
  , Vcl.Graphics
  , Vcl.Imaging.pngimage
  , DelphiLint.Data
  , DelphiLint.ToolFrame
  ;

type
  TLintResources = class(TObject)
  private
    FLoadedPngs: TObjectDictionary<string, TPngImage>;
    FLoadedBitmaps: TObjectDictionary<string, TBitmap>;
    FLoadedStrings: TDictionary<string, string>;

    function LoadPng(ResourceName: string): TPngImage;
    function LoadBitmap(ResourceName: string): TBitmap;
    function LoadFileString(ResourceName: string): string;
    function AssetName(Path: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function LintStatusIcon(FileStatus: TCurrentFileStatus): TGraphic;
    function RuleTypeIcon(RuleType: TRuleType): TGraphic;
    function RuleSeverityIcon(RuleType: TRuleSeverity): TGraphic;
    function ImpactSeverityIcon(Severity: TImpactSeverity): TGraphic;
    function DelphiLintIcon: TBitmap;
    function DelphiLintSplash: TBitmap;

    function JsLibScript: string;
    function RuleHtmlCss: string;
  end;

function LintResources: TLintResources;

implementation

uses
    System.SysUtils
  , System.Classes
  , System.Types
  , System.Hash
  ;

var
  G_LintResources: TLintResources;

//______________________________________________________________________________________________________________________

function LintResources: TLintResources;
begin
  if not Assigned(G_LintResources) then begin
    G_LintResources := TLintResources.Create;
  end;

  Result := G_LintResources;
end;

//______________________________________________________________________________________________________________________

function TLintResources.AssetName(Path: string): string;
begin
  Result := 'DL_ASSET_' + UpperCase(THashSHA2.GetHashString(UpperCase(Path)));
end;

//______________________________________________________________________________________________________________________

constructor TLintResources.Create;
begin
  inherited;

  FLoadedPngs := TObjectDictionary<string, TPngImage>.Create([doOwnsValues]);
  FLoadedBitmaps := TObjectDictionary<string, TBitmap>.Create([doOwnsValues]);
  FLoadedStrings := TDictionary<string, string>.Create;
end;

//______________________________________________________________________________________________________________________

destructor TLintResources.Destroy;
begin
  FreeAndNil(FLoadedPngs);
  FreeAndNil(FLoadedBitmaps);
  FreeAndNil(FLoadedStrings);
  inherited;
end;

//______________________________________________________________________________________________________________________

function TLintResources.DelphiLintIcon: TBitmap;
begin
  Result := LoadBitmap(AssetName('delphilint_icon_48x48.bmp'));
end;

//______________________________________________________________________________________________________________________

function TLintResources.DelphiLintSplash: TBitmap;
begin
  Result := LoadBitmap(AssetName('delphilint_icon_24x24.bmp'));
end;

//______________________________________________________________________________________________________________________

function TLintResources.RuleHtmlCss: string;
begin
  Result := LoadFileString(AssetName('rulehtml.css'));
end;

//______________________________________________________________________________________________________________________

function TLintResources.RuleSeverityIcon(RuleType: TRuleSeverity): TGraphic;
const
  CRuleSeverityResourceNames: array[TRuleSeverity] of string = (
    'sonar_icon\severity\info.png',
    'sonar_icon\severity\minor.png',
    'sonar_icon\severity\major.png',
    'sonar_icon\severity\critical.png',
    'sonar_icon\severity\blocker.png'
  );
begin
  Result := LoadPng(AssetName(CRuleSeverityResourceNames[RuleType]));
end;

//______________________________________________________________________________________________________________________

function TLintResources.RuleTypeIcon(RuleType: TRuleType): TGraphic;
const
  CRuleTypeResourceNames: array[TRuleType] of string = (
    'sonar_icon\type\code_smell.png',
    'sonar_icon\type\bug.png',
    'sonar_icon\type\vulnerability.png',
    'sonar_icon\type\hotspot.png'
  );
begin
  Result := LoadPng(AssetName(CRuleTypeResourceNames[RuleType]));
end;

//______________________________________________________________________________________________________________________

function TLintResources.ImpactSeverityIcon(Severity: TImpactSeverity): TGraphic;
const
  CImpactSeverityResourceNames: array[TImpactSeverity] of string = (
    'sonar_icon\impact\low.png',
    'sonar_icon\impact\medium.png',
    'sonar_icon\impact\high.png'
  );
begin
  Result := LoadPng(AssetName(CImpactSeverityResourceNames[Severity]));
end;

//______________________________________________________________________________________________________________________

function TLintResources.JsLibScript: string;
begin
  Result := LoadFileString('DL_GENERATED_JS');
end;

//______________________________________________________________________________________________________________________

function TLintResources.LintStatusIcon(FileStatus: TCurrentFileStatus): TGraphic;
const
  CFileStatusResourceNames: array[TCurrentFileStatus] of string = (
    'status_icon\lint_disabled.png',
    'status_icon\lint_disabled.png',
    'status_icon\lint_work.png',
    'status_icon\lint_fail.png',
    'status_icon\lint_success.png',
    'status_icon\lint_success_outdated.png',
    'status_icon\lint_warn.png',
    'status_icon\lint_warn_outdated.png'
  );
begin
  Result := LoadPng(AssetName(CFileStatusResourceNames[FileStatus]));
end;

//______________________________________________________________________________________________________________________

function TLintResources.LoadBitmap(ResourceName: string): TBitmap;
var
  Stream: TResourceStream;
begin
  if not FLoadedBitmaps.ContainsKey(ResourceName) then begin
    Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      FLoadedBitmaps.Add(ResourceName, TBitmap.Create);
      FLoadedBitmaps[ResourceName].LoadFromStream(Stream);
    finally
      FreeAndNil(Stream);
    end;
  end;

  Result := FLoadedBitmaps[ResourceName];
end;

//______________________________________________________________________________________________________________________

function TLintResources.LoadPng(ResourceName: string): TPngImage;
var
  Stream: TResourceStream;
begin
  if not FLoadedPngs.ContainsKey(ResourceName) then begin
    Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      FLoadedPngs.Add(ResourceName, TPngImage.Create);
      FLoadedPngs[ResourceName].LoadFromStream(Stream);
    finally
      FreeAndNil(Stream);
    end;
  end;

  Result := FLoadedPngs[ResourceName];
end;

//______________________________________________________________________________________________________________________

function TLintResources.LoadFileString(ResourceName: string): string;
var
  Stream: TResourceStream;
  StreamReader: TStreamReader;
begin
  if not FLoadedStrings.ContainsKey(ResourceName) then begin
    Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      StreamReader := TStreamReader.Create(Stream, True);
      FLoadedStrings.Add(ResourceName, StreamReader.ReadToEnd);
    finally
      FreeAndNil(StreamReader);
      FreeAndNil(Stream);
    end;
  end;

  Result := FLoadedStrings[ResourceName];
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(G_LintResources);

end.
