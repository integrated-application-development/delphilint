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

constructor TLintResources.Create;
begin
  inherited;

  FLoadedPngs := TObjectDictionary<string, TPngImage>.Create;
  FLoadedBitmaps := TObjectDictionary<string, TBitmap>.Create;
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
  Result := LoadBitmap('DL_ICON');
end;

//______________________________________________________________________________________________________________________

function TLintResources.DelphiLintSplash: TBitmap;
begin
  Result := LoadBitmap('DL_SPLASH');
end;

//______________________________________________________________________________________________________________________

function TLintResources.RuleHtmlCss: string;
begin
  Result := LoadFileString('DL_HTML_CSS');
end;

//______________________________________________________________________________________________________________________

function TLintResources.RuleSeverityIcon(RuleType: TRuleSeverity): TGraphic;
const
  CRuleSeverityResourceNames: array[TRuleSeverity] of string = (
    'DL_RS_INFO',
    'DL_RS_MINOR',
    'DL_RS_MAJOR',
    'DL_RS_CRITICAL',
    'DL_RS_BLOCKER'
  );
begin
  Result := LoadPng(CRuleSeverityResourceNames[RuleType]);
end;

//______________________________________________________________________________________________________________________

function TLintResources.RuleTypeIcon(RuleType: TRuleType): TGraphic;
const
  CRuleTypeResourceNames: array[TRuleType] of string = (
    'DL_RT_CODESMELL',
    'DL_RT_BUG',
    'DL_RT_VULNERABILITY',
    'DL_RT_HOTSPOT'
  );
begin
  Result := LoadPng(CRuleTypeResourceNames[RuleType]);
end;

//______________________________________________________________________________________________________________________

function TLintResources.ImpactSeverityIcon(Severity: TImpactSeverity): TGraphic;
const
  CImpactSeverityResourceNames: array[TImpactSeverity] of string = (
    'DL_IS_LOW',
    'DL_IS_MEDIUM',
    'DL_IS_HIGH'
  );
begin
  Result := LoadPng(CImpactSeverityResourceNames[Severity]);
end;

//______________________________________________________________________________________________________________________

function TLintResources.JsLibScript: string;
begin
  Result := LoadFileString('DL_HTML_SCRIPT');
end;

//______________________________________________________________________________________________________________________

function TLintResources.LintStatusIcon(FileStatus: TCurrentFileStatus): TGraphic;
const
  CFileStatusResourceNames: array[TCurrentFileStatus] of string = (
    'DL_LINT_DISABLED',
    'DL_LINT_DISABLED',
    'DL_LINT_WORK',
    'DL_LINT_FAIL',
    'DL_LINT_SUCCESS',
    'DL_LINT_SUCCESSOUT',
    'DL_LINT_WARN',
    'DL_LINT_WARNOUT'
  );
begin
  Result := LoadPng(CFileStatusResourceNames[FileStatus]);
end;

//______________________________________________________________________________________________________________________

function TLintResources.LoadBitmap(ResourceName: string): TBitmap;
begin
  if not FLoadedBitmaps.ContainsKey(ResourceName) then begin
    FLoadedBitmaps.Add(ResourceName, TBitmap.Create);
    FLoadedBitmaps[ResourceName].LoadFromResourceName(HInstance, ResourceName);
  end;

  Result := FLoadedBitmaps[ResourceName];
end;

//______________________________________________________________________________________________________________________

function TLintResources.LoadPng(ResourceName: string): TPngImage;
begin
  if not FLoadedPngs.ContainsKey(ResourceName) then begin
    FLoadedPngs.Add(ResourceName, TPngImage.Create);
    FLoadedPngs[ResourceName].LoadFromResourceName(HInstance, ResourceName);
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
