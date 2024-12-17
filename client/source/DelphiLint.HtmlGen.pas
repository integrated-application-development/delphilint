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
unit DelphiLint.HtmlGen;

interface

uses
    System.Classes
  , System.Generics.Collections
  , Vcl.Graphics
  , DelphiLint.Data
  ;

type
  THtmlUtils = class(TObject)
  public
    class function ColorToHex(Color: TColor): string;
    class function WrapHtml(Html: string; WrappingTag: string): string;
    class function ImageToBase64(Image: TGraphic): string;
  end;

  TRuleHtmlGenerator = class(TObject)
  private
    FTextColor: string;
    FBgColor: string;
    FScrollbarColor: string;
    FLinkColor: string;
    FHoverTabColor: string;
    FActiveTabColor: string;
    FTempPath: string;
    FJsPath: string;
    FCssPath: string;
    FLocks: TObjectList<TFileStream>;

    function BuildHtmlPage(BodyHtml: string; BodyClass: string = ''): string;
    procedure CreateReadOnlyFile(Path: string; Text: string);

    function GenerateRuleDescriptionHtml(Description: TRuleDescription): string;
  public
    class function GetRuleTypeStr(RuleType: TRuleType): string;
    class function GetCleanCodeAttributeStr(Attribute: TCleanCodeAttribute): string;
    class function GetCleanCodeCategoryStr(Category: TCleanCodeAttributeCategory): string;
    class function GetSoftwareQualityStr(Quality: TSoftwareQuality): string;
    class function GetImpactSeverityClassName(Severity: TImpactSeverity): string;
    class function GetRuleSeverityClassName(Severity: TRuleSeverity): string;
    class function GetRuleTypeTooltip(RuleType: TRuleType; Severity: TRuleSeverity): string;
    class function GetImpactTooltip(Quality: TSoftwareQuality; Severity: TImpactSeverity): string;
    class function GetAttributeTooltip(Attribute: TCleanCodeAttribute): string;

    constructor Create;
    destructor Destroy; override;
    procedure UpdateColors;

    function GenerateHtmlText(Rule: TRule): string;
    function GenerateHtmlFile(Rule: TRule): string;
    function GenerateCss: string;
  end;

implementation

uses
    System.SysUtils
  , System.StrUtils
  , System.IOUtils
  , System.NetEncoding
  , Winapi.Windows
  , DelphiLint.Context
  , DelphiLint.Resources
  ;

//______________________________________________________________________________________________________________________

class function THtmlUtils.ImageToBase64(Image: TGraphic): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Image.SaveToStream(Stream);
    Result := Format('data:image/png;base64,%s', [TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size)]);
  finally
    FreeAndNil(Stream);
  end;
end;

//______________________________________________________________________________________________________________________

class function THtmlUtils.ColorToHex(Color: TColor): string;
begin
  Color := ColorToRGB(Color);
  Result := '#' +
    IntToHex(GetRValue(Color), 2) +
    IntToHex(GetGValue(Color), 2) +
    IntToHex(GetBValue(Color), 2);
end;

//______________________________________________________________________________________________________________________

class function THtmlUtils.WrapHtml(Html: string; WrappingTag: string): string;
var
  OpeningTag: string;
  ClosingTag: string;
begin
  OpeningTag := Format('<%s>', [WrappingTag]);
  Result := Trim(Html);

  if not StartsText(OpeningTag, Result) then begin
    ClosingTag := Format('</%s>', [WrappingTag]);
    Result := Format('%s%s%s', [OpeningTag, Result, ClosingTag]);
  end;
end;

//______________________________________________________________________________________________________________________

constructor TRuleHtmlGenerator.Create;
begin
  inherited Create;
  FLocks := TObjectList<TFileStream>.Create;

  UpdateColors;

  FTempPath := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempPath);
  FJsPath := TPath.Combine(FTempPath, 'script.js');
  CreateReadOnlyFile(FJsPath, LintResources.JsLibScript);
  FCssPath := TPath.Combine(FTempPath, 'style.css');
  CreateReadOnlyFile(FCssPath, LintResources.RuleHtmlCss);
end;

//______________________________________________________________________________________________________________________

destructor TRuleHtmlGenerator.Destroy;
begin
  FreeAndNil(FLocks);
  TDirectory.Delete(FTempPath, True);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGenerator.CreateReadOnlyFile(Path: string; Text: string);
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(Text);

  Stream := TFileStream.Create(Path, fmCreate or fmShareDenyWrite);
  FLocks.Add(Stream);
  Stream.WriteBuffer(Bytes, Length(Bytes));
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGenerator.UpdateColors;
begin
  FTextColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnText));
  FBgColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clWindow));
  FScrollbarColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnFace));
  FLinkColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clHotLight));
  FHoverTabColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnFace));
  FActiveTabColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnHighlight));
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateHtmlFile(Rule: TRule): string;
begin
  Result := TPath.Combine(FTempPath, TGUID.NewGuid.ToString + '.html');
  CreateReadOnlyFile(Result, GenerateHtmlText(Rule));
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateHtmlText(Rule: TRule): string;
var
  BodyHtml: string;

  procedure Add(Snippet: string);
  begin
    BodyHtml := BodyHtml + Snippet;
  end;

  procedure AddFmt(Snippet: string; Args: array of const);
  begin
    Add(Format(Snippet, Args));
  end;

var
  Quality: TSoftwareQuality;
  ImpactSeverity: TImpactSeverity;
begin
  // Add clean code attributes above rule title (CC only)
  if Assigned(Rule.CleanCode) then begin
    AddFmt(
      '<span class="subheading cleancode" title="%s">' +
        '<strong>%s rule</strong> | %s' +
      '</span>',
      [
        GetAttributeTooltip(Rule.CleanCode.Attribute),
        GetCleanCodeCategoryStr(Rule.CleanCode.Category),
        GetCleanCodeAttributeStr(Rule.CleanCode.Attribute)
      ]);
  end;

  // Add rule title
  AddFmt('<h1 title="%s">%s</h1>', [Rule.RuleKey, Rule.Name]);

  // Add badges
  Add('<div class="impacts">');

  // Add impact severity badges (CC only)
  if Assigned(Rule.CleanCode) then begin
    Add('<div class="impacts">');

    for Quality := Low(TSoftwareQuality) to High(TSoftwareQuality) do begin
      if not Rule.CleanCode.Impacts.ContainsKey(Quality) then begin
        Continue;
      end;

      ImpactSeverity := Rule.CleanCode.Impacts[Quality];
      AddFmt(
        '<span class="impact %s" title="%s">%s <img src="%s"/></span>',
        [
          GetImpactSeverityClassName(ImpactSeverity),
          GetImpactTooltip(Quality, ImpactSeverity),
          GetSoftwareQualityStr(Quality),
          THtmlUtils.ImageToBase64(LintResources.ImpactSeverityIcon(ImpactSeverity))
        ]);
    end;

    Add('</div>');
  end;

  // Add rule type/severity badge (non-CC only)
  if not Assigned(Rule.CleanCode) then begin
    if Rule.RuleType = rtSecurityHotspot then begin
      AddFmt(
        '<span class="impact hotspot" title="%s"><img src="%s"/>Security Hotspot</span>',
        [
          GetRuleTypeTooltip(Rule.RuleType, Rule.Severity),
          THtmlUtils.ImageToBase64(LintResources.RuleTypeIcon(Rule.RuleType, Rule.Severity))
        ]);
    end
    else begin
      AddFmt(
        '<span class="impact standard %s" title="%s"><img src="%s"/>%s<img src="%s"/></span>',
        [
          IfThen(Rule.RuleType = rtSecurityHotspot, '', GetRuleSeverityClassName(Rule.Severity)),
          GetRuleTypeTooltip(Rule.RuleType, Rule.Severity),
          THtmlUtils.ImageToBase64(LintResources.RuleTypeIcon(Rule.RuleType, Rule.Severity)),
          GetRuleTypeStr(Rule.RuleType),
          THtmlUtils.ImageToBase64(LintResources.RuleSeverityIcon(Rule.Severity))
        ]);
    end;
  end;

  Add('</div>');

  // Add rule description
  Add(GenerateRuleDescriptionHtml(Rule.Description));

  // Substitute into HTML boilerplate
  Result := BuildHtmlPage(BodyHtml, IfThen(Assigned(Rule.CleanCode), 'cleancode', ''));
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateRuleDescriptionHtml(Description: TRuleDescription): string;

  function EnsureWrapped(Content: string): string;
  begin
    // Old SonarDelphi versions automatically wrapped the rule in a <p> tag, but newer
    // ones (beginning with a heading) are meant to be displayed directly.
    //
    // In addition, custom rules are generally not wrapped in HTML tags.
    if StartsText('<', Trim(Content)) then begin
      Result := Content;
    end
    else begin
      Result := THtmlUtils.WrapHtml(Content, 'p');
    end;
  end;

  function GenerateButtonHtml(ContentId: string; Text: string; Active: Boolean): string;
  begin
    Result := Format(
      '<div class="tab-btn%s" data-content-id="%s">%s</div>',
      [IfThen(Active, ' active', ''), ContentId, Text]);
  end;

  function GenerateContentHtml(ContentId: string; Content: string; Active: Boolean): string;
  begin
    Result := Format(
      '<div class="tab-content%s" id="%s">%s</div>',
      [IfThen(Active, ' active', ''), ContentId, EnsureWrapped(Content)]);
  end;

var
  NoTabs: Boolean;
  ButtonsHtml: string;
  ContentHtml: string;
begin
  ButtonsHtml := '<div class="tab-buttons">';
  ContentHtml := '<div class="tab-contents">';
  NoTabs := True;

  if Description.RootCause <> '' then begin
    ButtonsHtml := ButtonsHtml + GenerateButtonHtml('desc-root-cause', 'Why is this an issue?', NoTabs);
    ContentHtml := ContentHtml + GenerateContentHtml('desc-root-cause', Description.RootCause, NoTabs);
    NoTabs := False;
  end;

  if Description.HowToFix <> '' then begin
    ButtonsHtml := ButtonsHtml + GenerateButtonHtml('desc-how-to-fix', 'How can I fix it?', NoTabs);
    ContentHtml := ContentHtml + GenerateContentHtml('desc-how-to-fix', Description.HowToFix, NoTabs);
    NoTabs := False;
  end;

  if Description.Resources <> '' then begin
    ButtonsHtml := ButtonsHtml + GenerateButtonHtml('desc-more-info', 'More info', NoTabs);
    ContentHtml := ContentHtml + GenerateContentHtml(
      'desc-more-info',
      '<h2>Resources</h2>' + EnsureWrapped(Description.Resources),
      NoTabs);
  end;

  ButtonsHtml := ButtonsHtml + '</div>';
  ContentHtml := ContentHtml + '</div>';

  if Description.Introduction <> '' then begin
    Result := EnsureWrapped(Description.Introduction);
  end;
  if not NoTabs then begin
    Result := Result + ButtonsHtml + ContentHtml;
  end;
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.BuildHtmlPage(BodyHtml: string; BodyClass: string): string;
begin
  Result := Format(
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <link rel="stylesheet" href="style.css"/> ' +
    '  <style>%s</style>' +
    '</head>' +
    '<body class="%s">' +
    '  <div class="content">%s</div>' +
    '  <script src="script.js"></script>' +
    '</body>' +
    '</html>',
    [GenerateCss, BodyClass, BodyHtml]);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateCss: string;
begin
  Result := Format(
    'html { color: %s; background-color: %s; }' +
    'body::-webkit-scrollbar { background-color: %s; padding: 1em; }' +
    'body::-webkit-scrollbar-thumb { background-color: %s; }' +
    'a { color: %s; }' +
    '.tab-buttons { border-color: %s; }' +
    '.tab-btn:hover { background-color: %s; }' +
    '.tab-btn.active { background-color: %s; }',
    [FTextColor, FBgColor, FBgColor, FScrollbarColor, FLinkColor, FTextColor, FHoverTabColor, FActiveTabColor]);
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetAttributeTooltip(Attribute: TCleanCodeAttribute): string;
begin
  case Attribute of
    ccaFormatted:
      Result := 'This rule ensures code is presented systematically and regularly.';
    ccaConventional:
      Result := 'This rule ensures code consistently adheres to a single choice.';
    ccaIdentifiable:
      Result := 'This rule ensures names follow a regular structure based on language conventions.';
    ccaClear:
      Result := 'This rule ensures code is self-explanatory, transparently communicating its functionality.';
    ccaLogical:
      Result := 'This rule ensures code has well-formed and sound instructions that work together.';
    ccaComplete:
      Result := 'This rule ensures code constructs are comprehensive and used adequately and thoroughly.';
    ccaEfficient:
      Result := 'This rule ensures code utilizes resources without needless waste.';
    ccaFocused:
      Result := 'This rule ensures each code unit has a single, narrow, and specific scope.';
    ccaDistinct:
      Result := 'This rule ensures code procedures and data do not have unnecessary duplication.';
    ccaModular:
      Result := 'This rule ensures code has been organized to emphasize the separation between its parts.';
    ccaTested:
      Result := 'This rule ensures code has automated checks that provide confidence in the functionality.';
    ccaLawful:
      Result := 'This rule ensures licensing and copyright regulation is respected.';
    ccaTrustworthy:
      Result := 'This rule ensures code abstains from revealing or hard-coding private information.';
    ccaRespectful:
      Result := 'This rule ensures code refrains from using discriminatory and offensive language.';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetCleanCodeAttributeStr(Attribute: TCleanCodeAttribute): string;
begin
  case Attribute of
    ccaFormatted: Result := 'Formatted';
    ccaConventional: Result := 'Conventional';
    ccaIdentifiable: Result := 'Identifiable';
    ccaClear: Result := 'Clear';
    ccaLogical: Result := 'Logical';
    ccaComplete: Result := 'Complete';
    ccaEfficient: Result := 'Efficient';
    ccaFocused: Result := 'Focused';
    ccaDistinct: Result := 'Distinct';
    ccaModular: Result := 'Modular';
    ccaTested: Result := 'Tested';
    ccaLawful: Result := 'Lawful';
    ccaTrustworthy: Result := 'Trustworthy';
    ccaRespectful: Result := 'Respectful';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetCleanCodeCategoryStr(Category: TCleanCodeAttributeCategory): string;
begin
  case Category of
    cccConsistent: Result := 'Consistency';
    cccIntentional: Result := 'Intentionality';
    cccAdaptable: Result := 'Adaptability';
    cccResponsible: Result := 'Responsibility';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetImpactSeverityClassName(Severity: TImpactSeverity): string;
begin
  case Severity of
    imsLow: Result := 'low';
    imsMedium: Result := 'medium';
    imsHigh: Result := 'high';
    imsInfo: Result := 'info';
    imsBlocker: Result := 'blocker';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetRuleSeverityClassName(Severity: TRuleSeverity): string;
begin
  case Severity of
    rsMinor: Result := 'low';
    rsMajor: Result := 'medium';
    rsCritical: Result := 'high';
    rsInfo: Result := 'info';
    rsBlocker: Result := 'blocker';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetRuleTypeTooltip(RuleType: TRuleType; Severity: TRuleSeverity): string;
const
  CRuleTypeWords: array[TRuleType] of string = ('code smell', 'bug', 'vulnerability', 'security hotspot');
  CSeverityWords: array[TRuleSeverity] of string = ('info-level', 'minor', 'major', 'critical', 'blocker-level');
  CRuleTypeDescs: array[TRuleType] of string = (
    'may make your code confusing, inconsistent, or difficult to maintain',
    'may result in an error or unexpected behaviour',
    'may leave your code open to attack',
    'is security-sensitive and needs to be carefully reviewed'
  );
var
  SeverityWord: string;
begin
  SeverityWord := '';
  if RuleType <> rtSecurityHotspot then begin
    SeverityWord := CSeverityWords[Severity] + ' ';
  end;

  Result := Format(
    'This %s%s %s.',
    [SeverityWord, CRuleTypeWords[RuleType], CRuleTypeDescs[RuleType]]);
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetImpactTooltip(Quality: TSoftwareQuality; Severity: TImpactSeverity): string;
var
  SeverityWord: string;
  QualityWord: string;
begin
  case Severity of
    imsLow: SeverityWord := 'has a low';
    imsMedium: SeverityWord := 'has a medium';
    imsHigh: SeverityWord := 'has a high';
    imsInfo: SeverityWord := 'may have an';
    imsBlocker: SeverityWord := 'has a very high (blocking)';
  end;

  case Quality of
    sqaSecurity: QualityWord := 'security';
    sqaReliability: QualityWord := 'reliability';
    sqaMaintainability: QualityWord := 'maintainability';
  end;

  Result := Format(
    'This %s impact on the %s of your code.',
    [SeverityWord, QualityWord]);
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetRuleTypeStr(RuleType: TRuleType): string;
begin
  case RuleType of
    rtCodeSmell: Result := 'Code Smell';
    rtBug: Result := 'Bug';
    rtVulnerability: Result := 'Vulnerability';
    rtSecurityHotspot: Result := 'Security Hotspot';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetSoftwareQualityStr(Quality: TSoftwareQuality): string;
begin
  case Quality of
    sqaSecurity: Result := 'Security';
    sqaReliability: Result := 'Reliability';
    sqaMaintainability: Result := 'Maintainability';
  end;
end;

//______________________________________________________________________________________________________________________

end.
