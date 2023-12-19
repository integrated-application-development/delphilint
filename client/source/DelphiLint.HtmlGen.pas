unit DelphiLint.HtmlGen;

interface

uses
    Vcl.Graphics
  , DelphiLint.Data
  ;

type
  THtmlUtils = class(TObject)
  public
    class function ColorToHex(Color: TColor): string;
    class function WrapHtml(Html: string; WrappingTag: string): string;
    class function ImageToBase64(Image: TGraphic): string;
    class function BuildHtmlPage(BodyHtml: string; Css: string = ''; Js: string = ''; BodyClass: string = ''): string;
  end;

  TRuleHtmlGenerator = class(TObject)
  private
    FTextColor: string;
    FBgColor: string;
    FCodeBgColor: string;
    FLinkColor: string;

    function BuildTooltip(Html: string; Text: string = ''): string;
  public
    class function GetRuleTypeStr(RuleType: TRuleType): string;
    class function GetRuleSeverityStr(Severity: TRuleSeverity): string;
    class function GetCleanCodeAttributeStr(Attribute: TCleanCodeAttribute): string;
    class function GetCleanCodeCategoryStr(Category: TCleanCodeAttributeCategory): string;
    class function GetSoftwareQualityStr(Quality: TSoftwareQuality): string;
    class function GetImpactSeverityClassName(Severity: TImpactSeverity): string;
    class function GetImpactTooltip(Quality: TSoftwareQuality; Severity: TImpactSeverity): string;
    class function GetAttributeTooltip(Attribute: TCleanCodeAttribute): string;

    constructor Create;
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
  , System.Classes
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
  UpdateColors;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGenerator.UpdateColors;
begin
  FTextColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnText));
  FBgColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clWindow));
  FCodeBgColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clBtnFace));
  FLinkColor := THtmlUtils.ColorToHex(LintContext.IDEServices.GetSystemColor(clHotLight));
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateHtmlFile(Rule: TRule): string;
begin
  Result := TPath.GetTempFileName;
  TFile.WriteAllText(Result, GenerateHtmlText(Rule), TEncoding.UTF8);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateHtmlText(Rule: TRule): string;
var
  ImpactsHtml: string;
  BodyHtml: string;
  Quality: TSoftwareQuality;
  ImpactSeverity: TImpactSeverity;
  RuleSeverityHtml: string;
  ProcessedRuleDesc: string;
begin
  // Old SonarDelphi versions automatically wrapped the rule in a <p> tag, but newer
  // ones (beginning with a heading) are meant to be displayed directly.
  if StartsText('<h', Trim(Rule.Desc)) then begin
    ProcessedRuleDesc := Rule.Desc;
  end
  else begin
    ProcessedRuleDesc := THtmlUtils.WrapHtml(Rule.Desc, 'p');
  end;

  if Assigned(Rule.CleanCode) then begin
    for Quality in Rule.CleanCode.Impacts.Keys do begin
      ImpactSeverity := Rule.CleanCode.Impacts[Quality];
      ImpactsHtml := ImpactsHtml + BuildTooltip(
        Format(
          '<span class="impact %s">%s <img src="%s"/></span>',
          [
            GetImpactSeverityClassName(ImpactSeverity),
            GetSoftwareQualityStr(Quality),
            THtmlUtils.ImageToBase64(LintResources.ImpactSeverityIcon(ImpactSeverity))
          ]),
        GetImpactTooltip(Quality, ImpactSeverity)
      );
    end;

    BodyHtml := Format(
      BuildTooltip(
        '<span class="subheading cleancode">' +
          '<strong>%s rule</strong> | %s' +
        '</span>',
        GetAttributeTooltip(Rule.CleanCode.Attribute)
      ) +
      '<h1>%s</h1>' +
      '<hr/>' +
      '<div class="impacts">%s</div>' +
      '%s',
      [
        GetCleanCodeCategoryStr(Rule.CleanCode.Category),
        GetCleanCodeAttributeStr(Rule.CleanCode.Attribute),
        Rule.Name,
        ImpactsHtml,
        ProcessedRuleDesc
      ]);

    Result := THtmlUtils.BuildHtmlPage(BodyHtml, GenerateCss, LintResources.JsLibScript, 'cleancode');
  end
  else begin
    if Rule.RuleType <> rtSecurityHotspot then begin
      RuleSeverityHtml := Format(
        '<span class="gap"></span><img src="%s"/>%s',
        [
          THtmlUtils.ImageToBase64(LintResources.RuleSeverityIcon(Rule.Severity)),
          GetRuleSeverityStr(Rule.Severity)
        ]);
    end;

    BodyHtml := Format(
      '  <h1>%s</h1>' +
      '  <hr/>' +
      '  <span class="subheading"><img src="%s"/>%s%s</span>' +
      '  %s',
      [
        Rule.Name,
        THtmlUtils.ImageToBase64(LintResources.RuleTypeIcon(Rule.RuleType)),
        GetRuleTypeStr(Rule.RuleType),
        RuleSeverityHtml,
        ProcessedRuleDesc
      ]);

    Result := THtmlUtils.BuildHtmlPage(BodyHtml, GenerateCss, LintResources.JsLibScript);
  end;
end;

//______________________________________________________________________________________________________________________

class function THtmlUtils.BuildHtmlPage(BodyHtml: string; Css: string = ''; Js: string = ''; BodyClass: string = ''): string;
begin
  Result := Format(
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge"/>' +
    '  <style>%s</style>' +
    '</head>' +
    '<body class="%s">' +
    '  %s' +
    '  <script>%s</script>' +
    '</body>' +
    '</html>',
    [Css, BodyClass, BodyHtml, Js]);
end;

//______________________________________________________________________________________________________________________

function TRuleHtmlGenerator.GenerateCss: string;
begin
  Result := LintResources.RuleHtmlCss + Format(
    'body { color: %s; background-color: %s; }' +
    'pre { background-color: %s; }' +
    'a { color: %s; }' +
    '.tooltip-content { background-color: %s; }',
    [FTextColor, FBgColor, FCodeBgColor, FLinkColor, FCodeBgColor]);
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
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetImpactTooltip(Quality: TSoftwareQuality; Severity: TImpactSeverity): string;
var
  SeverityWord: string;
  QualityWord: string;
begin
  case Severity of
    imsLow: SeverityWord := 'low';
    imsMedium: SeverityWord := 'medium';
    imsHigh: SeverityWord := 'high';
  end;

  case Quality of
    sqaSecurity: QualityWord := 'security';
    sqaReliability: QualityWord := 'reliability';
    sqaMaintainability: QualityWord := 'maintainability';
  end;

  Result := Format(
    'This has a %s impact on the %s of your code.',
    [SeverityWord, QualityWord]);
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetRuleSeverityStr(Severity: TRuleSeverity): string;
begin
  case Severity of
    rsInfo: Result := 'Info';
    rsMinor: Result := 'Minor';
    rsMajor: Result := 'Major';
    rsCritical: Result := 'Critical';
    rsBlocker: Result := 'Blocker';
  end;
end;

//______________________________________________________________________________________________________________________

class function TRuleHtmlGenerator.GetRuleTypeStr(RuleType: TRuleType): string;
begin
  case RuleType of
    rtCodeSmell: Result := 'Code smell';
    rtBug: Result := 'Bug';
    rtVulnerability: Result := 'Vulnerability';
    rtSecurityHotspot: Result := 'Security hotspot';
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

function TRuleHtmlGenerator.BuildTooltip(Html: string; Text: string): string;
begin
  Result := Format('<span class="tooltip-hover"><span class="tooltip-content">%s</span>%s</span>', [Text, Html]);
end;

//______________________________________________________________________________________________________________________

end.
