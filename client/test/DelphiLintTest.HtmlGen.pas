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
unit DelphiLintTest.HtmlGen;

interface

uses
    DUnitX.TestFramework
  , DelphiLint.HtmlGen
  ;

type
  [TestFixture]
  THtmlUtilsTest = class(TObject)
  public
    [Test]
    procedure TestColorToHex;
    [Test]
    procedure TestWrapHtml;
    [Test]
    procedure TestNoWrapTwice;
    [Test]
    procedure TestImageToBase64;
  end;

  [TestFixture]
  TRuleHtmlGeneratorTest = class(TObject)
  private
    FHtmlGenerator: TRuleHtmlGenerator;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestUsesColorsFromIDETheme;
    [Test]
    procedure TestLinksToJsLibScript;
    [Test]
    procedure TestWrapsOldStyleDescriptions;
    [Test]
    procedure TestDoesNotWrapNewStyleDescriptions;
    [Test]
    procedure TestRuleNameIsHeading;
    [Test]
    procedure TestRuleNameIsHeadingCleanCode;
    [Test]
    procedure TestSecurityHotspotDoesNotContainSeverity;
    [Test]
    procedure TestRuleContainsRuleTypeAndSeverity;
    [Test]
    procedure TestCleanCodeRuleContainsAttributes;
    [Test]
    procedure TestCleanCodeRuleDoesNotContainRuleTypeOrSeverity;
    [Test]
    procedure TestAllCleanCodeAttributesHaveTooltip;
    [Test]
    procedure TestAllQualitiesHaveImpactTooltip;
    [Test]
    procedure TestAllRuleTypesHaveTooltip;
    [Test]
    procedure TestAllImpactSeveritiesHaveClassName;
    [Test]
    procedure TestAllCleanCodeCategoriesHaveStr;
    [Test]
    procedure TestAllCleanCodeAttributesHaveStr;
    [Test]
    procedure TestAllRuleTypesHaveStr;
    [Test]
    procedure TestAllQualitiesHaveStr;
  end;

implementation

uses
    System.SysUtils
  , System.Generics.Collections
  , Vcl.Graphics
  , DelphiLintTest.MockContext
  , DelphiLint.Resources
  , DelphiLint.Data
  ;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.Setup;
var
  MockIDEServices: TMockIDEServices;
begin
  MockIDEServices := TMockIDEServices.Create;
  MockIDEServices.MockSystemColor(clBtnText, $0000FF); // Text color
  MockIDEServices.MockSystemColor(clWindow, $00FF00); // Background color
  MockIDEServices.MockSystemColor(clBtnFace, $FF0000); // Scrollbar color
  MockIDEServices.MockSystemColor(clHotLight, $00FFFF); // Link color
  MockContext.MockIDEServices(MockIDEServices);
  FHtmlGenerator := TRuleHtmlGenerator.Create;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TearDown;
begin
  FreeAndNil(FHtmlGenerator);
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestUsesColorsFromIDETheme;
var
  Css: string;
begin
  Css := FHtmlGenerator.GenerateCss;
  Assert.IsMatch(
    'html\s*{[^}]*?color:\s*#FF0000',
    Css,
    'CSS HTML text color should be the hex code for clBtnText');
  Assert.IsMatch(
    '.tab-buttons\s*,\s*.tab-btn\s*{[^}]*?border-color:\s*#FF0000',
    Css,
    'CSS tab border color should be the hex code for clBtnText');
  Assert.IsMatch(
    'html\s*{[^}]*?background-color:\s*#00FF00',
    Css,
    'CSS HTML background color should be the hex code for clWindow');
  Assert.IsMatch(
    'body::-webkit-scrollbar\s*{[^}]*?background-color:\s*#00FF00',
    Css,
    'CSS scrollbar background color should be the hex code for clWindow');
  Assert.IsMatch(
    'body::-webkit-scrollbar-thumb\s*{[^}]*?background-color:\s*#0000FF',
    Css,
    'CSS scrollbar thumb color should be the hex code for clBtnFace');
  Assert.IsMatch(
    'a\s*{[^}]*?color:\s*#FFFF00',
    Css,
    'CSS link color should be the hex code for clHotLight');
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestWrapsOldStyleDescriptions;
const
  CIntro = 'My <strong>intro</strong>';
  CRootCause = 'My <strong>root cause</strong>';
  CHowToFix = 'My <strong>How to fix</strong>';
  CResources = 'My <strong>resources</strong>';
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'RuleName1',
    TRuleDescription.Create(CIntro, CRootCause, CHowToFix, CResources),
    rsMajor,
    rtCodeSmell,
    nil
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, '<p>' + CIntro + '</p>');
    Assert.Contains(HtmlText, '<p>' + CRootCause + '</p>');
    Assert.Contains(HtmlText, '<p>' + CHowToFix + '</p>');
    Assert.Contains(HtmlText, '<p>' + CResources + '</p>');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestDoesNotWrapNewStyleDescriptions;
const
  CDescription = '<h2>Why is this an issue?</h2>'#13#10'<p>My <strong>HTML</strong> description</p>';
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'RuleName1',
    TRuleDescription.Create('', CDescription, '', ''),
    rsMajor,
    rtCodeSmell,
    nil
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, CDescription);
    Assert.DoesNotContain(HtmlText, '<p>' + CDescription + '</p>');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestLinksToJsLibScript;
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'RuleName1',
    TRuleDescription.Create('','','',''),
    rsMajor,
    rtCodeSmell,
    nil
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, '<script src="script.js"></script>');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllCleanCodeAttributesHaveStr;
var
  Value: TCleanCodeAttribute;
begin
  for Value := Low(TCleanCodeAttribute) to High(TCleanCodeAttribute) do begin
    Assert.IsNotEmpty(TRuleHtmlGenerator.GetCleanCodeAttributeStr(Value));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllCleanCodeAttributesHaveTooltip;
var
  Value: TCleanCodeAttribute;
begin
  for Value := Low(TCleanCodeAttribute) to High(TCleanCodeAttribute) do begin
    Assert.IsNotEmpty(TRuleHtmlGenerator.GetAttributeTooltip(Value));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllCleanCodeCategoriesHaveStr;
var
  Value: TCleanCodeAttributeCategory;
begin
  for Value := Low(TCleanCodeAttributeCategory) to High(TCleanCodeAttributeCategory) do begin
    Assert.IsNotEmpty(TRuleHtmlGenerator.GetCleanCodeCategoryStr(Value));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllImpactSeveritiesHaveClassName;
var
  Value: TImpactSeverity;
begin
  for Value := Low(TImpactSeverity) to High(TImpactSeverity) do begin
    Assert.IsNotEmpty(TRuleHtmlGenerator.GetImpactSeverityClassName(Value));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllQualitiesHaveImpactTooltip;
var
  Severity: TImpactSeverity;
  Quality: TSoftwareQuality;
begin
  for Quality := Low(TSoftwareQuality) to High(TSoftwareQuality) do begin
    for Severity := Low(TImpactSeverity) to High(TImpactSeverity) do begin
      Assert.IsNotEmpty(TRuleHtmlGenerator.GetImpactTooltip(Quality, Severity));
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllQualitiesHaveStr;
var
  Value: TSoftwareQuality;
begin
  for Value := Low(TSoftwareQuality) to High(TSoftwareQuality) do begin
    Assert.IsNotEmpty(TRuleHtmlGenerator.GetSoftwareQualityStr(Value));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllRuleTypesHaveStr;
var
  Value: TRuleType;
begin
  for Value := Low(TRuleType) to High(TRuleType) do begin
    Assert.IsNotEmpty(TRuleHtmlGenerator.GetRuleTypeStr(Value));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestAllRuleTypesHaveTooltip;
var
  RuleType: TRuleType;
  Severity: TRuleSeverity;
begin
  for RuleType := Low(TRuleType) to High(TRuleType) do begin
    for Severity := Low(TRuleSeverity) to High(TRuleSeverity) do begin
      Assert.IsNotEmpty(TRuleHtmlGenerator.GetRuleTypeTooltip(RuleType, Severity));
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestCleanCodeRuleContainsAttributes;
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'This rule could be better',
    TRuleDescription.Create('', '', '', ''),
    rsMajor,
    rtCodeSmell,
    TRuleCleanCode.Create(
      ccaClear,
      cccIntentional,
      TDictionary<TSoftwareQuality, TImpactSeverity>.Create([
        TPair<TSoftwareQuality, TImpactSeverity>.Create(sqaSecurity, imsMedium),
        TPair<TSoftwareQuality, TImpactSeverity>.Create(sqaMaintainability, imsLow)
      ])
    )
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, '<strong>Intentionality rule</strong> | Clear');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestCleanCodeRuleDoesNotContainRuleTypeOrSeverity;
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'This rule could be better',
    TRuleDescription.Create('', '', '', ''),
    rsMajor,
    rtCodeSmell,
    TRuleCleanCode.Create(
      ccaClear,
      cccIntentional,
      TDictionary<TSoftwareQuality, TImpactSeverity>.Create([
        TPair<TSoftwareQuality, TImpactSeverity>.Create(sqaSecurity, imsMedium),
        TPair<TSoftwareQuality, TImpactSeverity>.Create(sqaMaintainability, imsLow)
      ])
    )
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.DoesNotContain(HtmlText, 'Code smell');
    Assert.DoesNotContain(HtmlText, 'Major');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestSecurityHotspotDoesNotContainSeverity;
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'This rule could be better',
    TRuleDescription.Create('', '', '', ''),
    rsMajor,
    rtSecurityHotspot,
    nil
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, 'Security Hotspot');
    Assert.Contains(HtmlText, 'class="impact hotspot"');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestRuleContainsRuleTypeAndSeverity;
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    'This rule could be better',
    TRuleDescription.Create('', '', '', ''),
    rsMajor,
    rtCodeSmell,
    nil
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, 'Code Smell');
    Assert.Contains(HtmlText, 'class="impact standard medium"');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestRuleNameIsHeading;
const
  CRuleName = 'This code should be better';
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    CRuleName,
    TRuleDescription.Create('', '', '', ''),
    rsMajor,
    rtCodeSmell,
    nil
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, '<h1 title="rk1">' + CRuleName + '</h1>');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TRuleHtmlGeneratorTest.TestRuleNameIsHeadingCleanCode;
const
  CRuleName = 'This code should be better';
var
  Rule: TRule;
  HtmlText: string;
begin
  Rule := TRule.Create(
    'rk1',
    CRuleName,
    TRuleDescription.Create('', '', '', ''),
    rsMajor,
    rtCodeSmell,
    TRuleCleanCode.Create(ccaClear, cccIntentional, TDictionary<TSoftwareQuality, TImpactSeverity>.Create)
  );
  try
    HtmlText := FHtmlGenerator.GenerateHtmlText(Rule);
    Assert.Contains(HtmlText, '<h1 title="rk1">' + CRuleName + '</h1>');
  finally
    FreeAndNil(Rule);
  end;
end;

//______________________________________________________________________________________________________________________

procedure THtmlUtilsTest.TestColorToHex;
begin
  Assert.AreEqual('#000000', THtmlUtils.ColorToHex(0));
  Assert.AreEqual('#0000FF', THtmlUtils.ColorToHex($FF0000));
  Assert.AreEqual('#EFCDAB', THtmlUtils.ColorToHex($ABCDEF));
  Assert.AreEqual('#EF8B04', THtmlUtils.ColorToHex($048BEF));
  Assert.AreEqual('#FFFFFF', THtmlUtils.ColorToHex($FFFFFF));
end;

//______________________________________________________________________________________________________________________

procedure THtmlUtilsTest.TestWrapHtml;
begin
  Assert.AreEqual('<p>My HTML</p>', THtmlUtils.WrapHtml('My HTML', 'p'));
  Assert.AreEqual('<p>My <strong>HTML</strong></p>', THtmlUtils.WrapHtml('My <strong>HTML</strong>', 'p'));
  Assert.AreEqual('<p>My <p>HTML</p></p>', THtmlUtils.WrapHtml('My <p>HTML</p>', 'p'));
end;

//______________________________________________________________________________________________________________________

procedure THtmlUtilsTest.TestNoWrapTwice;
begin
  Assert.AreEqual('<p>My HTML</p>', THtmlUtils.WrapHtml('<p>My HTML</p>', 'p'));
  Assert.AreEqual('<p>My <strong>HTML</strong></p>', THtmlUtils.WrapHtml('<p>My <strong>HTML</strong></p>', 'p'));
  Assert.AreEqual('<p>My <p>HTML</p></p>', THtmlUtils.WrapHtml('<p>My <p>HTML</p></p>', 'p'));
end;

//______________________________________________________________________________________________________________________

procedure THtmlUtilsTest.TestImageToBase64;
const
  CRuleSeverityIconBase64 = 'data:image/png;base64,'
  + 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAA'#13#10
  + 'GXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAWlJREFUeNqlk8FKw0AQhmd2RQQv'#13#10
  + '+ggmRdRWfJBSL9FHEMRDpSli6VlERRPwIIKPoBVFxPdoaYq2qY/gqSBidpy0SU3TpBQ6EHZ3MvPN'#13#10
  + '7j+7SEQwi2EqwNQ3PYRVfyoJPsByG1MBvLK2jYTnPF2LhbYI1LG0ui+pAGVqVXad+P6UHfvBVWG5'#13#10
  + 'Z2OAoPLThOQhhFMK0nZfRwDK1Fvj2041h3eR/QewYAqgHgv6IcS9IOiOh/noTwEiB1a72Qd4Zd1A'#13#10
  + 'gofRZNqV1mdfMM9cKfDx7qMQQtiRV24tCfDLyYafrEraTb+a3T0IIDVezg2EAIN1eBwc4TCTU5Ia'#13#10
  + 'YbtEb2lLLX5d82I/gN6K3nKRffVQJyFEFi7bTkTEjMPc9SDhm7+FmCZRX5NFzI220dQKCPg8XRsx'#13#10
  + 'L+3OW8JF0is8nE68SIQVYXcuxi5SaF5JzyOCH7ARS25y5aOwcipgaCysJ2jwmFC8+z1PCsNZn/Mf'#13#10
  + '+JG24UBLkDYAAAAASUVORK5CYII=';
begin
  Assert.AreEqual(CRuleSeverityIconBase64, THtmlUtils.ImageToBase64(LintResources.RuleSeverityIcon(rsMajor)));
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(THtmlUtilsTest);
  TDUnitX.RegisterTestFixture(TRuleHtmlGeneratorTest);

end.
