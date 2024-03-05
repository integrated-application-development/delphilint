package au.com.integradev.delphilint.remote.sonarqube;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import au.com.integradev.delphilint.remote.CleanCodeAttribute;
import au.com.integradev.delphilint.remote.CleanCodeAttributeCategory;
import au.com.integradev.delphilint.remote.ImpactSeverity;
import au.com.integradev.delphilint.remote.IssueLikeType;
import au.com.integradev.delphilint.remote.IssueStatus;
import au.com.integradev.delphilint.remote.RemoteActiveRule;
import au.com.integradev.delphilint.remote.RemoteIssue;
import au.com.integradev.delphilint.remote.RemotePlugin;
import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;
import au.com.integradev.delphilint.remote.SoftwareQuality;
import au.com.integradev.delphilint.remote.SonarHostException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang3.NotImplementedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SonarQubeHostTest {
  private static final String DEFAULT_QP_URL =
      "/api/qualityprofiles/search?language=delphi&defaults=true";
  private static final String QP_OK_JSON = "qualityProfileOk.json";
  private static final String QP_KEY = "AYmGl3ZO2-GgcVw1YiT1";
  private static final Version LATEST_VERSION = new Version("10.4");

  private static SonarQubeHost buildSonarHost(SonarApi sonarApi) {
    return buildSonarHost(sonarApi, "");
  }

  private static SonarQubeHost buildSonarHost(SonarApi sonarApi, String projectKey) {
    return new SonarQubeHost(sonarApi, projectKey, "delphi", "communitydelphi", "delphi");
  }

  @Test
  void recognisesUnsupportedVersion() throws SonarHostException {
    var api = new ResourceBackedSonarApi(new Version("5.7"), Collections.emptyMap());

    var host = buildSonarHost(api);
    assertFalse(host.getCharacteristics().isSupported());
  }

  @ParameterizedTest
  @ValueSource(strings = {"9.9.0.0", "11.2.0.0.01923", "11.5.3"})
  void recognisesSupportedVersion(String versionStr) throws SonarHostException {
    var api = new ResourceBackedSonarApi(new Version(versionStr), Collections.emptyMap());

    var host = buildSonarHost(api);
    assertTrue(host.getCharacteristics().isSupported());
  }

  @Test
  void recognisesNonCleanCodeVersion() throws SonarHostException {
    var api = new ResourceBackedSonarApi(new Version("9.9"), Collections.emptyMap());

    var host = buildSonarHost(api);
    assertFalse(host.getCharacteristics().usesCodeAttributes());
  }

  @ParameterizedTest
  @ValueSource(strings = {"10.2", "11.2.0.0.01923", "11.5.3"})
  void recognisesCleanCodeVersion(String versionStr) throws SonarHostException {
    var api = new ResourceBackedSonarApi(new Version(versionStr), Collections.emptyMap());

    var host = buildSonarHost(api);
    assertTrue(host.getCharacteristics().usesCodeAttributes());
  }

  @Test
  void getsDefaultQualityProfileWhenNoProject() throws SonarHostException {
    var api = new ResourceBackedSonarApi(LATEST_VERSION, Map.of(DEFAULT_QP_URL, QP_OK_JSON));

    var host = buildSonarHost(api);
    SonarQubeQualityProfile profile = host.getQualityProfile();

    assertEquals("AYmGl3ZO2-GgcVw1YiT1", QP_KEY);
    assertEquals("Sonar way", profile.getName());
    assertEquals("delphi", profile.getLanguage());
    assertEquals(89, profile.getActiveRuleCount());
  }

  @Test
  void getsQualityProfileByProjectKey() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of("/api/qualityprofiles/search?language=delphi&project=MyProject", QP_OK_JSON));

    var host = buildSonarHost(api, "MyProject");
    SonarQubeQualityProfile profile = host.getQualityProfile();

    assertEquals("AYmGl3ZO2-GgcVw1YiT1", profile.getKey());
    assertEquals("Sonar way", profile.getName());
    assertEquals("delphi", profile.getLanguage());
    assertEquals(89, profile.getActiveRuleCount());
  }

  @Test
  void throwsOnErrorQualityProfile() {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of("/api/qualityprofiles/search?language=delphi&defaults=true", "error.json"));

    var host = buildSonarHost(api);
    assertThrows(SonarHostException.class, host::getQualityProfile);
  }

  @Test
  void getsRuleNamesByRuleKey() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&f=name&language=delphi&qprofile="
                    + QP_KEY,
                "ruleNamesByRuleKeyOk.json"));

    var host = buildSonarHost(api);
    Map<String, String> ruleNamesByRuleKey = host.getRuleNamesByRuleKey();

    assertEquals(3, ruleNamesByRuleKey.size());
    assertEquals("name1", ruleNamesByRuleKey.get("key1"));
    assertEquals("name2", ruleNamesByRuleKey.get("key2"));
    assertEquals("name3", ruleNamesByRuleKey.get("key3"));
  }

  @ParameterizedTest
  @ValueSource(strings = {"rulesOk.json", "rulesCleanCodeOk.json"})
  void getsRules(String responseFile) throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&languages=delphi&qprofile="
                    + QP_KEY
                    + "&f=name%2ChtmlDesc%2Cseverity%2CcleanCodeAttribute",
                responseFile));

    var host = buildSonarHost(api);
    Set<RemoteRule> rules = host.getRules();

    assertEquals(3, rules.size());
  }

  @Test
  void parsesIssueWithoutCleanCode() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.1"),
            Map.of(
                "/api/qualityprofiles/search?language=delphi&project=MyProject",
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&projectKey=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/issues/search?componentKeys=MyProject%3AUnitA.pas%2C&resolved=true",
                "issuesSingularOk.json",
                "/api/issues/search?componentKeys=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "issuesSingularOk.json"));

    var host = buildSonarHost(api, "MyProject");
    var issueOpt =
        host.getResolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()).stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.ISSUE)
            .findFirst();

    assertTrue(issueOpt.isPresent());

    RemoteIssue issue = issueOpt.get();

    assertEquals("key1", issue.getRuleKey());
    assertEquals("msg1", issue.getMessage());
    assertEquals(RuleSeverity.MINOR, issue.getSeverity());
    assertEquals(RuleType.CODE_SMELL, issue.getType());
    assertEquals("e141e75692a375ca2a38daa5e01f38bd", issue.getLineHash());
    assertNull(issue.getCleanCode());
  }

  @Test
  void parsesIssueWithCleanCode() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.2"),
            Map.of(
                "/api/qualityprofiles/search?language=delphi&project=MyProject",
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&projectKey=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true",
                "issuesSingularCleanCodeOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "issuesSingularCleanCodeOk.json"));

    var host = buildSonarHost(api, "MyProject");
    var issueOpt =
        host.getResolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()).stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.ISSUE)
            .findFirst();

    assertTrue(issueOpt.isPresent());

    RemoteIssue issue = issueOpt.get();

    assertEquals("key1", issue.getRuleKey());
    assertEquals("msg1", issue.getMessage());
    assertEquals(RuleSeverity.MINOR, issue.getSeverity());
    assertEquals(RuleType.CODE_SMELL, issue.getType());
    assertEquals("e141e75692a375ca2a38daa5e01f38bd", issue.getLineHash());
    assertNotNull(issue.getCleanCode());
    assertEquals(CleanCodeAttribute.CLEAR, issue.getCleanCode().getAttribute());
    assertEquals(
        CleanCodeAttributeCategory.INTENTIONAL, issue.getCleanCode().getAttribute().getCategory());
    assertEquals(
        ImpactSeverity.HIGH,
        issue.getCleanCode().getImpactedQualities().get(SoftwareQuality.MAINTAINABILITY));
  }

  @Test
  void parsesRuleWithoutCleanCode() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("9.9"),
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&languages=delphi&qprofile="
                    + QP_KEY
                    + "&f=name%2ChtmlDesc%2Cseverity",
                "rulesSingularOk.json"));

    var host = buildSonarHost(api);
    Set<RemoteRule> rules = host.getRules();

    assertEquals(1, rules.size());
    RemoteRule rule = rules.stream().findFirst().orElse(null);
    assert rule != null;
    assertEquals("key1", rule.getKey());
    assertEquals("name1", rule.getName());
    assertEquals("html1", rule.getHtmlDesc());
    assertEquals(RuleSeverity.MAJOR, rule.getSeverity());
    assertEquals(RuleType.BUG, rule.getType());
    assertNull(rule.getCleanCode());
  }

  @Test
  void parsesRuleWithCleanCode() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.4"),
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&languages=delphi&qprofile="
                    + QP_KEY
                    + "&f=name%2ChtmlDesc%2Cseverity%2CcleanCodeAttribute",
                "rulesSingularCleanCodeOk.json"));

    var host = buildSonarHost(api);
    Set<RemoteRule> rules = host.getRules();

    assertEquals(1, rules.size());
    RemoteRule rule = rules.stream().findFirst().orElse(null);
    assert rule != null;
    assertEquals("key1", rule.getKey());
    assertEquals("name1", rule.getName());
    assertEquals("html1", rule.getHtmlDesc());
    assertEquals(RuleSeverity.MAJOR, rule.getSeverity());
    assertEquals(RuleType.BUG, rule.getType());
    assertNotNull(rule.getCleanCode());
    assertEquals(CleanCodeAttribute.RESPECTFUL, rule.getCleanCode().getAttribute());
    assertEquals(
        CleanCodeAttributeCategory.RESPONSIBLE, rule.getCleanCode().getAttribute().getCategory());
    assertEquals(
        ImpactSeverity.MEDIUM,
        rule.getCleanCode().getImpactedQualities().get(SoftwareQuality.SECURITY));
  }

  @Test
  void getsNoResolvedIssuesWithNoProjectKey() throws SonarHostException {
    var api = new ResourceBackedSonarApi(LATEST_VERSION, Collections.emptyMap());

    var host = buildSonarHost(api);
    assertTrue(host.getResolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()).isEmpty());
  }

  @Test
  void getsResolvedTestIssuesAndHotspotsPre10_2() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.1"),
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&projectKey=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/hotspots/search?files=TestUnitA.pas%2C&projectKey=MyProject&status=REVIEWED",
                "resolvedTestHotspotsOk.json",
                "/api/issues/search?componentKeys=MyProject%3AUnitA.pas%2C&resolved=true",
                "resolvedIssuesOk.json",
                "/api/issues/search?componentKeys=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "resolvedIssuesOk.json",
                "/api/issues/search?componentKeys=MyProject%3ATestUnitA.pas%2C&resolved=true",
                "resolvedTestIssuesOk.json",
                "/api/issues/search?componentKeys=MyProject%3ATestUnitA.pas%2C&resolved=true&p=1",
                "resolvedTestIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> resolvedIssues =
        new HashSet<>(host.getResolvedIssues(Set.of("UnitA.pas"), Set.of("TestUnitA.pas")));

    assertTrue(
        resolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("RuleOnlyForTestCode")));
    assertTrue(
        resolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("HotspotOnlyForTestCode")));
  }

  @Test
  void getsResolvedTestIssuesAndHotspotsPost10_2() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.2"),
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&projectKey=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/hotspots/search?files=TestUnitA.pas%2C&projectKey=MyProject&status=REVIEWED",
                "resolvedTestHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true",
                "resolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "resolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=true",
                "resolvedTestIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=true&p=1",
                "resolvedTestIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> resolvedIssues =
        new HashSet<>(host.getResolvedIssues(Set.of("UnitA.pas"), Set.of("TestUnitA.pas")));

    assertTrue(
        resolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("RuleOnlyForTestCode")));
    assertTrue(
        resolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("HotspotOnlyForTestCode")));
  }

  @Test
  void getsResolvedTestIssuesAndHotspots() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&project=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/hotspots/search?files=TestUnitA.pas%2C&project=MyProject&status=REVIEWED",
                "resolvedTestHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true",
                "resolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "resolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=true",
                "resolvedTestIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=true&p=1",
                "resolvedTestIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> resolvedIssues =
        new HashSet<>(host.getResolvedIssues(Set.of("UnitA.pas"), Set.of("TestUnitA.pas")));

    assertTrue(
        resolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("RuleOnlyForTestCode")));
    assertTrue(
        resolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("HotspotOnlyForTestCode")));
  }

  @Test
  void getsResolvedIssuesAndHotspots() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&project=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true",
                "resolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "resolvedIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> resolvedIssues =
        new HashSet<>(host.getResolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()));

    assertEquals(9, resolvedIssues.size());
    assertEquals(
        4,
        resolvedIssues.stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.SECURITY_HOTSPOT)
            .count());
    assertEquals(
        5,
        resolvedIssues.stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.ISSUE)
            .count());
  }

  @Test
  void getsUnresolvedTestIssuesAndHotspotsPre10_2() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.1"),
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&projectKey=MyProject",
                "unresolvedHotspotsOk.json",
                "/api/hotspots/search?files=TestUnitA.pas%2C&projectKey=MyProject",
                "unresolvedTestHotspotsOk.json",
                "/api/issues/search?componentKeys=MyProject%3AUnitA.pas%2C&resolved=false",
                "unresolvedIssuesOk.json",
                "/api/issues/search?componentKeys=MyProject%3AUnitA.pas%2C&resolved=false&p=1",
                "unresolvedIssuesOk.json",
                "/api/issues/search?componentKeys=MyProject%3ATestUnitA.pas%2C&resolved=false",
                "unresolvedTestIssuesOk.json",
                "/api/issues/search?componentKeys=MyProject%3ATestUnitA.pas%2C&resolved=false&p=1",
                "unresolvedTestIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> unresolvedIssues =
        new HashSet<>(host.getUnresolvedIssues(Set.of("UnitA.pas"), Set.of("TestUnitA.pas")));

    assertTrue(
        unresolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("RuleOnlyForTestCode")));
    assertTrue(
        unresolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("HotspotOnlyForTestCode")));
  }

  @Test
  void getsUnresolvedTestIssuesAndHotspotsPost10_2() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.2"),
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&projectKey=MyProject",
                "unresolvedHotspotsOk.json",
                "/api/hotspots/search?files=TestUnitA.pas%2C&projectKey=MyProject",
                "unresolvedTestHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false",
                "unresolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false&p=1",
                "unresolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=false",
                "unresolvedTestIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=false&p=1",
                "unresolvedTestIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> unresolvedIssues =
        new HashSet<>(host.getUnresolvedIssues(Set.of("UnitA.pas"), Set.of("TestUnitA.pas")));

    assertTrue(
        unresolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("RuleOnlyForTestCode")));
    assertTrue(
        unresolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("HotspotOnlyForTestCode")));
  }

  @Test
  void getsUnresolvedTestIssuesAndHotspots() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&project=MyProject",
                "unresolvedHotspotsOk.json",
                "/api/hotspots/search?files=TestUnitA.pas%2C&project=MyProject",
                "unresolvedTestHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false",
                "unresolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false&p=1",
                "unresolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=false",
                "unresolvedTestIssuesOk.json",
                "/api/issues/search?components=MyProject%3ATestUnitA.pas%2C&resolved=false&p=1",
                "unresolvedTestIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> unresolvedIssues =
        new HashSet<>(host.getUnresolvedIssues(Set.of("UnitA.pas"), Set.of("TestUnitA.pas")));

    assertTrue(
        unresolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("RuleOnlyForTestCode")));
    assertTrue(
        unresolvedIssues.stream()
            .anyMatch(remoteIssue -> remoteIssue.getRuleKey().equals("HotspotOnlyForTestCode")));
  }

  @Test
  void getsUnresolvedIssuesAndHotspots() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&project=MyProject",
                "unresolvedHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false",
                "unresolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false&p=1",
                "unresolvedIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> unresolvedIssues =
        new HashSet<>(host.getUnresolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()));

    assertEquals(8, unresolvedIssues.size());
    assertEquals(
        3,
        unresolvedIssues.stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.SECURITY_HOTSPOT)
            .count());
    assertEquals(
        5,
        unresolvedIssues.stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.ISSUE)
            .count());
  }

  @Test
  void doesNotTreatAcknowledgedHotspotsAsResolvedIssues() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&project=MyProject&status=REVIEWED",
                "resolvedHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true",
                "resolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=true&p=1",
                "resolvedIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> resolvedIssues =
        new HashSet<>(host.getResolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()));

    var resolvedHotspots =
        resolvedIssues.stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.SECURITY_HOTSPOT)
            .collect(Collectors.toList());

    assertEquals(4, resolvedHotspots.size());
    assertTrue(
        resolvedHotspots.stream().noneMatch(issue -> "ACKNOWLEDGED".equals(issue.getResolution())));
  }

  @Test
  void treatsAcknowledgedHotspotsAsUnresolvedIssues() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/hotspots/search?files=UnitA.pas%2C&project=MyProject",
                "unresolvedHotspotsOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false",
                "unresolvedIssuesOk.json",
                "/api/issues/search?components=MyProject%3AUnitA.pas%2C&resolved=false&p=1",
                "unresolvedIssuesOk.json"));

    var host = buildSonarHost(api, "MyProject");
    Set<RemoteIssue> unresolvedIssues =
        new HashSet<>(host.getUnresolvedIssues(Set.of("UnitA.pas"), Collections.emptySet()));

    var unresolvedHotspots =
        unresolvedIssues.stream()
            .filter(issue -> issue.getLikeType() == IssueLikeType.SECURITY_HOTSPOT)
            .collect(Collectors.toList());

    assertEquals(3, unresolvedHotspots.size());
    assertEquals(
        1,
        unresolvedHotspots.stream()
            .filter(hotspot -> hotspot.getStatus() == IssueStatus.REVIEWED)
            .count());
    assertEquals(
        "ACKNOWLEDGED",
        unresolvedHotspots.stream()
            .filter(hotspot -> hotspot.getStatus() == IssueStatus.REVIEWED)
            .findFirst()
            .get()
            .getResolution());
  }

  @Test
  void getsActiveRules() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&f=actives%2CtemplateKey&language=delphi&qprofile="
                    + QP_KEY,
                "activeRulesOk.json"));

    var host = buildSonarHost(api);
    Set<RemoteActiveRule> activeRules = host.getActiveRules();

    assertEquals(3, activeRules.size());

    Set<String> expectedActiveRules =
        Set.of(
            "community-delphi:DestructorWithoutInherited",
            "community-delphi:TooManyParameters",
            "community-delphi:ClassName");
    assertEquals(
        expectedActiveRules,
        activeRules.stream().map(RemoteActiveRule::getRuleKey).collect(Collectors.toSet()));
  }

  @Test
  void getsActiveRulesWithParams() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&f=actives%2CtemplateKey&language=delphi&qprofile="
                    + QP_KEY,
                "activeRulesOk.json"));

    var host = buildSonarHost(api);
    Set<RemoteActiveRule> activeRules = host.getActiveRules();

    assertEquals(3, activeRules.size());

    Optional<RemoteActiveRule> tooManyParams =
        activeRules.stream()
            .filter(filter -> "community-delphi:TooManyParameters".equals(filter.getRuleKey()))
            .findFirst();
    assertTrue(tooManyParams.isPresent());
    assertEquals(2, tooManyParams.get().getParams().size());
    assertEquals("5", tooManyParams.get().getParams().get("max"));
    assertEquals("7", tooManyParams.get().getParams().get("constructorMax"));
  }

  @Test
  void getsActiveRulesWithNoParams() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                DEFAULT_QP_URL,
                QP_OK_JSON,
                "/api/rules/search?ps=500&activation=true&f=actives%2CtemplateKey&language=delphi&qprofile="
                    + QP_KEY,
                "activeRulesOk.json"));

    var host = buildSonarHost(api);
    Set<RemoteActiveRule> activeRules = host.getActiveRules();

    assertEquals(3, activeRules.size());

    Optional<RemoteActiveRule> tooManyParams =
        activeRules.stream()
            .filter(
                filter -> "community-delphi:DestructorWithoutInherited".equals(filter.getRuleKey()))
            .findFirst();
    assertTrue(tooManyParams.isPresent());
    assertEquals(0, tooManyParams.get().getParams().size());
  }

  @Test
  void getsPluginJar() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of("/api/plugins/download?plugin=abcd", "filepath1")); // Can be any file

    var host = buildSonarHost(api);
    Optional<Path> pluginJarPath = host.getPluginJar("abcd");
    assertTrue(pluginJarPath.isPresent());
  }

  @Test
  void getsOnlyDelphiPlugins() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION, Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();
    assertTrue(plugins.stream().noneMatch(p -> p.getPluginKey().equals("java")));
  }

  @Test
  void getsRequiredForLanguagesDelphiPluginsOnSupportedVersions() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.5"), Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();

    assertTrue(plugins.stream().anyMatch(p -> p.getPluginKey().equals("mydelpcustomhi")));
  }

  @Test
  void doesNotGetRequiredForLanguagesDelphiPluginsOnUnsupportedVersions()
      throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("9.9"), Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();

    assertTrue(plugins.stream().noneMatch(p -> p.getPluginKey().equals("mydelpcustomhi")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"10.5", "10.2", "5.7", "11.5.3"})
  void getsNameHeuristicDelphiPluginsOnAllVersions(String versionStr) throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version(versionStr), Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();

    assertTrue(plugins.stream().anyMatch(p -> p.getPluginKey().equals("mycustomdelphi")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"10.5", "10.2", "5.7", "11.5.3"})
  void getsCorePluginOnAllVersions(String versionTxt) throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version(versionTxt), Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();

    assertTrue(plugins.stream().anyMatch(p -> p.getPluginKey().equals("communitydelphi")));
  }

  @Test
  void getsDelphiPluginFilenames() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version("10.2"), Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();

    assertEquals(2, plugins.size());

    var mainPlugin =
        plugins.stream()
            .filter(plugin -> "communitydelphi".equals(plugin.getPluginKey()))
            .findFirst();
    assertTrue(mainPlugin.isPresent());
    assertEquals("filename1", mainPlugin.get().getFileName());

    var secondaryPlugin =
        plugins.stream()
            .filter(plugin -> "mycustomdelphi".equals(plugin.getPluginKey()))
            .findFirst();
    assertTrue(secondaryPlugin.isPresent());
    assertEquals("filename2", secondaryPlugin.get().getFileName());
  }

  @ParameterizedTest
  @ValueSource(strings = {"10.2", "10.5"})
  void identifiesCorePlugin(String versionStr) throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            new Version(versionStr), Map.of("/api/plugins/installed", "installedPluginsOk.json"));

    var host = buildSonarHost(api);
    Set<RemotePlugin> plugins = host.getDelphiPlugins();

    var mainPlugins =
        plugins.stream().filter(RemotePlugin::isCorePlugin).collect(Collectors.toSet());
    assertEquals(1, mainPlugins.size());
    assertEquals("communitydelphi", mainPlugins.stream().findFirst().get().getPluginKey());
  }

  @Test
  void getsTestFilePaths() throws SonarHostException {
    var api =
        new ResourceBackedSonarApi(
            LATEST_VERSION,
            Map.of(
                "/api/components/tree?ps=500&component=MyProject&qualifiers=UTS",
                "testComponentsOk.json",
                "/api/components/tree?ps=500&component=MyProject&qualifiers=UTS&p=1",
                "testComponentsOk.json"));

    var host = buildSonarHost(api, "MyProject");

    Set<String> testFilePaths = host.getTestFilePaths();
    assertEquals(2, testFilePaths.size());
  }

  static class ResourceBackedSonarApi implements SonarApi {
    private static final Path BASE_PATH =
        Path.of("src/test/resources/au/com/integradev/delphilint/remote/sonarqube/mockedApi");

    private final Map<String, String> responseResources;
    private final ObjectMapper mapper;
    private final Version version;

    public ResourceBackedSonarApi(Version version, Map<String, String> responseResources) {
      this.version = version;
      this.responseResources = responseResources;
      this.mapper = new ObjectMapper();
    }

    @Override
    public String getHostUrl() {
      return "<DUMMY>";
    }

    private Path getPath(String url) {
      if (!responseResources.containsKey(url)) {
        throw new NotImplementedException("No resource assigned to mocked URL " + url);
      }
      return BASE_PATH.resolve(responseResources.get(url));
    }

    @Override
    public JsonNode getJson(String url) {
      try {
        return mapper.readTree(Files.newInputStream(getPath(url)));
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    }

    @Override
    public JsonNode getJson(String url, Map<String, String> params) {
      return getJson(url + HttpUtils.buildParamString(params));
    }

    @Override
    public Path getFile(String url) {
      return getPath(url);
    }

    @Override
    public Path getFile(String url, Map<String, String> params) {
      return getFile(url + HttpUtils.buildParamString(params));
    }

    @Override
    public String getText(String url) {
      if (url.endsWith("/api/server/version")) {
        return version.toString();
      }

      try {
        return Files.readString(getPath(url));
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    }

    @Override
    public String getText(String url, Map<String, String> params) {
      return getText(url + HttpUtils.buildParamString(params));
    }
  }
}
