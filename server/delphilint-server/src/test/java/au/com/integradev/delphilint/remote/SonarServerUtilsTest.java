package au.com.integradev.delphilint.remote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import au.com.integradev.delphilint.analysis.DelphiIssue;
import au.com.integradev.delphilint.analysis.DelphiLintInputFile;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.lang3.NotImplementedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.sonarsource.sonarlint.core.analysis.api.ClientInputFile;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.commons.TextRange;

class SonarServerUtilsTest {
  private static final Path BASE_DIR =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote");
  private static final Path FILE_A =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote/LocalFileA.pas");

  Issue buildIssue(Path filePath, String ruleKey, String message, TextRange range) {
    ClientInputFile file = new DelphiLintInputFile(BASE_DIR, BASE_DIR.relativize(filePath));
    return new Issue(
        ruleKey,
        message,
        Collections.emptyMap(),
        range,
        file,
        Collections.emptyList(),
        Collections.emptyList(),
        Optional.empty());
  }

  @Test
  void issuesAreConverted() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), Collections.emptySet(), Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("rk1", issues.get(0).getRuleKey());
    assertEquals("issue 1", issues.get(0).getMessage());
    assertEquals(6, issues.get(0).getTextRange().getStartLine());
    assertEquals(3, issues.get(0).getTextRange().getStartOffset());
    assertEquals(6, issues.get(0).getTextRange().getEndLine());
    assertEquals(9, issues.get(0).getTextRange().getEndOffset());
  }

  @Test
  void localIssuesHaveNoMetadata() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), Collections.emptySet(), Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertNull(issues.get(0).getMetadata());
  }

  @Test
  void unresolvedSecurityHotspotsPopulateMetadata() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withType(RuleType.SECURITY_HOTSPOT)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("ACKNOWLEDGED")
                .withHashFrom(FILE_A)
                .withServerMetadata("user1", "creation date 1")
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), Collections.emptySet(), resolvedIssues);

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("user1", issues.get(0).getMetadata().getAssignee());
    assertEquals("creation date 1", issues.get(0).getMetadata().getCreationDate());
    assertEquals(IssueStatus.REVIEWED, issues.get(0).getMetadata().getStatus());
  }

  @Test
  void unresolvedIssuesPopulateMetadata() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.CONFIRMED)
                .withResolution("resolution 1")
                .withHashFrom(FILE_A)
                .withServerMetadata("user1", "creation date 1")
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), Collections.emptySet(), resolvedIssues);

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("user1", issues.get(0).getMetadata().getAssignee());
    assertEquals("creation date 1", issues.get(0).getMetadata().getCreationDate());
    assertEquals(IssueStatus.CONFIRMED, issues.get(0).getMetadata().getStatus());
  }

  @ParameterizedTest
  @ValueSource(strings = {"OPEN", "CONFIRMED", "REOPENED", "TO_REVIEW"})
  void unresolvedIssuesAreNotPruned(String issueStatus) throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHashFrom(FILE_A)
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), Collections.emptySet(), unresolvedIssues);

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
  }

  @Test
  void resolvedIdenticalIssuesArePruned() throws SonarHostException {
    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk2", "issue 2", new TextRange(9, 0, 20, 10)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.RESOLVED)
                .withHashFrom(FILE_A)
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), resolvedIssues, Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("issue 2", issues.get(0).getMessage());
  }

  @Test
  void resolvedHotspotsArePruned() throws SonarHostException {
    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk2", "issue 2", new TextRange(9, 0, 20, 10)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("FIXED")
                .withType(RuleType.SECURITY_HOTSPOT)
                .withHashFrom(FILE_A)
                .build(),
            new RemoteIssueBuilder()
                .withRuleKey("rk2")
                .withMessage("issue 2")
                .withRange(9, 0, 20, 10)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("SAFE")
                .withType(RuleType.SECURITY_HOTSPOT)
                .withHashFrom(FILE_A)
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), resolvedIssues, Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void acknowledgedHotspotsAreNotPruned() throws SonarHostException {
    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk2", "issue 2", new TextRange(9, 0, 20, 10)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("ACKNOWLEDGED")
                .withHashFrom(FILE_A)
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), resolvedIssues, Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("issue 2", issues.get(0).getMessage());
  }

  @Test
  void movedResolvedIssuesArePruned() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(8, 3, 6, 9)
                .withStatus(IssueStatus.RESOLVED)
                .withHash(SonarHasher.hashFileLine(FILE_A, 6))
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), resolvedIssues, Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void movedResolvedIssuesWithDifferentMessagesArePruned() throws SonarHostException {
    Set<Issue> rawIssues =
        Set.of(buildIssue(FILE_A, "rk1", "local issue 1", new TextRange(6, 3, 6, 9)));

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssueBuilder()
                .withRuleKey("rk1")
                .withMessage("remote issue 1")
                .withRange(8, 3, 6, 9)
                .withStatus(IssueStatus.RESOLVED)
                .withHash(SonarHasher.hashFileLine(FILE_A, 6))
                .build());

    SonarHost host =
        new DummySonarHost(Collections.emptyMap(), resolvedIssues, Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void issuesWithNoMessageUseRuleName() throws SonarHostException {
    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk1", null, new TextRange(6, 3, 6, 9)));

    Map<String, String> ruleNameMap = Map.of("rk1", "Rule key 1");

    SonarHost host =
        new DummySonarHost(ruleNameMap, Collections.emptySet(), Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(2, issues.size());
    assertEquals("Rule key 1", issues.get(0).getMessage());
    assertEquals("Rule key 1", issues.get(1).getMessage());
  }

  @Test
  void issuesWithMessageUseMessage() throws SonarHostException {
    Set<Issue> rawIssues =
        Set.of(buildIssue(FILE_A, "rk1", "Issue message 1", new TextRange(6, 3, 6, 9)));

    Map<String, String> ruleNameMap = Map.of("rk1", "Rule key 1");

    SonarHost host =
        new DummySonarHost(ruleNameMap, Collections.emptySet(), Collections.emptySet());

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("Issue message 1", issues.get(0).getMessage());
  }

  static class DummySonarHost implements SonarHost {
    private final Map<String, String> _ruleNamesByRuleKey;
    private final Collection<RemoteIssue> _resolvedIssues;
    private final Collection<RemoteIssue> _unresolvedIssues;

    public DummySonarHost(
        Map<String, String> _ruleNamesByRuleKey,
        Collection<RemoteIssue> _resolvedIssues,
        Collection<RemoteIssue> _unresolvedIssues) {
      this._ruleNamesByRuleKey = _ruleNamesByRuleKey;
      this._resolvedIssues = _resolvedIssues;
      this._unresolvedIssues = _unresolvedIssues;
    }

    @Override
    public SonarCharacteristics getCharacteristics() {
      throw new NotImplementedException();
    }

    @Override
    public Map<String, String> getRuleNamesByRuleKey() {
      return _ruleNamesByRuleKey;
    }

    @Override
    public Collection<RemoteIssue> getResolvedIssues(Collection<String> relativeFilePaths) {
      return _resolvedIssues;
    }

    @Override
    public Collection<RemoteIssue> getUnresolvedIssues(Collection<String> relativeFilePaths) {
      return _unresolvedIssues;
    }

    @Override
    public Set<RemoteRule> getRules() {
      throw new NotImplementedException();
    }

    @Override
    public Set<RemoteActiveRule> getActiveRules() {
      throw new NotImplementedException();
    }

    @Override
    public Optional<Path> getPluginJar(String pluginKey) throws SonarHostException {
      throw new NotImplementedException();
    }

    @Override
    public Set<RemotePlugin> getDelphiPlugins() {
      throw new NotImplementedException();
    }

    @Override
    public String getName() {
      throw new NotImplementedException();
    }
  }
}
