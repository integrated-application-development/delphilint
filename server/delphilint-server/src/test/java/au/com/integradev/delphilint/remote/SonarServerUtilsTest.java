package au.com.integradev.delphilint.remote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import au.com.integradev.delphilint.analysis.DelphiIssue;
import au.com.integradev.delphilint.analysis.DelphiLintInputFile;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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

    SonarHost host = mock(SonarHost.class);

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

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

    SonarHost host = mock(SonarHost.class);

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertNull(issues.get(0).getMetadata());
  }

  @Test
  void unresolvedSecurityHotspotsPopulateMetadata() throws SonarHostException {
    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withType(RuleType.SECURITY_HOTSPOT)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("ACKNOWLEDGED")
                .withHashFrom(FILE_A)
                .withServerMetadata("user1", "creation date 1")
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getUnresolvedIssues(any(), any())).thenReturn(unresolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("user1", issues.get(0).getMetadata().getAssignee());
    assertEquals("creation date 1", issues.get(0).getMetadata().getCreationDate());
    assertEquals(IssueStatus.REVIEWED, issues.get(0).getMetadata().getStatus());
  }

  @Test
  void unresolvedIssuesPopulateMetadata() throws SonarHostException {
    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.CONFIRMED)
                .withResolution("resolution 1")
                .withHashFrom(FILE_A)
                .withServerMetadata("user1", "creation date 1")
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getUnresolvedIssues(any(), any())).thenReturn(unresolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("user1", issues.get(0).getMetadata().getAssignee());
    assertEquals("creation date 1", issues.get(0).getMetadata().getCreationDate());
    assertEquals(IssueStatus.CONFIRMED, issues.get(0).getMetadata().getStatus());
  }

  @ParameterizedTest
  @ValueSource(strings = {"OPEN", "CONFIRMED", "REOPENED", "TO_REVIEW"})
  void unresolvedIssuesAreNotPruned(String issueStatus) throws SonarHostException {
    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHashFrom(FILE_A)
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getUnresolvedIssues(any(), any())).thenReturn(unresolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
  }

  @ParameterizedTest
  @ValueSource(strings = {"RESOLVED", "ACCEPTED"})
  void resolvedIdenticalIssuesArePruned(String issueStatus) throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHashFrom(FILE_A)
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk2", "issue 2", new TextRange(9, 0, 20, 10)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("issue 2", issues.get(0).getMessage());
  }

  @Test
  void resolvedHotspotsArePruned() throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("FIXED")
                .withType(RuleType.SECURITY_HOTSPOT)
                .withHashFrom(FILE_A)
                .build(),
            new RemoteIssue.Builder()
                .withRuleKey("rk2")
                .withMessage("issue 2")
                .withRange(9, 0, 20, 10)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("SAFE")
                .withType(RuleType.SECURITY_HOTSPOT)
                .withHashFrom(FILE_A)
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk2", "issue 2", new TextRange(9, 0, 20, 10)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void acknowledgedHotspotsAreNotPruned() throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("ACKNOWLEDGED")
                .withHashFrom(FILE_A)
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk2", "issue 2", new TextRange(9, 0, 20, 10)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("issue 2", issues.get(0).getMessage());
  }

  @ParameterizedTest
  @ValueSource(strings = {"RESOLVED", "ACCEPTED"})
  void movedResolvedIssuesArePruned(String issueStatus) throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(8, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHash(SonarHasher.hashFileLine(FILE_A, 6))
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue(FILE_A, "rk1", "issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @ParameterizedTest
  @ValueSource(strings = {"RESOLVED", "ACCEPTED"})
  void movedResolvedIssuesWithDifferentMessagesArePruned(String issueStatus)
      throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("remote issue 1")
                .withRange(8, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHash(SonarHasher.hashFileLine(FILE_A, 6))
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(buildIssue(FILE_A, "rk1", "local issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void issuesWithNoMessageUseRuleName() throws SonarHostException {
    SonarHost host = mock(SonarHost.class);
    when(host.getRuleNamesByRuleKey()).thenReturn(Map.of("rk1", "Rule key 1"));

    Set<Issue> rawIssues =
        Set.of(
            buildIssue(FILE_A, "rk1", "", new TextRange(6, 3, 6, 9)),
            buildIssue(FILE_A, "rk1", null, new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(2, issues.size());
    assertEquals("Rule key 1", issues.get(0).getMessage());
    assertEquals("Rule key 1", issues.get(1).getMessage());
  }

  @Test
  void issuesWithMessageUseMessage() throws SonarHostException {
    SonarHost host = mock(SonarHost.class);
    when(host.getRuleNamesByRuleKey()).thenReturn(Map.of("rk1", "Rule key 1"));

    Set<Issue> rawIssues =
        Set.of(buildIssue(FILE_A, "rk1", "Issue message 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("Issue message 1", issues.get(0).getMessage());
  }

  @Test
  void providingNullTestPathsQueriesHost() throws SonarHostException {
    Set<String> allFilePaths = Set.of("a", "b", "c");
    Set<String> testFilePaths = Set.of("b", "c");
    Set<String> mainFilePaths = Set.of("a");

    SonarHost host = mock(SonarHost.class);
    when(host.getTestFilePaths()).thenReturn(testFilePaths);

    SonarServerUtils.postProcessIssues(allFilePaths, null, Collections.emptySet(), host);

    verify(host, times(1)).getTestFilePaths();
    verify(host, times(1)).getResolvedIssues(mainFilePaths, testFilePaths);
    verify(host, times(1)).getUnresolvedIssues(mainFilePaths, testFilePaths);
  }

  @Test
  void providingNonNullTestPathsDoesNotQueryHost() throws SonarHostException {
    Set<String> allFilePaths = Set.of("a", "b", "c");
    Set<String> testFilePaths = Set.of("b", "c");
    Set<String> mainFilePaths = Set.of("a");

    SonarHost host = mock(SonarHost.class);
    SonarServerUtils.postProcessIssues(allFilePaths, testFilePaths, Collections.emptySet(), host);

    verify(host, never()).getTestFilePaths();
    verify(host, times(1)).getResolvedIssues(mainFilePaths, testFilePaths);
    verify(host, times(1)).getUnresolvedIssues(mainFilePaths, testFilePaths);
  }

  @Test
  void onlyQueriedTestFilesInProvidedPathsAreUsed() throws SonarHostException {
    Set<String> allFilePaths = Set.of("a", "b", "c");
    Set<String> testFilePaths = Set.of("b", "c", "d");
    Set<String> mainFilePaths = Set.of("a");
    Set<String> includedTestFilePaths = Set.of("b", "c");

    SonarHost host = mock(SonarHost.class);
    when(host.getTestFilePaths()).thenReturn(testFilePaths);

    SonarServerUtils.postProcessIssues(allFilePaths, null, Collections.emptySet(), host);

    verify(host, times(1)).getTestFilePaths();
    verify(host, times(1)).getResolvedIssues(mainFilePaths, includedTestFilePaths);
    verify(host, times(1)).getUnresolvedIssues(mainFilePaths, includedTestFilePaths);
  }

  @Test
  void onlyProvidedTestFilesInProvidedPathsAreUsed() throws SonarHostException {
    Set<String> allFilePaths = Set.of("a", "b", "c");
    Set<String> testFilePaths = Set.of("b", "c", "d");
    Set<String> mainFilePaths = Set.of("a");
    Set<String> includedTestFilePaths = Set.of("b", "c");

    SonarHost host = mock(SonarHost.class);
    SonarServerUtils.postProcessIssues(allFilePaths, testFilePaths, Collections.emptySet(), host);

    verify(host, never()).getTestFilePaths();
    verify(host, times(1)).getResolvedIssues(mainFilePaths, includedTestFilePaths);
    verify(host, times(1)).getUnresolvedIssues(mainFilePaths, includedTestFilePaths);
  }

  @Test
  void queriesTestFilesIfBadRequest() throws SonarHostException {
    Set<String> allFilePaths = Set.of("a", "b", "c");

    Set<String> localTestFilePaths = Set.of("b", "c", "d");
    Set<String> localMainFilePaths = Set.of("a");
    Set<String> localIncludedTestFilePaths = Set.of("b", "c");

    Set<String> serverTestFilePaths = Set.of("a", "c", "d");
    Set<String> serverMainFilePaths = Set.of("b");
    Set<String> serverIncludedTestFilePaths = Set.of("a", "c");

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(localMainFilePaths, localIncludedTestFilePaths))
        .thenThrow(new SonarHostBadRequestException());
    when(host.getTestFilePaths()).thenReturn(serverTestFilePaths);

    SonarServerUtils.postProcessIssues(
        allFilePaths, localTestFilePaths, Collections.emptySet(), host);

    verify(host, times(1)).getResolvedIssues(localMainFilePaths, localIncludedTestFilePaths);
    verify(host, times(1)).getTestFilePaths();
    verify(host, times(1)).getResolvedIssues(serverMainFilePaths, serverIncludedTestFilePaths);
    verify(host, times(1)).getUnresolvedIssues(serverMainFilePaths, serverIncludedTestFilePaths);
  }
}
