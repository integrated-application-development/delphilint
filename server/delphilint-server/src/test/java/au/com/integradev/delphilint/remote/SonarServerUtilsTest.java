/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
import au.com.integradev.delphilint.analysis.TextRange;
import java.nio.charset.StandardCharsets;
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

class SonarServerUtilsTest {
  private static final Path BASE_DIR =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote");
  private static final Path FILE_PATH =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote/Utf8File.pas");

  Issue buildIssue(String ruleKey, String message, TextRange range) {
    ClientInputFile file =
        new DelphiLintInputFile(BASE_DIR, Path.of("Utf8File.pas"), StandardCharsets.UTF_8);
    return new Issue(
        ruleKey,
        message,
        Collections.emptyMap(),
        toSonarLintTextRange(range),
        file,
        Collections.emptyList(),
        Collections.emptyList(),
        Optional.empty());
  }

  static org.sonarsource.sonarlint.core.commons.TextRange toSonarLintTextRange(
      TextRange textRange) {
    return new org.sonarsource.sonarlint.core.commons.TextRange(
        textRange.getStartLine(),
        textRange.getStartOffset(),
        textRange.getEndLine(),
        textRange.getEndOffset());
  }

  @Test
  void testIssuesAreConverted() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 12)));

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
    assertEquals(12, issues.get(0).getTextRange().getEndOffset());
  }

  @Test
  void testLocalIssuesHaveNoMetadata() throws SonarHostException {
    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 9)));

    SonarHost host = mock(SonarHost.class);

    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertNull(issues.get(0).getMetadata());
  }

  @Test
  void testUnresolvedSecurityHotspotsPopulateMetadata() throws SonarHostException {
    var textRange = new TextRange(6, 3, 6, 12);

    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(textRange)
                .withType(RuleType.SECURITY_HOTSPOT)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("ACKNOWLEDGED")
                .withHash(SonarHasher.hashFileRange(FILE_PATH, textRange))
                .withServerMetadata("user1", "creation date 1")
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getUnresolvedIssues(any(), any())).thenReturn(unresolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "issue 1", textRange));
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
  void testUnresolvedIssuesPopulateMetadata() throws SonarHostException {
    var textRange = new TextRange(6, 3, 6, 12);

    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(textRange)
                .withStatus(IssueStatus.CONFIRMED)
                .withResolution("resolution 1")
                .withHash(SonarHasher.hashFileRange(FILE_PATH, textRange))
                .withServerMetadata("user1", "creation date 1")
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getUnresolvedIssues(any(), any())).thenReturn(unresolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 12)));
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
  void testUnresolvedIssuesAreNotPruned(String issueStatus) throws SonarHostException {
    var textRange = new au.com.integradev.delphilint.analysis.TextRange(6, 3, 6, 12);

    Set<RemoteIssue> unresolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(textRange)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHash(SonarHasher.hashFileRange(FILE_PATH, textRange))
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getUnresolvedIssues(any(), any())).thenReturn(unresolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 12)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));
    assertEquals(1, issues.size());
  }

  @ParameterizedTest
  @ValueSource(strings = {"RESOLVED", "ACCEPTED"})
  void testResolvedIdenticalIssuesArePruned(String issueStatus) throws SonarHostException {
    var textRange = new au.com.integradev.delphilint.analysis.TextRange(6, 3, 6, 12);

    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(textRange)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHash(SonarHasher.hashFileRange(FILE_PATH, textRange))
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(
            buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 12)),
            buildIssue("rk2", "issue 2", new TextRange(9, 0, 20, 10)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("issue 2", issues.get(0).getMessage());
  }

  @Test
  void testResolvedHotspotsArePruned() throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("FIXED")
                .withType(RuleType.SECURITY_HOTSPOT)
                .withHash("abc")
                .build(),
            new RemoteIssue.Builder()
                .withRuleKey("rk2")
                .withMessage("issue 2")
                .withRange(9, 0, 20, 10)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("SAFE")
                .withType(RuleType.SECURITY_HOTSPOT)
                .withHash("abc")
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(
            buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue("rk2", "issue 2", new TextRange(9, 0, 20, 10)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void testAcknowledgedHotspotsAreNotPruned() throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(6, 3, 6, 9)
                .withStatus(IssueStatus.REVIEWED)
                .withResolution("ACKNOWLEDGED")
                .withHash("abc")
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues =
        Set.of(
            buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 9)),
            buildIssue("rk2", "issue 2", new TextRange(9, 0, 20, 10)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("issue 2", issues.get(0).getMessage());
  }

  @ParameterizedTest
  @ValueSource(strings = {"RESOLVED", "ACCEPTED"})
  void testMovedResolvedIssuesArePruned(String issueStatus) throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("issue 1")
                .withRange(8, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHash(SonarHasher.hashFileLine(FILE_PATH, 6))
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @ParameterizedTest
  @ValueSource(strings = {"RESOLVED", "ACCEPTED"})
  void testMovedResolvedIssuesWithDifferentMessagesArePruned(String issueStatus)
      throws SonarHostException {
    Set<RemoteIssue> resolvedIssues =
        Set.of(
            new RemoteIssue.Builder()
                .withRuleKey("rk1")
                .withMessage("remote issue 1")
                .withRange(8, 3, 6, 9)
                .withStatus(IssueStatus.fromSonarQubeIssueStatus(issueStatus))
                .withHash(SonarHasher.hashFileLine(FILE_PATH, 6))
                .build());

    SonarHost host = mock(SonarHost.class);
    when(host.getResolvedIssues(any(), any())).thenReturn(resolvedIssues);

    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "local issue 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(0, issues.size());
  }

  @Test
  void testIssuesWithNoMessageUseRuleName() throws SonarHostException {
    SonarHost host = mock(SonarHost.class);
    when(host.getRuleNamesByRuleKey()).thenReturn(Map.of("rk1", "Rule key 1"));

    Set<Issue> rawIssues =
        Set.of(
            buildIssue("rk1", "", new TextRange(6, 3, 6, 9)),
            buildIssue("rk1", null, new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(2, issues.size());
    assertEquals("Rule key 1", issues.get(0).getMessage());
    assertEquals("Rule key 1", issues.get(1).getMessage());
  }

  @Test
  void testIssuesWithMessageUseMessage() throws SonarHostException {
    SonarHost host = mock(SonarHost.class);
    when(host.getRuleNamesByRuleKey()).thenReturn(Map.of("rk1", "Rule key 1"));

    Set<Issue> rawIssues = Set.of(buildIssue("rk1", "Issue message 1", new TextRange(6, 3, 6, 9)));
    List<DelphiIssue> issues =
        new ArrayList<>(
            SonarServerUtils.postProcessIssues(
                Collections.emptySet(), Collections.emptySet(), rawIssues, host));

    assertEquals(1, issues.size());
    assertEquals("Issue message 1", issues.get(0).getMessage());
  }

  @Test
  void testProvidingNullTestPathsQueriesHost() throws SonarHostException {
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
  void testProvidingNonNullTestPathsDoesNotQueryHost() throws SonarHostException {
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
  void testOnlyQueriedTestFilesInProvidedPathsAreUsed() throws SonarHostException {
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
  void testOnlyProvidedTestFilesInProvidedPathsAreUsed() throws SonarHostException {
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
  void testQueriesTestFilesIfBadRequest() throws SonarHostException {
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
