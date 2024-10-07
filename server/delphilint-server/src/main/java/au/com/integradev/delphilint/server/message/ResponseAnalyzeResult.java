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
package au.com.integradev.delphilint.server.message;

import au.com.integradev.delphilint.analysis.DelphiIssue;
import au.com.integradev.delphilint.analysis.DelphiQuickFix;
import au.com.integradev.delphilint.analysis.TextRange;
import au.com.integradev.delphilint.server.message.data.IssueData;
import au.com.integradev.delphilint.server.message.data.IssueMetadataData;
import au.com.integradev.delphilint.server.message.data.QuickFixData;
import au.com.integradev.delphilint.server.message.data.TextEditData;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public final class ResponseAnalyzeResult {
  @JsonProperty private Set<IssueData> issues;
  @JsonProperty private List<String> logMessages;

  private ResponseAnalyzeResult(Set<IssueData> issues, List<String> logMessages) {
    this.issues = issues;
    this.logMessages = logMessages;
  }

  public void convertPathsToAbsolute(Path baseDir) {
    issues.forEach(issue -> issue.setFile(baseDir.resolve(issue.getFile()).toString()));
  }

  public static ResponseAnalyzeResult fromIssueSet(
      Collection<DelphiIssue> delphiIssues, List<String> logMessages) {
    Set<IssueData> issues =
        delphiIssues.stream()
            .map(
                delphiIssue ->
                    new IssueData(
                        delphiIssue.getRuleKey(),
                        delphiIssue.getMessage(),
                        delphiIssue.getFile(),
                        transformRange(delphiIssue.getTextRange()),
                        transformMetadata(delphiIssue.getMetadata()),
                        transformQuickFixes(delphiIssue.getQuickFixes())))
            .collect(Collectors.toSet());

    return new ResponseAnalyzeResult(issues, logMessages);
  }

  private static TextRange transformRange(TextRange range) {
    if (range == null) {
      return null;
    }

    return new TextRange(
        range.getStartLine(), range.getStartOffset(), range.getEndLine(), range.getEndOffset());
  }

  private static IssueMetadataData transformMetadata(DelphiIssue.RemoteMetadata metadata) {
    if (metadata == null) {
      return null;
    }

    return new IssueMetadataData(
        metadata.getAssignee(), metadata.getCreationDate(), metadata.getStatus());
  }

  private static List<QuickFixData> transformQuickFixes(List<DelphiQuickFix> quickFixes) {
    return quickFixes.stream()
        .map(
            quickFix ->
                new QuickFixData(
                    quickFix.message(),
                    quickFix.textEdits().stream()
                        .map(edit -> new TextEditData(edit.replacement(), edit.range()))
                        .collect(Collectors.toList())))
        .collect(Collectors.toList());
  }
}
