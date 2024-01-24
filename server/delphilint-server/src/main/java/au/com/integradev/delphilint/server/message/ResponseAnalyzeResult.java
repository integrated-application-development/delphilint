/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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
import au.com.integradev.delphilint.analysis.TextRange;
import au.com.integradev.delphilint.server.message.data.IssueData;
import au.com.integradev.delphilint.server.message.data.IssueMetadataData;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

public final class ResponseAnalyzeResult {
  @JsonProperty private Set<IssueData> issues;

  private ResponseAnalyzeResult(Set<IssueData> issues) {
    this.issues = issues;
  }

  public void convertPathsToAbsolute(Path baseDir) {
    issues.forEach(issue -> issue.setFile(baseDir.resolve(issue.getFile()).toString()));
  }

  public static ResponseAnalyzeResult fromIssueSet(Collection<DelphiIssue> delphiIssues) {
    Set<IssueData> issues =
        delphiIssues.stream()
            .map(
                delphiIssue -> {
                  TextRange range = null;
                  if (delphiIssue.getTextRange() != null) {
                    range =
                        new TextRange(
                            delphiIssue.getTextRange().getStartLine(),
                            delphiIssue.getTextRange().getStartOffset(),
                            delphiIssue.getTextRange().getEndLine(),
                            delphiIssue.getTextRange().getEndOffset());
                  }

                  IssueMetadataData metadata = null;
                  if (delphiIssue.getMetadata() != null) {
                    metadata =
                        new IssueMetadataData(
                            delphiIssue.getMetadata().getAssignee(),
                            delphiIssue.getMetadata().getCreationDate(),
                            delphiIssue.getMetadata().getStatus());
                  }

                  return new IssueData(
                      delphiIssue.getRuleKey(),
                      delphiIssue.getMessage(),
                      delphiIssue.getFile(),
                      range,
                      metadata);
                })
            .collect(Collectors.toSet());

    return new ResponseAnalyzeResult(issues);
  }
}
