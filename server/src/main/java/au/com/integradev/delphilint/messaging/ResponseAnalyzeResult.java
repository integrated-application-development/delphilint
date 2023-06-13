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
package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.analysis.DelphiLintIssue;
import au.com.integradev.delphilint.analysis.TextRange;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Collectors;

public class ResponseAnalyzeResult {
  @JsonProperty private Set<DelphiLintIssue> issues;

  private ResponseAnalyzeResult(Set<DelphiLintIssue> issues) {
    this.issues = issues;
  }

  public void convertPathsToAbsolute(Path baseDir) {
    issues.forEach(issue -> issue.setFile(baseDir.resolve(issue.getFile()).toString()));
  }

  public static ResponseAnalyzeResult fromIssueSet(
      Set<org.sonarsource.sonarlint.core.analysis.api.Issue> sonarIssues) {
    Set<DelphiLintIssue> issues =
        sonarIssues.stream()
            .map(
                sonarIssue -> {
                  TextRange range = null;
                  if (sonarIssue.getTextRange() != null) {
                    range =
                        new TextRange(
                            sonarIssue.getTextRange().getStartLine(),
                            sonarIssue.getTextRange().getStartLineOffset(),
                            sonarIssue.getTextRange().getEndLine(),
                            sonarIssue.getTextRange().getEndLineOffset());
                  }

                  String path = "";
                  if (sonarIssue.getInputFile() != null) {
                    path = sonarIssue.getInputFile().relativePath();
                  }

                  return new DelphiLintIssue(
                      sonarIssue.getRuleKey(), sonarIssue.getMessage(), path, range);
                })
            .collect(Collectors.toSet());

    return new ResponseAnalyzeResult(issues);
  }
}
