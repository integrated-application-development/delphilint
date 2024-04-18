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
package au.com.integradev.delphilint.analysis;

import au.com.integradev.delphilint.remote.IssueStatus;
import org.sonarsource.sonarlint.core.analysis.api.Issue;

public class DelphiIssue {
  private String ruleKey;
  private String message;
  private String file;
  private TextRange range;
  private RemoteMetadata metadata;

  public DelphiIssue(Issue issue, RemoteMetadata metadata) {
    var textRange = issue.getTextRange();
    this.ruleKey = issue.getRuleKey();
    this.message = issue.getMessage();
    this.file = issue.getInputFile() == null ? "" : issue.getInputFile().relativePath();
    this.range =
        textRange == null
            ? null
            : new TextRange(
                textRange.getStartLine(),
                textRange.getStartLineOffset(),
                textRange.getEndLine(),
                textRange.getEndLineOffset());
    this.metadata = metadata;
  }

  public DelphiIssue(
      String ruleKey, String message, String file, TextRange range, RemoteMetadata metadata) {
    this.ruleKey = ruleKey;
    this.message = message;
    this.file = file;
    this.range = range;
    this.metadata = metadata;
  }

  public String getRuleKey() {
    return ruleKey;
  }

  public String getMessage() {
    return message;
  }

  public String getFile() {
    return file;
  }

  public void setFile(String file) {
    this.file = file;
  }

  public TextRange getTextRange() {
    return range;
  }

  public RemoteMetadata getMetadata() {
    return metadata;
  }

  public static class RemoteMetadata {
    private final String assignee;
    private final String creationDate;
    private final IssueStatus status;

    public RemoteMetadata(String assignee, String creationDate, IssueStatus status) {
      this.assignee = assignee;
      this.creationDate = creationDate;
      this.status = status;
    }

    public String getAssignee() {
      return assignee;
    }

    public String getCreationDate() {
      return creationDate;
    }

    public IssueStatus getStatus() {
      return status;
    }
  }
}
