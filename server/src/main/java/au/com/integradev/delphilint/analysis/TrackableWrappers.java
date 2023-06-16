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
package au.com.integradev.delphilint.analysis;

import au.com.integradev.delphilint.sonarqube.SonarQubeIssue;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;
import org.sonarsource.sonarlint.core.commons.TextRangeWithHash;
import org.sonarsource.sonarlint.core.issuetracking.Trackable;

public class TrackableWrappers {
  private TrackableWrappers() {
    // Utility class
  }

  public static class ClientTrackable implements Trackable<Issue> {
    private final Issue issue;

    public ClientTrackable(Issue issue) {
      this.issue = issue;
    }

    @Override
    public Issue getClientObject() {
      return issue;
    }

    @Override
    public String getRuleKey() {
      return issue.getRuleKey();
    }

    @Override
    public IssueSeverity getSeverity() {
      return IssueSeverity.INFO;
    }

    @Override
    public String getMessage() {
      return issue.getMessage();
    }

    @Override
    public RuleType getType() {
      return RuleType.CODE_SMELL;
    }

    @Override
    public Integer getLine() {
      return issue.getStartLine();
    }

    @Override
    public String getLineHash() {
      return null;
    }

    @Override
    public TextRangeWithHash getTextRange() {
      return new TextRangeWithHash(
          issue.getStartLine(),
          issue.getStartLineOffset(),
          issue.getEndLine(),
          issue.getEndLineOffset(),
          getLineHash());
    }

    @Override
    public Long getCreationDate() {
      return null;
    }

    @Override
    public String getServerIssueKey() {
      return "";
    }

    @Override
    public boolean isResolved() {
      return false;
    }
  }

  public static class ServerTrackable implements Trackable<SonarQubeIssue> {
    private final SonarQubeIssue issue;

    public ServerTrackable(SonarQubeIssue issue) {
      this.issue = issue;
    }

    @Override
    public SonarQubeIssue getClientObject() {
      return issue;
    }

    @Override
    public String getRuleKey() {
      return issue.getRuleKey();
    }

    @Override
    public IssueSeverity getSeverity() {
      return issue.getSeverity();
    }

    @Override
    public String getMessage() {
      return issue.getMessage();
    }

    @Override
    public RuleType getType() {
      return issue.getType();
    }

    @Override
    public Integer getLine() {
      return issue.getLine();
    }

    @Override
    public String getLineHash() {
      return issue.getLineHash();
    }

    @Override
    public TextRangeWithHash getTextRange() {
      return new TextRangeWithHash(
          issue.getTextRange().getStartLine(),
          issue.getTextRange().getStartOffset(),
          issue.getTextRange().getEndLine(),
          issue.getTextRange().getEndOffset(),
          issue.getLineHash());
    }

    @Override
    public Long getCreationDate() {
      return null;
    }

    @Override
    public String getServerIssueKey() {
      return issue.getServerIssueKey();
    }

    @Override
    public boolean isResolved() {
      return true;
    }
  }
}
