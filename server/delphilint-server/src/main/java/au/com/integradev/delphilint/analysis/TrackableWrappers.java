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

import au.com.integradev.delphilint.remote.RemoteIssue;
import au.com.integradev.delphilint.remote.SonarHasher;
import java.nio.file.Path;
import java.util.regex.Pattern;
import org.apache.commons.lang3.NotImplementedException;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.commons.HotspotReviewStatus;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;
import org.sonarsource.sonarlint.core.commons.TextRangeWithHash;
import org.sonarsource.sonarlint.core.issuetracking.Trackable;

public class TrackableWrappers {
  private TrackableWrappers() {
    // Utility class
  }

  public static class ClientTrackable implements Trackable<Issue> {
    private static final Pattern MATCH_ALL_WHITESPACES = Pattern.compile("\\s");

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
      if (issue.getTextRange() != null && issue.getInputFile() != null) {
        return SonarHasher.hashFileRange(
            Path.of(issue.getInputFile().uri()), new TextRange(issue.getTextRange()));
      } else {
        return null;
      }
    }

    @Override
    public TextRangeWithHash getTextRange() {
      if (issue.getStartLine() == null) {
        return null;
      }

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

    @Override
    public HotspotReviewStatus getReviewStatus() {
      return null;
    }
  }

  public static class ServerTrackable implements Trackable<RemoteIssue> {
    private final RemoteIssue issue;

    public ServerTrackable(RemoteIssue issue) {
      this.issue = issue;
    }

    @Override
    public RemoteIssue getClientObject() {
      return issue;
    }

    @Override
    public String getRuleKey() {
      return issue.getRuleKey();
    }

    @Override
    public IssueSeverity getSeverity() {
      switch (issue.getSeverity()) {
        case INFO:
          return IssueSeverity.INFO;
        case MINOR:
          return IssueSeverity.MINOR;
        case MAJOR:
          return IssueSeverity.MAJOR;
        case BLOCKER:
          return IssueSeverity.BLOCKER;
        case CRITICAL:
          return IssueSeverity.CRITICAL;
        default:
          throw new NotImplementedException("Unknown issue severity");
      }
    }

    @Override
    public String getMessage() {
      return issue.getMessage();
    }

    @Override
    public RuleType getType() {
      switch (issue.getType()) {
        case BUG:
          return RuleType.BUG;
        case CODE_SMELL:
          return RuleType.CODE_SMELL;
        case VULNERABILITY:
          return RuleType.VULNERABILITY;
        case SECURITY_HOTSPOT:
          return RuleType.SECURITY_HOTSPOT;
        default:
          throw new NotImplementedException("Unknown rule type");
      }
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
      return "";
    }

    @Override
    public boolean isResolved() {
      return true;
    }

    @Override
    public HotspotReviewStatus getReviewStatus() {
      return null;
    }
  }
}
