package au.com.integradev.delphilint.analysis;

import au.com.integradev.delphilint.sonarqube.ServerIssue;
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

  public static class ServerTrackable implements Trackable<ServerIssue> {
    private final ServerIssue issue;

    public ServerTrackable(ServerIssue issue) {
      this.issue = issue;
    }

    @Override
    public ServerIssue getClientObject() {
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
