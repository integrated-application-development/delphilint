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
