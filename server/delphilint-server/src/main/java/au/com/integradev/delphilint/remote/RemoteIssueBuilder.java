package au.com.integradev.delphilint.remote;

import au.com.integradev.delphilint.analysis.TextRange;
import java.nio.file.Path;
import java.util.Map;

public class RemoteIssueBuilder {
  private String rule = "";
  private int line = -1;
  private String hash = null;
  private Path hashPath = null;
  private TextRange textRange = null;
  private String message = "";
  private RuleSeverity severity = RuleSeverity.MAJOR;
  private RuleType type = RuleType.CODE_SMELL;
  private IssueStatus status = IssueStatus.OPEN;
  private IssueLikeType likeType = IssueLikeType.ISSUE;
  private String assignee = "";
  private String creationDate = "";
  private String resolution = "";
  private RemoteCleanCode cleanCode = null;

  public RemoteIssueBuilder() {}

  public RemoteIssueBuilder withRuleKey(String ruleKey) {
    this.rule = ruleKey;
    return this;
  }

  public RemoteIssueBuilder withMessage(String message) {
    this.message = message;
    return this;
  }

  public RemoteIssueBuilder withRange(TextRange textRange) {
    this.textRange = textRange;
    if (textRange != null) {
      this.line = textRange.getStartLine();
    }
    return this;
  }

  public RemoteIssueBuilder withHash(String hash) {
    this.hash = hash;
    return this;
  }

  public RemoteIssueBuilder withHashFrom(Path filePath) {
    this.hashPath = filePath;
    return this;
  }

  public RemoteIssueBuilder withRange(int startLine, int startOffset, int endLine, int endOffset) {
    this.textRange = new TextRange(startLine, startOffset, endLine, endOffset);
    this.line = textRange.getStartLine();
    return this;
  }

  public RemoteIssueBuilder withSeverity(RuleSeverity severity) {
    this.severity = severity;
    return this;
  }

  public RemoteIssueBuilder withType(RuleType type) {
    this.type = type;
    return this;
  }

  public RemoteIssueBuilder withCleanCode(
      CleanCodeAttribute attribute, Map<SoftwareQuality, ImpactSeverity> impactedQualities) {
    this.cleanCode = new RemoteCleanCode(attribute, impactedQualities);
    return this;
  }

  public RemoteIssueBuilder withLikeType(IssueLikeType likeType) {
    this.likeType = likeType;
    return this;
  }

  public RemoteIssueBuilder withStatus(IssueStatus status) {
    this.status = status;
    return this;
  }

  public RemoteIssueBuilder withServerMetadata(String assignee, String creationDate) {
    this.assignee = assignee;
    this.creationDate = creationDate;
    return this;
  }

  public RemoteIssueBuilder withResolution(String resolution) {
    this.resolution = resolution;
    return this;
  }

  public RemoteIssue build() {
    if (hash == null && hashPath != null) {
      if (textRange == null) {
        throw new IllegalStateException(
            "Cannot build hash from file as text range has not been provided");
      }

      hash = SonarHasher.hashFileRange(hashPath, textRange);
    }

    return new RemoteIssue(
        rule,
        line,
        hash,
        textRange,
        message,
        severity,
        type,
        cleanCode,
        status,
        likeType,
        assignee,
        creationDate,
        resolution);
  }
}
