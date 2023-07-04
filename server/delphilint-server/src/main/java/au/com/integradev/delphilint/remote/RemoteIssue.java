package au.com.integradev.delphilint.remote;

import au.com.integradev.delphilint.analysis.TextRange;

public class RemoteIssue {
  private String key;
  private String rule;
  private int line;

  private String hash;
  private TextRange textRange;
  private String message;
  private RuleSeverity severity;
  private RuleType type;

  public RemoteIssue(
      String key,
      String rule,
      int line,
      String hash,
      TextRange textRange,
      String message,
      RuleSeverity severity,
      RuleType type) {
    this.key = key;
    this.rule = rule;
    this.line = line;
    this.hash = hash;
    this.textRange = textRange;
    this.message = message;
    this.severity = severity;
    this.type = type;
  }

  public String getRuleKey() {
    return rule;
  }

  public RuleSeverity getSeverity() {
    return severity;
  }

  public String getMessage() {
    return message;
  }

  public RuleType getType() {
    return type;
  }

  public Integer getLine() {
    return line;
  }

  public String getLineHash() {
    return hash;
  }

  public TextRange getTextRange() {
    if (textRange == null) {
      textRange = new TextRange();
    }
    return textRange;
  }

  public String getServerIssueKey() {
    return key;
  }
}
