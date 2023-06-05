package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.analysis.TextRange;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ServerIssue {
  @JsonProperty private String key;
  @JsonProperty private String rule;
  @JsonProperty private String project;
  @JsonProperty private int line;
  @JsonProperty private String hash;
  @JsonProperty private TextRange textRange;
  @JsonProperty private String resolution;
  @JsonProperty private String status;
  @JsonProperty private String message;
  @JsonProperty private IssueSeverity severity;
  @JsonProperty private RuleType type;

  public String getRuleKey() {
    return rule;
  }

  public IssueSeverity getSeverity() {
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
    return textRange;
  }

  public String getServerIssueKey() {
    return key;
  }
}
