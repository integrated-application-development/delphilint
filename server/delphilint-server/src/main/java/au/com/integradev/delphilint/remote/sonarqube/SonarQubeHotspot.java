package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.analysis.TextRange;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SonarQubeHotspot {
  @JsonProperty private String key;
  @JsonProperty private String ruleKey;
  @JsonProperty private String message;
  @JsonProperty private String status;
  @JsonProperty private String resolution;
  @JsonProperty private TextRange textRange;
  @JsonProperty private int line;
  @JsonProperty private String assignee;
  @JsonProperty private String creationDate;

  public int getLine() {
    return line;
  }

  public String getKey() {
    return key;
  }

  public String getRuleKey() {
    return ruleKey;
  }

  public String getMessage() {
    return message;
  }

  public String getStatus() {
    return status;
  }

  public String getResolution() {
    return resolution;
  }

  public TextRange getTextRange() {
    return textRange;
  }

  public String getAssignee() {
    return assignee;
  }

  public String getCreationDate() {
    return creationDate;
  }
}
