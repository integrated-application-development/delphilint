package au.com.integradev.delphilint.analysis;

import com.fasterxml.jackson.annotation.JsonProperty;

public class DelphiLintIssue {
  @JsonProperty private String ruleKey;
  @JsonProperty private String message;
  @JsonProperty private String file;

  public void setFile(String file) {
    this.file = file;
  }

  @JsonProperty private TextRange range;

  public DelphiLintIssue(String ruleKey, String message, String file, TextRange range) {
    this.ruleKey = ruleKey;
    this.message = message;
    this.file = file;
    this.range = range;
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

  public TextRange getRange() {
    return range;
  }
}
