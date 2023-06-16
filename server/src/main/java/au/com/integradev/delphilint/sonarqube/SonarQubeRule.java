package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.sonar.api.batch.rule.Severity;
import org.sonarsource.sonarlint.core.commons.RuleType;

public class SonarQubeRule {
  @JsonProperty private String key;
  @JsonProperty private String name;
  @JsonProperty private String htmlDesc;
  @JsonProperty private Severity severity;
  @JsonProperty private RuleType type;

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public String getHtmlDesc() {
    return htmlDesc;
  }

  public Severity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }
}
