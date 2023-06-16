package au.com.integradev.delphilint.server.message.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.sonarqube.SonarQubeRule;
import org.sonar.api.batch.rule.Severity;
import org.sonarsource.sonarlint.core.commons.RuleType;

public class RuleData {
  @JsonProperty private String key;
  @JsonProperty private String name;
  @JsonProperty private String desc;
  @JsonProperty private Severity severity;
  @JsonProperty private RuleType type;

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public String getDesc() {
    return desc;
  }

  public Severity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }

  public RuleData(SonarQubeRule rule) {
    key = rule.getKey();
    name = rule.getName();
    desc = rule.getHtmlDesc();
    severity = rule.getSeverity();
    type = rule.getType();
  }
}
