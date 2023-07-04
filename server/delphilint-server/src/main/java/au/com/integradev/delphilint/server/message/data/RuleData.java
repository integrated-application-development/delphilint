package au.com.integradev.delphilint.server.message.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;

public class RuleData {
  @JsonProperty private String key;
  @JsonProperty private String name;
  @JsonProperty private String desc;
  @JsonProperty private RuleSeverity severity;
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

  public RuleSeverity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }

  public RuleData(RemoteRule rule) {
    key = rule.getKey();
    name = rule.getName();
    desc = rule.getHtmlDesc();
    severity = rule.getSeverity();
    type = rule.getType();
  }
}
