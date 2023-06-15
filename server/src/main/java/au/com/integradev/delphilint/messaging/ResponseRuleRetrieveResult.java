package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.sonarqube.RuleInfo;
import java.util.Map;

public class ResponseRuleRetrieveResult {
  @JsonProperty private final Map<String, RuleInfo> rules;

  public ResponseRuleRetrieveResult(Map<String, RuleInfo> rules) {
    this.rules = rules;
  }

  public Map<String, RuleInfo> getRules() {
    return rules;
  }
}
