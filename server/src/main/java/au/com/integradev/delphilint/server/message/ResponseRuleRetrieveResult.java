package au.com.integradev.delphilint.server.message;

import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.server.message.data.RuleData;
import java.util.Map;

public class ResponseRuleRetrieveResult {
  @JsonProperty private final Map<String, RuleData> rules;

  public ResponseRuleRetrieveResult(Map<String, RuleData> rules) {
    this.rules = rules;
  }

  public Map<String, RuleData> getRules() {
    return rules;
  }
}
