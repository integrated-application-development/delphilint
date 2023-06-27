package au.com.integradev.delphilint.remote;

import java.util.Map;
import org.sonarsource.sonarlint.core.analysis.api.ActiveRule;

public class RemoteActiveRule {
  private final String ruleKey;
  private final String languageKey;
  private final String templateRuleKey;
  private final Map<String, String> params;

  public RemoteActiveRule(
      String ruleKey, String languageKey, String templateRuleKey, Map<String, String> params) {
    this.ruleKey = ruleKey;
    this.languageKey = languageKey;
    this.params = params;
    this.templateRuleKey = templateRuleKey;
  }

  public String getRuleKey() {
    return ruleKey;
  }

  public String getLanguageKey() {
    return languageKey;
  }

  public ActiveRule toSonarLintActiveRule() {
    var activeRule = new ActiveRule(ruleKey, languageKey);

    if (templateRuleKey != null) {
      activeRule.setTemplateRuleKey(templateRuleKey);
    }

    activeRule.setParams(params);

    return activeRule;
  }
}
