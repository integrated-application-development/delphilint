/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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

  public Map<String, String> getParams() {
    return params;
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
