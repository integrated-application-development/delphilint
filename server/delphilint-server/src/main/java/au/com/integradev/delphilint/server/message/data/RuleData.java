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
package au.com.integradev.delphilint.server.message.data;

import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;
import com.fasterxml.jackson.annotation.JsonProperty;

public class RuleData {
  @JsonProperty private String key;
  @JsonProperty private String name;
  @JsonProperty private RuleDescriptionData description;
  @JsonProperty private RuleSeverity severity;
  @JsonProperty private RuleType type;
  @JsonProperty private CleanCodeData cleanCode;

  public RuleData(RemoteRule rule) {
    key = rule.getKey();
    name = rule.getName();
    description = new RuleDescriptionData(rule.getRuleDescription());
    severity = rule.getSeverity();
    type = rule.getType();

    if (rule.getCleanCode() != null) {
      cleanCode = new CleanCodeData(rule.getCleanCode());
    }
  }

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public RuleDescriptionData getDescription() {
    return description;
  }

  public RuleSeverity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }
}
