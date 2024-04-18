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
package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.CleanCodeAttribute;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SonarQubeRule {
  @JsonProperty private String key;
  @JsonProperty private String name;
  @JsonProperty private String htmlDesc;
  @JsonProperty private IssueSeverity severity;
  @JsonProperty private RuleType type;
  @JsonProperty private CleanCodeAttribute cleanCodeAttribute;
  @JsonProperty private List<SonarQubeQualityImpact> impacts;

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public String getHtmlDesc() {
    return htmlDesc;
  }

  public IssueSeverity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }

  public CleanCodeAttribute getCleanCodeAttribute() {
    return cleanCodeAttribute;
  }

  public List<SonarQubeQualityImpact> getImpacts() {
    return impacts;
  }
}
