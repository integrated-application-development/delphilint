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

import au.com.integradev.delphilint.analysis.TextRange;
import au.com.integradev.delphilint.remote.CleanCodeAttribute;
import au.com.integradev.delphilint.remote.IssueLikeType;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SonarQubeHotspot implements SonarQubeIssueLike {
  @JsonProperty private String key;
  @JsonProperty private String ruleKey;
  @JsonProperty private String message;
  @JsonProperty private String status;
  @JsonProperty private String resolution;
  @JsonProperty private TextRange textRange;
  @JsonProperty private int line;
  @JsonProperty private String assignee;
  @JsonProperty private String creationDate;

  public int getLine() {
    return line;
  }

  public String getKey() {
    return key;
  }

  public String getRuleKey() {
    return ruleKey;
  }

  public String getMessage() {
    return message;
  }

  public String getStatus() {
    return status;
  }

  public String getResolution() {
    return resolution;
  }

  public TextRange getTextRange() {
    return textRange;
  }

  public String getAssignee() {
    return assignee;
  }

  public String getCreationDate() {
    return creationDate;
  }

  public IssueSeverity getSeverity() {
    return IssueSeverity.MAJOR;
  }

  public RuleType getIssueType() {
    return RuleType.SECURITY_HOTSPOT;
  }

  public CleanCodeAttribute getCleanCodeAttribute() {
    return null;
  }

  public List<SonarQubeQualityImpact> getImpacts() {
    return Collections.emptyList();
  }

  public IssueLikeType getLikeType() {
    return IssueLikeType.SECURITY_HOTSPOT;
  }
}
