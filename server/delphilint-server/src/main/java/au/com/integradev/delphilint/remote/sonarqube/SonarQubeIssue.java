/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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
import java.util.List;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SonarQubeIssue implements SonarQubeIssueLike {
  @JsonProperty private String key;
  @JsonProperty private String rule;
  @JsonProperty private String project;

  @JsonProperty(defaultValue = "-1")
  private int line;

  @JsonProperty private String hash;
  @JsonProperty private TextRange textRange;
  @JsonProperty private String resolution;
  @JsonProperty private String status;
  @JsonProperty private String message;
  @JsonProperty private IssueSeverity severity;
  @JsonProperty private RuleType type;
  @JsonProperty private String assignee;
  @JsonProperty private String creationDate;
  @JsonProperty private CleanCodeAttribute cleanCodeAttribute;
  @JsonProperty private List<SonarQubeQualityImpact> impacts;

  public String getRuleKey() {
    return rule;
  }

  public IssueSeverity getSeverity() {
    return severity;
  }

  public String getMessage() {
    return message;
  }

  public RuleType getIssueType() {
    return type;
  }

  public Integer getLine() {
    return line;
  }

  public String getLineHash() {
    return hash;
  }

  public TextRange getTextRange() {
    if (textRange == null) {
      textRange = new TextRange();
    }
    return textRange;
  }

  public String getServerIssueKey() {
    return key;
  }

  public String getProject() {
    return project;
  }

  public String getResolution() {
    return resolution;
  }

  public String getStatus() {
    return status;
  }

  public String getAssignee() {
    return assignee;
  }

  public String getCreationDate() {
    return creationDate;
  }

  public CleanCodeAttribute getCleanCodeAttribute() {
    return cleanCodeAttribute;
  }

  public List<SonarQubeQualityImpact> getImpacts() {
    return impacts;
  }

  public IssueLikeType getLikeType() {
    return IssueLikeType.ISSUE;
  }
}
