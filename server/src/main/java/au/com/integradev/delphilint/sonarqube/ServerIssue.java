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
package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.analysis.TextRange;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ServerIssue {
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

  public String getRuleKey() {
    return rule;
  }

  public IssueSeverity getSeverity() {
    return severity;
  }

  public String getMessage() {
    return message;
  }

  public RuleType getType() {
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
}
