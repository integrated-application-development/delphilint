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
package au.com.integradev.delphilint.remote;

import au.com.integradev.delphilint.analysis.TextRange;

public class RemoteIssue {
  private final String key;
  private final String rule;
  private final int line;
  private final String hash;
  private TextRange textRange;
  private final String message;
  private final RuleSeverity severity;
  private final RuleType type;
  private final IssueStatus status;
  private final boolean isSecurityHotspot;
  private final String assignee;
  private final String creationDate;

  public RemoteIssue(
      String key,
      String rule,
      int line,
      String hash,
      TextRange textRange,
      String message,
      RuleSeverity severity,
      RuleType type,
      IssueStatus status,
      boolean isSecurityHotspot,
      String assignee,
      String creationDate) {
    this.key = key;
    this.rule = rule;
    this.line = line;
    this.hash = hash;
    this.textRange = textRange;
    this.message = message;
    this.severity = severity;
    this.type = type;
    this.status = status;
    this.isSecurityHotspot = isSecurityHotspot;
    this.assignee = assignee;
    this.creationDate = creationDate;
  }

  public String getRuleKey() {
    return rule;
  }

  public RuleSeverity getSeverity() {
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

  public IssueStatus getStatus() {
    return status;
  }

  public boolean isSecurityHotspot() {
    return isSecurityHotspot;
  }

  public String getAssignee() {
    return assignee;
  }

  public String getCreationDate() {
    return creationDate;
  }
}
