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

public class RemoteRule {
  private final String key;
  private final String name;
  private final String htmlDesc;
  private final RuleSeverity severity;
  private final RuleType type;
  private final RemoteCleanCode defaultCleanCode;

  public RemoteRule(
      String key,
      String name,
      String htmlDesc,
      RuleSeverity severity,
      RuleType type,
      RemoteCleanCode cleanCode) {
    this.key = key;
    this.name = name;
    this.htmlDesc = htmlDesc;
    this.severity = severity;
    this.type = type;
    this.defaultCleanCode = cleanCode;
  }

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public String getHtmlDesc() {
    return htmlDesc;
  }

  public RuleSeverity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }

  public RemoteCleanCode getCleanCode() {
    return defaultCleanCode;
  }
}
