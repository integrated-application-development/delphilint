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

import au.com.integradev.delphilint.analysis.TextRange;
import com.fasterxml.jackson.annotation.JsonProperty;

public class IssueData {
  @JsonProperty private String ruleKey;
  @JsonProperty private String message;
  @JsonProperty private String file;
  @JsonProperty private TextRange range;
  @JsonProperty private IssueMetadataData metadata;

  public IssueData(
      String ruleKey, String message, String file, TextRange range, IssueMetadataData metadata) {
    this.ruleKey = ruleKey;
    this.message = message;
    this.file = file;
    this.range = range;
    this.metadata = metadata;
  }

  public String getRuleKey() {
    return ruleKey;
  }

  public String getMessage() {
    return message;
  }

  public String getFile() {
    return file;
  }

  public void setFile(String file) {
    this.file = file;
  }

  public TextRange getRange() {
    return range;
  }
}
