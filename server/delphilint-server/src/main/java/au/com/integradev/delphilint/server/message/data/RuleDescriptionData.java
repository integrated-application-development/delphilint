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

import au.com.integradev.delphilint.remote.RemoteRuleDescription;
import com.fasterxml.jackson.annotation.JsonProperty;

public class RuleDescriptionData {
  @JsonProperty private String introduction;
  @JsonProperty private String rootCause;
  @JsonProperty private String howToFix;
  @JsonProperty private String resources;

  public RuleDescriptionData(RemoteRuleDescription ruleDescription) {
    introduction = ruleDescription.getIntroductionSection();
    rootCause = ruleDescription.getRootCauseSection();
    howToFix = ruleDescription.getHowToFixSection();
    resources = ruleDescription.getResourcesSection();
  }

  public String getIntroduction() {
    return introduction;
  }

  public String getRootCause() {
    return rootCause;
  }

  public String getHowToFix() {
    return howToFix;
  }

  public String getResources() {
    return resources;
  }
}
