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

import java.util.List;
import java.util.function.Function;

public class RemoteRuleDescription {
  private final String introductionSection;
  private final String rootCauseSection;
  private final String howToFixSection;
  private final String resourcesSection;

  public RemoteRuleDescription(
      String introductionSection,
      String rootCauseSection,
      String howToFixSection,
      String resourcesSection) {
    this.introductionSection = introductionSection;
    this.rootCauseSection = rootCauseSection;
    this.howToFixSection = howToFixSection;
    this.resourcesSection = resourcesSection;
  }

  public static <T> RemoteRuleDescription fromDescriptionSections(
      List<T> sections, Function<T, String> getKey, Function<T, String> getContent) {
    var introDesc = "";
    var causeDesc = "";
    var fixDesc = "";
    var resourcesDesc = "";

    for (var section : sections) {
      switch (getKey.apply(section)) {
        case "introduction":
          introDesc = getContent.apply(section);
          break;
        case "default":
        case "root_cause":
          causeDesc = getContent.apply(section);
          break;
        case "how_to_fix":
          fixDesc = getContent.apply(section);
          break;
        case "resources":
          resourcesDesc = getContent.apply(section);
          break;
        default: // Ignore other sections
      }
    }

    return new RemoteRuleDescription(introDesc, causeDesc, fixDesc, resourcesDesc);
  }

  public String getIntroductionSection() {
    return introductionSection;
  }

  public String getRootCauseSection() {
    return rootCauseSection;
  }

  public String getHowToFixSection() {
    return howToFixSection;
  }

  public String getResourcesSection() {
    return resourcesSection;
  }
}
