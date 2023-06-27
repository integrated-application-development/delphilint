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

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;
import org.sonarsource.sonarlint.core.analysis.api.Issue;

public class SonarQubeUtils {
  private SonarQubeUtils() {
    // utility class
  }

  public static Set<Issue> populateIssueMessages(SonarQubeConnection sq, Collection<Issue> issues)
      throws ApiException {
    var ruleNameMap = sq.getRuleNamesByRuleKey();

    return issues.stream()
        .map(
            oldIssue -> {
              if (oldIssue.getMessage() == null || oldIssue.getMessage().isEmpty()) {
                return new Issue(
                    oldIssue.getRuleKey(),
                    ruleNameMap.get(oldIssue.getRuleKey()),
                    oldIssue.getTextRange(),
                    oldIssue.getInputFile(),
                    oldIssue.flows(),
                    oldIssue.quickFixes(),
                    oldIssue.getRuleDescriptionContextKey());
              } else {
                return oldIssue;
              }
            })
        .collect(Collectors.toSet());
  }
}
