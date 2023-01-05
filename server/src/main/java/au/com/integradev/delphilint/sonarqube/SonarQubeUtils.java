package au.com.integradev.delphilint.sonarqube;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;
import org.sonarsource.sonarlint.core.analysis.api.Issue;

public class SonarQubeUtils {
  private SonarQubeUtils() {
    // utility class
  }

  public static Set<Issue> populateIssueMessages(SonarQubeConnection sq, Collection<Issue> issues) {
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
                    oldIssue.quickFixes());
              } else {
                return oldIssue;
              }
            })
        .collect(Collectors.toSet());
  }
}
