package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;
import org.sonar.api.batch.rule.Severity;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;

public enum RuleSeverity {
  INFO,
  MINOR,
  MAJOR,
  CRITICAL,
  BLOCKER;

  public static RuleSeverity fromSonarLintIssueSeverity(IssueSeverity severity) {
    switch (severity) {
      case INFO:
        return RuleSeverity.INFO;
      case MINOR:
        return RuleSeverity.MINOR;
      case MAJOR:
        return RuleSeverity.MAJOR;
      case BLOCKER:
        return RuleSeverity.BLOCKER;
      case CRITICAL:
        return RuleSeverity.CRITICAL;
      default:
        throw new NotImplementedException("Unknown issue severity");
    }
  }

  public static RuleSeverity fromSonarLintSeverity(Severity severity) {
    switch (severity) {
      case INFO:
        return RuleSeverity.INFO;
      case MINOR:
        return RuleSeverity.MINOR;
      case MAJOR:
        return RuleSeverity.MAJOR;
      case BLOCKER:
        return RuleSeverity.BLOCKER;
      case CRITICAL:
        return RuleSeverity.CRITICAL;
      default:
        throw new NotImplementedException("Unknown issue severity");
    }
  }
}
