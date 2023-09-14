package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;

public enum ImpactSeverity {
  LOW,
  MEDIUM,
  HIGH;

  public static ImpactSeverity fromSonarLintImpactSeverity(
      org.sonarsource.sonarlint.core.commons.ImpactSeverity severity) {
    switch (severity) {
      case LOW:
        return LOW;
      case MEDIUM:
        return MEDIUM;
      case HIGH:
        return HIGH;
      default:
        throw new NotImplementedException("Unknown impact severity");
    }
  }
}
