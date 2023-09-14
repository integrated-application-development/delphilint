package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;

public enum SoftwareQuality {
  SECURITY,
  RELIABILITY,
  MAINTAINABILITY;

  public static SoftwareQuality fromSonarLintSoftwareQuality(
      org.sonarsource.sonarlint.core.commons.SoftwareQuality quality) {
    switch (quality) {
      case MAINTAINABILITY:
        return MAINTAINABILITY;
      case RELIABILITY:
        return RELIABILITY;
      case SECURITY:
        return SECURITY;
      default:
        throw new NotImplementedException("Unknown software quality");
    }
  }
}
