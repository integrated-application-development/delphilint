package au.com.integradev.delphilint.remote;

import au.com.integradev.delphilint.remote.sonarqube.Version;

public class SonarCharacteristics {
  private static final Version FIRST_SUPPORTED_VERSION = new Version("7.9");
  private static final Version FIRST_CODE_ATTRIBUTES_VERSION = new Version("10.2");
  private static final Version FIRST_PLUGIN_REQUIRED_FOR_LANGUAGES_VERSION = new Version("10.4");
  private static final Version EARLIEST_ALL_FEATURES_VERSION =
      FIRST_PLUGIN_REQUIRED_FOR_LANGUAGES_VERSION;

  private final Version version;

  public SonarCharacteristics(Version version) {
    this.version = version;
  }

  public boolean usesCodeAttributes() {
    return version.compareTo(FIRST_CODE_ATTRIBUTES_VERSION) >= 0;
  }

  public boolean supportsPluginRequiredForLanguages() {
    return version.compareTo(FIRST_PLUGIN_REQUIRED_FOR_LANGUAGES_VERSION) >= 0;
  }

  public boolean requiresPluginRequiredForLanguagesHeuristic() {
    // Currently all versions use the heuristic as a backup method.
    return true;
  }

  public boolean isSupported() {
    return version.compareTo(FIRST_SUPPORTED_VERSION) >= 0;
  }

  public static SonarCharacteristics latest() {
    return new SonarCharacteristics(EARLIEST_ALL_FEATURES_VERSION);
  }
}
