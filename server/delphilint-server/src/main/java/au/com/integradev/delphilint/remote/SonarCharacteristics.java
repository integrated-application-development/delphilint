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

import au.com.integradev.delphilint.remote.sonarqube.Version;

public class SonarCharacteristics {
  private static final Version FIRST_SUPPORTED_VERSION = new Version("7.9");
  private static final Version FIRST_CODE_ATTRIBUTES_VERSION = new Version("10.2");
  private static final Version FIRST_PLUGIN_REQUIRED_FOR_LANGUAGES_VERSION = new Version("10.4");
  private static final Version EARLIEST_ALL_FEATURES_VERSION =
      FIRST_PLUGIN_REQUIRED_FOR_LANGUAGES_VERSION;
  private static final Version VERSION_10_2 = new Version("10.2");
  private static final Version VERSION_10_4 = new Version("10.4");

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

  public boolean hotspotsSearchProjectKeyDeprecated() {
    return version.compareTo(VERSION_10_4) >= 0;
  }

  public boolean issuesSearchComponentKeysDeprecated() {
    return version.compareTo(VERSION_10_2) >= 0;
  }

  public boolean isSupported() {
    return version.compareTo(FIRST_SUPPORTED_VERSION) >= 0;
  }

  public static SonarCharacteristics latest() {
    return new SonarCharacteristics(EARLIEST_ALL_FEATURES_VERSION);
  }
}
