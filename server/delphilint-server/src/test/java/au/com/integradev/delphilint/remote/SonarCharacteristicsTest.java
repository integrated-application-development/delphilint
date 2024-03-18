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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import au.com.integradev.delphilint.remote.sonarqube.Version;
import org.junit.jupiter.api.Test;

class SonarCharacteristicsTest {
  private SonarCharacteristics characteristicsFor(String versionStr) {
    return new SonarCharacteristics(new Version(versionStr));
  }

  @Test
  void testUsesCodeAttributes() {
    assertFalse(characteristicsFor("10.1").usesCodeAttributes());
    assertTrue(characteristicsFor("10.2").usesCodeAttributes());
    assertTrue(characteristicsFor("10.3").usesCodeAttributes());
  }

  @Test
  void testSupportsPluginRequiredForLanguages() {
    assertFalse(characteristicsFor("10.3").supportsPluginRequiredForLanguages());
    assertTrue(characteristicsFor("10.4").supportsPluginRequiredForLanguages());
    assertTrue(characteristicsFor("10.5").supportsPluginRequiredForLanguages());
  }

  @Test
  void testRequiresPluginRequiredForLanguagesHeuristic() {
    assertTrue(characteristicsFor("10.3").requiresPluginRequiredForLanguagesHeuristic());
    assertTrue(characteristicsFor("10.4").requiresPluginRequiredForLanguagesHeuristic());
    assertTrue(characteristicsFor("10.5").requiresPluginRequiredForLanguagesHeuristic());
    assertTrue(characteristicsFor("11.5").requiresPluginRequiredForLanguagesHeuristic());
  }

  @Test
  void testHotspotsSearchProjectKeyDeprecated() {
    assertFalse(characteristicsFor("10.3").hotspotsSearchProjectKeyDeprecated());
    assertTrue(characteristicsFor("10.4").hotspotsSearchProjectKeyDeprecated());
    assertTrue(characteristicsFor("10.5").hotspotsSearchProjectKeyDeprecated());
  }

  @Test
  void testIssuesSearchComponentKeysDeprecated() {
    assertFalse(characteristicsFor("10.1").issuesSearchComponentKeysDeprecated());
    assertTrue(characteristicsFor("10.2").issuesSearchComponentKeysDeprecated());
    assertTrue(characteristicsFor("10.3").issuesSearchComponentKeysDeprecated());
  }

  @Test
  void testIsSupported() {
    assertFalse(characteristicsFor("7.8").isSupported());
    assertTrue(characteristicsFor("7.9").isSupported());
    assertTrue(characteristicsFor("8.0").isSupported());
  }
}
