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
package au.com.integradev.delphilint.analysis;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class SonarDelphiUtilsTest {
  @Test
  void testOnlyTakesFirstLine() {
    assertEquals(
        "An error was raised (Failed to construct DelphiFile (abc )",
        SonarDelphiUtils.convertSonarDelphiError("Failed to construct DelphiFile (abc \ndef ghi)"));
  }

  @Test
  void testConvertDelphiFileConstructionError() {
    assertEquals(
        "A Delphi file could not be parsed (abc def ghi)",
        SonarDelphiUtils.convertSonarDelphiError("Failed to construct DelphiFile (abc def ghi)"));
  }

  @Test
  void testConvertDelphiFileConstructionErrorStripsCarriageReturns() {
    assertEquals(
        "A Delphi file could not be parsed (foo bar baz)",
        SonarDelphiUtils.convertSonarDelphiError("Failed to construct DelphiFile (foo bar\r baz)"));
  }

  @Test
  void testConvertNullPointerException() {
    assertEquals(
        "A null pointer exception was raised",
        SonarDelphiUtils.convertSonarDelphiError("abcd NullPointerException efg"));
  }

  @Test
  void testConvertGenericException() {
    assertEquals(
        "An error was raised (abcd null efgh ijk)",
        SonarDelphiUtils.convertSonarDelphiError("abcd null efgh ijk"));
  }

  @Test
  void testConvertGenericExceptionDoesNotStripCarriageReturns() {
    assertEquals(
        "An error was raised (abcd null \r ijk)",
        SonarDelphiUtils.convertSonarDelphiError("abcd null \r ijk"));
  }

  @Test
  void testConvertPrefersDelphiFileConstructionError() {
    assertEquals(
        "A Delphi file could not be parsed (NullPointerException)",
        SonarDelphiUtils.convertSonarDelphiError(
            "Failed to construct DelphiFile (NullPointerException)"));
  }
}
