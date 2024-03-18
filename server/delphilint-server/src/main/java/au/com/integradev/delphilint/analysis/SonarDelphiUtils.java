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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SonarDelphiUtils {
  private static final Pattern delphiFileConstructionRegex =
      Pattern.compile("^.*Failed to construct DelphiFile \\(([^)]*?)\\)$");

  private SonarDelphiUtils() {
    // Utility class
  }

  public static String convertSonarDelphiError(String sonarDelphiException) {
    String sonarDelphiError = sonarDelphiException.split("\n", 2)[0];

    Matcher delphiFileConstructionMatch = delphiFileConstructionRegex.matcher(sonarDelphiError);

    if (delphiFileConstructionMatch.matches()) {
      String antlrError = delphiFileConstructionMatch.replaceFirst("$1").replace("\r", "");
      return "A Delphi file could not be parsed (" + antlrError + ")";
    } else if (sonarDelphiError.contains("NullPointerException")) {
      return "A null pointer exception was raised";
    } else {
      return "An error was raised (" + sonarDelphiError + ")";
    }
  }
}
