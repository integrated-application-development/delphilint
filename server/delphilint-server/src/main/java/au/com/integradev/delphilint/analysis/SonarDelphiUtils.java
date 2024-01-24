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
