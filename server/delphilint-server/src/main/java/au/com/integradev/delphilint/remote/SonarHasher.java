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

import au.com.integradev.delphilint.analysis.TextRange;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/** Utility class emulating SonarQube's issue hashing functionality. */
public final class SonarHasher {
  private static final Logger LOG = LogManager.getLogger(SonarHasher.class);
  private static final Pattern MATCH_ALL_WHITESPACES = Pattern.compile("\\s");

  private SonarHasher() {
    // Utility class
  }

  public static String hashFileRange(Path filePath, TextRange textRange) {
    return hashFileLine(filePath, textRange.getStartLine(), StandardCharsets.UTF_8);
  }

  public static String hashFileRange(Path filePath, TextRange textRange, Charset charset) {
    return hashFileLine(filePath, textRange.getStartLine(), charset);
  }

  public static String hashFileLine(Path filePath, int lineNum) {
    return hashFileLine(filePath, lineNum, StandardCharsets.UTF_8);
  }

  public static String hashFileLine(Path filePath, int lineNum, Charset charset) {
    try (BufferedReader reader =
        new BufferedReader(new InputStreamReader(Files.newInputStream(filePath), charset))) {

      String line = reader.readLine();
      if (line == null) {
        return null;
      }
      int i = 1;

      String textToHash = "";
      do {
        // The SonarQube source code suggests that it hashes only the characters in
        // the text range. Experimental results have shown this not to be the case -
        // an issue's "line hash" is a hash of its start line.
        if (lineNum == i) {
          textToHash = line;
        }

        line = reader.readLine();
        i++;
      } while (line != null);

      return hash(textToHash);
    } catch (IOException e) {
      LOG.error(e);
      return null;
    }
  }

  public static String hash(String text) {
    Matcher matchAllWhitespaces = MATCH_ALL_WHITESPACES.matcher(text);
    return DigestUtils.md5Hex(matchAllWhitespaces.replaceAll(""));
  }
}
