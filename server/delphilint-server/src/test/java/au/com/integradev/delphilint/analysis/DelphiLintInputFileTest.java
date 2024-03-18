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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;

class DelphiLintInputFileTest {
  @Test
  @Deprecated
  void testGetPath() {
    var file = new DelphiLintInputFile(Path.of("/a/b/c"), Path.of("d/e/f"), StandardCharsets.UTF_8);
    assertEquals("/a/b/c/d/e/f", file.getPath());
  }

  @Test
  void testGetRelativePath() {
    var file = new DelphiLintInputFile(Path.of("/a/b/c"), Path.of("d/e/f"), StandardCharsets.UTF_8);
    assertEquals("d/e/f", file.relativePath());
  }

  @Test
  void testGetUri() {
    var file = new DelphiLintInputFile(Path.of("/a/b/c"), Path.of("d/e/f"), StandardCharsets.UTF_8);
    assertEquals(Path.of("/a/b/c/d/e/f").toUri(), file.uri());
  }

  @Test
  void testNoTestCode() {
    var file = new DelphiLintInputFile(Path.of("/a/b/c"), Path.of("d/e/f"), StandardCharsets.UTF_8);
    assertFalse(file.isTest());
  }

  @Test
  void testGetCharset() {
    var file =
        new DelphiLintInputFile(Path.of("/a/b/c"), Path.of("d/e/f"), StandardCharsets.UTF_16BE);
    assertEquals(StandardCharsets.UTF_16BE, file.getCharset());
  }

  @Test
  void testGetContentsUtf16() throws IOException {
    var file =
        new DelphiLintInputFile(
            Path.of("src/test/resources/au/com/integradev/delphilint/analysis/charsetDetection"),
            Path.of("Utf16File.pas"),
            StandardCharsets.UTF_16LE);
    assertEquals(
        "unit Utf16File;\r\n"
            + "\r\n"
            + "interface\r\n"
            + "\r\n"
            + "type\r\n"
            + "  TMy\uD83C\uDF55Pizza = class\r\n"
            + "  end;\r\n"
            + "\r\n"
            + "implementation\r\n"
            + "\r\n"
            + "end.",
        file.contents());
  }

  @Test
  void testGetContentsUtf8() throws IOException {
    var file =
        new DelphiLintInputFile(
            Path.of("src/test/resources/au/com/integradev/delphilint/analysis/charsetDetection"),
            Path.of("Utf8File.pas"),
            StandardCharsets.UTF_8);
    assertEquals(
        "unit Utf8File;\r\n"
            + "\r\n"
            + "interface\r\n"
            + "\r\n"
            + "type\r\n"
            + "  TMy\uD83C\uDF55Pizza = class\r\n"
            + "  end;\r\n"
            + "\r\n"
            + "implementation\r\n"
            + "\r\n"
            + "end.",
        file.contents());
  }

  @Test
  void testNullClientObject() {
    var file = new DelphiLintInputFile(Path.of("/a/b/c"), Path.of("d/e/f"), StandardCharsets.UTF_8);
    assertNull(file.<Integer>getClientObject());
  }
}
