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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import au.com.integradev.delphilint.analysis.TextRange;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;

class SonarHasherTest {
  private static final Path FILE_RESOURCE =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote/AsciiFile.pas");
  private static final Path FILE_RESOURCE_UTF8 =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote/Utf8File.pas");
  private static final Path FILE_RESOURCE_ANSI =
      Path.of("src/test/resources/au/com/integradev/delphilint/remote/AnsiFile.pas");

  @Test
  void textWithNoWhitespaceProducesCorrectMd5Hash() {
    assertEquals("d2a2b4a81c19e8df29afb2e2fe04245a", SonarHasher.hash("FooBarBaz...!"));
  }

  @Test
  void textWithWhitespaceStripsAndProducesCorrectMd5Hash() {
    assertEquals("d2a2b4a81c19e8df29afb2e2fe04245a", SonarHasher.hash("Foo Bar Baz...!"));
  }

  @Test
  void textWithWhitespaceProducesSameHashAsTextWithoutWhitespace() {
    String expectedHash = SonarHasher.hash("FooBarBaz...!");
    assertEquals(expectedHash, SonarHasher.hash("Foo Bar Baz...!"));
    assertEquals(expectedHash, SonarHasher.hash("\tFoo Bar\nBaz..\n.!"));
  }

  @Test
  void testDifferentCaseProducesDifferentHash() {
    assertNotEquals(SonarHasher.hash("foobarbaz"), SonarHasher.hash("FooBarBaz"));
  }

  @Test
  void testDifferentTextProducesDifferentHash() {
    assertNotEquals(
        SonarHasher.hash("The quick brown fox"), SonarHasher.hash("jumps over the lazy dog"));
  }

  @Test
  void testUtf8Hash() {
    String expectedHash = SonarHasher.hashFileLine(FILE_RESOURCE_UTF8, 6, StandardCharsets.UTF_8);
    assertEquals(expectedHash, SonarHasher.hash("TMy\uD83C\uDF55Pizza = class"));
  }

  @Test
  void testAnsiHash() {
    String expectedHash =
        SonarHasher.hashFileLine(FILE_RESOURCE_ANSI, 6, StandardCharsets.ISO_8859_1);
    assertEquals(expectedHash, SonarHasher.hash("TMy£PoundSterling = class"));
  }

  @Test
  void testFileLineHashProducesLineHashWithoutWhitespace() {
    assertEquals(SonarHasher.hash("MyType=class"), SonarHasher.hashFileLine(FILE_RESOURCE, 6));
  }

  @Test
  void testSingleLineFileRangeProducesFirstLineHash() {
    assertEquals(
        SonarHasher.hash("MyType=class"),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(6, 3, 6, 9)));
  }

  @Test
  void testMultiLineFileRangeProducesFirstLineHash() {
    assertEquals(
        SonarHasher.hash("MyType=class"),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(6, 5, 9, 2)));
  }

  @Test
  void testHashInvalidRangeProducesFirstLineHash() {
    assertEquals(
        SonarHasher.hash("MyType=class"),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(6, 100, 900, 1)));
  }

  @Test
  void testHashNonexistentLineProducesEmptyStringHash() {
    assertEquals(
        SonarHasher.hash(""),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(900, 0, 900, 1)));
  }
}
