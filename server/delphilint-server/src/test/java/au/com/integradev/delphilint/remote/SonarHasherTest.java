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
  void differentCaseProducesDifferentHash() {
    assertNotEquals(SonarHasher.hash("foobarbaz"), SonarHasher.hash("FooBarBaz"));
  }

  @Test
  void differentTextProducesDifferentHash() {
    assertNotEquals(
        SonarHasher.hash("The quick brown fox"), SonarHasher.hash("jumps over the lazy dog"));
  }

  @Test
  void utf8Hash() {
    String expectedHash = SonarHasher.hashFileLine(FILE_RESOURCE_UTF8, 6, StandardCharsets.UTF_8);
    assertEquals(expectedHash, SonarHasher.hash("TMy\uD83C\uDF55Pizza = class"));
  }

  @Test
  void ansiHash() {
    String expectedHash =
        SonarHasher.hashFileLine(FILE_RESOURCE_ANSI, 6, StandardCharsets.ISO_8859_1);
    assertEquals(expectedHash, SonarHasher.hash("TMy£PoundSterling = class"));
  }

  @Test
  void fileLineHashProducesLineHashWithoutWhitespace() {
    assertEquals(SonarHasher.hash("MyType=class"), SonarHasher.hashFileLine(FILE_RESOURCE, 6));
  }

  @Test
  void singleLineFileRangeProducesFirstLineHash() {
    assertEquals(
        SonarHasher.hash("MyType=class"),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(6, 3, 6, 9)));
  }

  @Test
  void multiLineFileRangeProducesFirstLineHash() {
    assertEquals(
        SonarHasher.hash("MyType=class"),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(6, 5, 9, 2)));
  }

  @Test
  void hashInvalidRangeProducesFirstLineHash() {
    assertEquals(
        SonarHasher.hash("MyType=class"),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(6, 100, 900, 1)));
  }

  @Test
  void hashNonexistentLineProducesEmptyStringHash() {
    assertEquals(
        SonarHasher.hash(""),
        SonarHasher.hashFileRange(FILE_RESOURCE, new TextRange(900, 0, 900, 1)));
  }
}
