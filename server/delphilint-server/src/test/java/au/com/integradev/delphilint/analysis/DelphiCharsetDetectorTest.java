package au.com.integradev.delphilint.analysis;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;

class DelphiCharsetDetectorTest {
  private static final Path BASE_DIR =
      Path.of("src/test/resources/au/com/integradev/delphilint/analysis/charsetDetection");
  private static final Path UTF8_FILE = BASE_DIR.resolve("Utf8File.pas");
  private static final Path ANSI_FILE = BASE_DIR.resolve("AnsiFile.pas");
  private static final Path UTF16_FILE = BASE_DIR.resolve("Utf16File.pas");

  @Test
  void testDetectsUtf8() {
    var detector = new DelphiCharsetDetector(StandardCharsets.ISO_8859_1);
    assertEquals(StandardCharsets.UTF_8, detector.detectCharset(UTF8_FILE));
  }

  @Test
  void testDetectsUtf16() {
    var detector = new DelphiCharsetDetector(StandardCharsets.ISO_8859_1);
    assertEquals(StandardCharsets.UTF_16LE, detector.detectCharset(UTF16_FILE));
  }

  @Test
  void testFallsBackToProvidedAnsiEncoding() {
    var detector = new DelphiCharsetDetector(StandardCharsets.ISO_8859_1);
    assertEquals(StandardCharsets.ISO_8859_1, detector.detectCharset(ANSI_FILE));
  }

  @Test
  void testFallsBackToProvidedUtf8Encoding() {
    var detector = new DelphiCharsetDetector(StandardCharsets.UTF_8);
    assertEquals(StandardCharsets.UTF_8, detector.detectCharset(ANSI_FILE));
  }
}
