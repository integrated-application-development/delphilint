package au.com.integradev.delphilint.analysis;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.commons.io.ByteOrderMark;
import org.apache.commons.io.input.BOMInputStream;

public class DelphiCharsetDetector {
  private final Charset defaultCharset;

  public DelphiCharsetDetector(Charset defaultCharset) {
    this.defaultCharset = defaultCharset;
  }

  public Charset detectCharset(Path filePath) {
    try (var bomStream =
        new BOMInputStream(
            Files.newInputStream(filePath),
            ByteOrderMark.UTF_8,
            ByteOrderMark.UTF_16BE,
            ByteOrderMark.UTF_16LE,
            ByteOrderMark.UTF_32BE,
            ByteOrderMark.UTF_32LE)) {
      String charsetName = bomStream.getBOMCharsetName();
      return charsetName != null ? Charset.forName(charsetName) : defaultCharset;
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
