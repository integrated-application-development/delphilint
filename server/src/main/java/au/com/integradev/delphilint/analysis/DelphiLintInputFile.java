package au.com.integradev.delphilint.analysis;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.Path;
import org.sonarsource.sonarlint.core.analysis.api.ClientInputFile;

public class DelphiLintInputFile implements ClientInputFile {
  private final Path baseDir;
  private final Path relativePath;
  private final Charset charset;

  public DelphiLintInputFile(Path baseDir, Path relativePath) {
    this.baseDir = baseDir;
    this.relativePath = relativePath;
    // TODO: pass in charset
    this.charset = Charset.defaultCharset();
  }

  @Override
  public String getPath() {
    return baseDir.resolve(relativePath).toString();
  }

  @Override
  public boolean isTest() {
    return false;
  }

  @Override
  public Charset getCharset() {
    return charset;
  }

  @Override
  public <G> G getClientObject() {
    return null;
  }

  @Override
  public InputStream inputStream() throws IOException {
    return new FileInputStream(baseDir.resolve(relativePath).toString());
  }

  @Override
  public String contents() throws IOException {
    try (var inputStream = inputStream()) {
      return inputStream.toString();
    }
  }

  @Override
  public String relativePath() {
    return relativePath.toString();
  }

  @Override
  public URI uri() {
    return baseDir.resolve(relativePath).toUri();
  }
}
