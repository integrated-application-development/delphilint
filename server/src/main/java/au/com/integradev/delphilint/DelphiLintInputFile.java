package au.com.integradev.delphilint;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.Path;
import org.sonarsource.sonarlint.core.analysis.api.ClientInputFile;

public class DelphiLintInputFile implements ClientInputFile {
  private Path path;
  private Charset charset;

  public DelphiLintInputFile(Path path, Charset charset) {
    this.path = path;
    this.charset = charset;
  }

  @Override
  public String getPath() {
    return path.toString();
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
    return new FileInputStream(path.toString());
  }

  @Override
  public String contents() throws IOException {
    var inputStream = inputStream();
    return inputStream.toString();
  }

  @Override
  public String relativePath() {
    return path.toString();
  }

  @Override
  public URI uri() {
    return path.toUri();
  }
}
