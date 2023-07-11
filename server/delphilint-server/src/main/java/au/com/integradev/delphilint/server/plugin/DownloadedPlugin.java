package au.com.integradev.delphilint.server.plugin;

import java.nio.file.Path;
import java.util.Objects;

public class DownloadedPlugin {
  private final String filename;
  private final Path path;

  public DownloadedPlugin(Path path, String filename) {
    this.filename = filename;
    this.path = path;
  }

  public String getFilename() {
    return filename;
  }

  public Path getPath() {
    return path;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DownloadedPlugin that = (DownloadedPlugin) o;
    return Objects.equals(path, that.path);
  }

  @Override
  public int hashCode() {
    return Objects.hash(path);
  }
}
