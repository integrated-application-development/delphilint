/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import org.sonarsource.sonarlint.core.analysis.api.ClientInputFile;

public class DelphiLintInputFile implements ClientInputFile {
  private final Path baseDir;
  private final Path relativePath;

  public DelphiLintInputFile(Path baseDir, Path relativePath) {
    this.baseDir = baseDir;
    this.relativePath = relativePath;
  }

  /**
   * @deprecated
   */
  @Override
  @Deprecated(since = "?")
  public String getPath() {
    return baseDir
        .resolve(relativePath)
        .toString()
        .replace(FileSystems.getDefault().getSeparator(), "/");
  }

  @Override
  public boolean isTest() {
    return false;
  }

  @Override
  public Charset getCharset() {
    return Charset.defaultCharset();
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
    return relativePath.toString().replace(FileSystems.getDefault().getSeparator(), "/");
  }

  @Override
  public URI uri() {
    return baseDir.resolve(relativePath).toUri();
  }
}
