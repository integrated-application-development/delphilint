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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import org.apache.commons.io.ByteOrderMark;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.input.BOMInputStream;
import org.sonarsource.sonarlint.core.analysis.api.ClientInputFile;

public class DelphiLintInputFile implements ClientInputFile {
  private final Path baseDir;
  private final Path relativePath;
  private final Charset charset;

  public DelphiLintInputFile(Path baseDir, Path relativePath, Charset charset) {
    this.baseDir = baseDir;
    this.relativePath = relativePath;
    this.charset = charset;
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
    return charset;
  }

  @Override
  public <G> G getClientObject() {
    return null;
  }

  @Override
  public InputStream inputStream() throws IOException {
    return new BOMInputStream(
        new FileInputStream(baseDir.resolve(relativePath).toString()),
        ByteOrderMark.UTF_8,
        ByteOrderMark.UTF_16BE,
        ByteOrderMark.UTF_16LE,
        ByteOrderMark.UTF_32BE,
        ByteOrderMark.UTF_32LE);
  }

  @Override
  public String contents() throws IOException {
    try (var inputStream = inputStream()) {
      return IOUtils.toString(inputStream, charset);
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
