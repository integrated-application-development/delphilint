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
