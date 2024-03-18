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
package au.com.integradev.delphilint.maintenance;

import au.com.integradev.delphilint.remote.sonarqube.Version;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.jar.JarFile;

public final class FallbackPlugin {
  private final Path path;
  private final Version version;

  private FallbackPlugin(Path path, Version version) {
    this.path = path;
    this.version = version;
  }

  public Path getPath() {
    return path;
  }

  public Version getVersion() {
    return version;
  }

  public static FallbackPlugin fromJar(Path jarPath) {
    return new FallbackPlugin(jarPath, getJarVersion(jarPath));
  }

  public static FallbackPlugin fromDownloader(
      SonarDelphiDownloader downloader, Path path, Version version) throws IOException {
    return new FallbackPlugin(downloader.downloadJar(path, version), version);
  }

  private static Version getJarVersion(Path jarPath) {
    try (var jar = new JarFile(jarPath.toFile())) {
      String versionValue = jar.getManifest().getMainAttributes().getValue("Plugin-Version");
      return new Version(versionValue);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
