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
import au.com.integradev.delphilint.server.plugin.DownloadedPlugin;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class FallbackPluginProvider {
  private static final Logger LOG = LogManager.getLogger(FallbackPluginProvider.class);
  private final Path jarsDir;
  private final SonarDelphiDownloader downloader;

  public FallbackPluginProvider(Path jarsDir, SonarDelphiDownloader downloader) {
    this.jarsDir = jarsDir;
    this.downloader = downloader;
  }

  public DownloadedPlugin getPlugin(Version version) {
    FallbackPlugin plugin = acquirePlugin(version);
    return new DownloadedPlugin(plugin.getPath(), plugin.getPath().getFileName().toString());
  }

  private FallbackPlugin acquirePlugin(Version targetVersion) {
    try {
      LOG.info("Resolving SonarDelphi fallback plugin using cache dir {}", jarsDir);
      Files.createDirectories(jarsDir);

      Path jarPath = getJarPath(targetVersion);

      if (Files.exists(jarPath)) {
        LOG.info("Using local cache of SonarDelphi {}", targetVersion);
        return FallbackPlugin.fromJar(jarPath);
      } else {
        LOG.info("Downloading SonarDelphi {}", targetVersion);
        return FallbackPlugin.fromDownloader(downloader, jarPath, targetVersion);
      }
    } catch (IOException | UncheckedIOException e) {
      throw new FallbackPluginProviderException("Fallback plugin provider failed", e);
    }
  }

  private Path getJarPath(Version version) {
    return this.jarsDir.resolve(String.format("DEFAULT-sonar-delphi-%s.jar", version));
  }
}
