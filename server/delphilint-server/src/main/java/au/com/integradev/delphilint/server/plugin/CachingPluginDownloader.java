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
package au.com.integradev.delphilint.server.plugin;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

import au.com.integradev.delphilint.remote.RemotePlugin;
import au.com.integradev.delphilint.remote.SonarHost;
import au.com.integradev.delphilint.remote.SonarHostException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CachingPluginDownloader {
  private static final Logger LOG = LogManager.getLogger(CachingPluginDownloader.class);
  private final Path pluginsPath;

  public CachingPluginDownloader(Path pluginsPath) {
    this.pluginsPath = pluginsPath;
  }

  public Optional<Set<DownloadedPlugin>> getRemotePluginJars(SonarHost host)
      throws SonarHostException {
    LOG.info("Getting Delphi plugins on Sonar host {}", host.getName());

    Set<RemotePlugin> remotePlugins = host.getDelphiPlugins();
    Set<DownloadedPlugin> secondaryPlugins = new HashSet<>();
    DownloadedPlugin corePlugin = null;

    for (RemotePlugin remotePlugin : remotePlugins) {
      Optional<DownloadedPlugin> downloadedPlugin = downloadPlugin(host, remotePlugin);

      if (downloadedPlugin.isPresent()) {
        if (remotePlugin.isCorePlugin()) {
          corePlugin = downloadedPlugin.get();
        } else {
          secondaryPlugins.add(downloadedPlugin.get());
        }
      }
    }

    if (corePlugin == null) {
      LOG.info("Core plugin could not be downloaded from host, using embedded core plugin only");
      return Optional.empty();
    } else {
      LOG.info(
          "Retrieved plugins from host: {}",
          () ->
              secondaryPlugins.stream()
                  .map(DownloadedPlugin::getFilename)
                  .collect(Collectors.joining(",")));

      secondaryPlugins.add(corePlugin);
      return Optional.of(secondaryPlugins);
    }
  }

  private Optional<DownloadedPlugin> downloadPlugin(SonarHost host, RemotePlugin plugin)
      throws SonarHostException {
    Path cachedPluginPath = pluginsPath.resolve(plugin.getFileName());

    if (!Files.exists(cachedPluginPath)) {
      LOG.info(
          "{} ({}) does not exist in local cache, downloading to {}",
          plugin.getFileName(),
          plugin.getPluginKey(),
          pluginsPath);

      if (!downloadJar(host, plugin.getPluginKey(), cachedPluginPath)) {
        LOG.warn("{} could not be downloaded", plugin.getFileName());
        return Optional.empty();
      }
    } else {
      LOG.info("{} found in local cache", plugin.getFileName());
    }

    return Optional.of(new DownloadedPlugin(cachedPluginPath, plugin.getFileName()));
  }

  private static boolean downloadJar(SonarHost host, String pluginKey, Path destination)
      throws SonarHostException {
    Optional<Path> tempPath = host.getPluginJar(pluginKey);

    if (tempPath.isEmpty()) {
      LOG.warn("Plugin jar download not successful");
      return false;
    }

    try {
      Files.copy(tempPath.get(), destination, REPLACE_EXISTING);
      LOG.info("Plugin jar saved to destination");
      Files.delete(tempPath.get());
      return true;
    } catch (IOException e) {
      LOG.error(e);
      return false;
    }
  }
}
