package au.com.integradev.delphilint.server.plugin;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

import au.com.integradev.delphilint.remote.SonarHost;
import au.com.integradev.delphilint.remote.SonarHostException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CachingPluginDownloader {
  private static final Logger LOG = LogManager.getLogger(CachingPluginDownloader.class);
  private final Path pluginsPath;

  public CachingPluginDownloader(Path pluginsPath) {
    this.pluginsPath = pluginsPath;
  }

  public Optional<DownloadedPlugin> getRemotePluginJar(SonarHost host) throws SonarHostException {
    LOG.info("Getting plugin jar for Sonar host {}", host.getName());

    Optional<String> optPluginJarName = host.getPluginJarName();

    if (optPluginJarName.isEmpty()) {
      LOG.info("No plugin jar on host, using embedded");
      return Optional.empty();
    } else {
      LOG.info("Plugin jar found on host: {}", optPluginJarName.get());
    }

    Path cachedJarFile = pluginsPath.resolve(optPluginJarName.get());

    if (!Files.exists(cachedJarFile)) {
      LOG.info(
          "{} does not exist in local cache, downloading to {}",
          optPluginJarName.get(),
          pluginsPath);
      if (!downloadJar(host, cachedJarFile)) {
        return Optional.empty();
      }
    } else {
      LOG.info("{} found in local cache", optPluginJarName.get());
    }

    return Optional.of(new DownloadedPlugin(cachedJarFile, optPluginJarName.get()));
  }

  private boolean downloadJar(SonarHost host, Path destination) throws SonarHostException {
    Optional<Path> tempPath = host.getPluginJar();

    if (tempPath.isEmpty()) {
      LOG.warn("Plugin jar download not successful");
      return false;
    }

    try {
      Files.copy(tempPath.get(), destination, REPLACE_EXISTING);
      LOG.info("Plugin jar saved to destination");
      return true;
    } catch (IOException e) {
      LOG.error(e);
      return false;
    }
  }
}
