package au.com.integradev.delphilint.maintenance;

import au.com.integradev.delphilint.maintenance.SonarDelphiDownloader.ReleaseInfo;
import au.com.integradev.delphilint.remote.sonarqube.Version;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Optional;
import java.util.jar.JarFile;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.util.StringUtils;

public class FallbackPluginProvider {
  private static final Logger LOG = LogManager.getLogger(FallbackPluginProvider.class);
  private final Path jarsDir;
  private final SonarDelphiDownloader downloader;

  public FallbackPluginProvider(Path jarsDir, SonarDelphiDownloader downloader) {
    this.jarsDir = jarsDir;
    this.downloader = downloader;
  }

  public Path getPlugin() throws FallbackPluginProviderException {
    try {
      LOG.info("Getting SonarDelphi fallback plugin using cache dir {}", jarsDir);
      Files.createDirectories(jarsDir);

      Optional<ReleaseInfo> latestRelease = downloader.getLatestRelease();
      Optional<Path> localJar = getLatestLocalJar();

      if (localJar.isPresent()) {
        var localVersion = getJarVersion(localJar.get());

        if (latestRelease.isEmpty()) {
          LOG.warn(
              "Could not retrieve latest SonarDelphi release, using local fallback {}",
              localVersion);
        } else if (latestRelease.get().getVersion().compareTo(localVersion) <= 0) {
          LOG.info(
              "Local SonarDelphi fallback is the latest version {} (latest remote release is {})",
              localVersion,
              latestRelease.get().getVersion());
          return localJar.get();
        } else {
          LOG.info("Local SonarDelphi fallback is outdated version {}", localVersion);
        }
      }

      if (latestRelease.isPresent()) {
        return downloadJar(latestRelease.get());
      } else {
        throw new FallbackPluginProviderException(
            "Could not retrieve latest SonarDelphi release from GitHub");
      }
    } catch (IOException | UncheckedIOException e) {
      throw new FallbackPluginProviderException("Fallback plugin provider failed", e);
    }
  }

  private Path downloadJar(ReleaseInfo release) throws IOException {
    Path path = downloader.downloadJar(getJarPath(release.getVersion()), release);
    assertJarVersion(path, release.getVersion());
    LOG.info("Downloaded SonarDelphi {} as fallback", release.getVersion());
    return path;
  }

  private Path getJarPath(Version version) {
    return this.jarsDir.resolve(String.format("DEFAULT-sonar-delphi-%s.jar", version));
  }

  private Optional<Path> getLatestLocalJar() throws IOException {
    if (!Files.exists(jarsDir)) {
      return Optional.empty();
    }

    try (var files = Files.list(jarsDir)) {
      return files
          .filter(
              path -> StringUtils.startsWithIgnoreCase(path.getFileName().toString(), "DEFAULT-"))
          .max(Comparator.comparing(FallbackPluginProvider::getJarVersion));
    }
  }

  private static Version getJarVersion(Path jarPath) {
    try (var jar = new JarFile(jarPath.toFile())) {
      String versionValue = jar.getManifest().getMainAttributes().getValue("Plugin-Version");
      return new Version(versionValue);
    } catch (IOException e) {
      LOG.error("Error getting jar version", e);
      throw new UncheckedIOException(e);
    }
  }

  private static void assertJarVersion(Path jarPath, Version expectedVersion) {
    var actualVersion = getJarVersion(jarPath);
    if (actualVersion.equals(expectedVersion)) {
      LOG.warn(
          "Jar downloaded from release {} is actually version {}", expectedVersion, actualVersion);
    }
  }
}
