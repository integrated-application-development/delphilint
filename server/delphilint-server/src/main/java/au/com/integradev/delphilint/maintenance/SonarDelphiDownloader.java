package au.com.integradev.delphilint.maintenance;

import au.com.integradev.delphilint.remote.sonarqube.Version;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Optional;
import java.util.stream.StreamSupport;
import javax.net.ssl.HttpsURLConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SonarDelphiDownloader {
  private static final Logger LOG = LogManager.getLogger(SonarDelphiDownloader.class);
  private static final String API_URL = "https://api.github.com";
  private static final String REPO_NAME = "integrated-application-development/sonar-delphi";
  private final ObjectMapper jsonMapper;

  public SonarDelphiDownloader() {
    jsonMapper = new ObjectMapper();
  }

  public Path downloadJar(Path path, ReleaseInfo latestRelease) throws IOException {
    var url =
        new URL(
            String.format(
                "%s/repos/%s/releases/assets/%d",
                API_URL, REPO_NAME, latestRelease.getJarAssetId()));
    var connection = (HttpsURLConnection) url.openConnection();
    connection.setRequestProperty("Accept", "application/octet-stream");
    connection.connect();

    try (var stream = connection.getInputStream()) {
      Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING);
    }

    return path;
  }

  public Optional<ReleaseInfo> getLatestRelease() {
    try {
      var url = new URL(API_URL + "/repos/" + REPO_NAME + "/releases/latest");
      JsonNode root = jsonMapper.readTree(url);

      Optional<Integer> assetId =
          StreamSupport.stream(root.get("assets").spliterator(), false)
              .filter(asset -> asset.get("name").asText().startsWith("sonar-delphi-plugin-"))
              .map(asset -> asset.get("id").asInt())
              .findFirst();

      if (assetId.isEmpty()) {
        throw new FallbackPluginProviderException(
            "No sonar-delphi-plugin asset found on latest release");
      }

      var versionTag = root.get("tag_name").asText().substring(1);
      return Optional.of(new ReleaseInfo(new Version(versionTag), assetId.get()));
    } catch (Exception e) {
      LOG.error("Could not get latest release of SonarDelphi", e);
      return Optional.empty();
    }
  }

  public static class ReleaseInfo {
    private final Version version;
    private final int jarAssetId;

    public ReleaseInfo(Version version, int jarAssetId) {
      this.version = version;
      this.jarAssetId = jarAssetId;
    }

    public Version getVersion() {
      return version;
    }

    public int getJarAssetId() {
      return jarAssetId;
    }
  }
}
