package au.com.integradev.delphilint.maintenance;

import au.com.integradev.delphilint.remote.sonarqube.Version;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import javax.net.ssl.HttpsURLConnection;

public class SonarDelphiDownloader {
  private static final String REPO_NAME = "integrated-application-development/sonar-delphi";

  public Path downloadJar(Path path, Version version) throws IOException {
    var url =
        new URL(
            String.format(
                "https://github.com/%s/releases/download/v%s/sonar-delphi-plugin-%s.jar",
                REPO_NAME, version, version));
    var connection = (HttpsURLConnection) url.openConnection();
    connection.setRequestProperty("Accept", "application/octet-stream");
    connection.connect();

    try (var stream = connection.getInputStream()) {
      Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING);
    }

    return path;
  }
}
