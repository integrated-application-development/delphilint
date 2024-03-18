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
