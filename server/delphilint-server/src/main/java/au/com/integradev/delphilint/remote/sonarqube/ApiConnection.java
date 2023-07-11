/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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
package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.JsonHttpHandler;
import au.com.integradev.delphilint.remote.SonarHostConnectException;
import au.com.integradev.delphilint.remote.SonarHostException;
import au.com.integradev.delphilint.remote.SonarHostStatusCodeException;
import au.com.integradev.delphilint.remote.SonarHostUnauthorizedException;
import com.fasterxml.jackson.databind.JsonNode;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse.BodyHandler;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Supplier;

public class ApiConnection {
  private final HttpClient http;
  private final String hostUrl;
  private final String token;

  public ApiConnection(String hostUrl, String token) {
    http = HttpClient.newHttpClient();
    this.hostUrl = hostUrl;
    this.token = token;
  }

  public String getHostUrl() {
    return hostUrl;
  }

  private <T> T getResponse(String url, BodyHandler<T> handler) throws SonarHostException {
    var reqBuilder = HttpRequest.newBuilder(URI.create(url));

    if (!this.token.isEmpty()) {
      reqBuilder.header("Authorization", "Bearer " + token);
    }

    HttpRequest request = reqBuilder.build();

    try {
      var response = http.send(request, handler);

      if (response.statusCode() == 401) {
        throw new SonarHostUnauthorizedException();
      } else if (response.statusCode() == 200) {
        return response.body();
      } else {
        throw new SonarHostStatusCodeException(response.statusCode());
      }
    } catch (IOException e) {
      throw new SonarHostConnectException();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
    return null;
  }

  public JsonNode getJson(String url) throws SonarHostException {
    Supplier<JsonNode> response = getResponse(hostUrl + url, new JsonHttpHandler());

    if (response != null) {
      return response.get();
    }
    return null;
  }

  public Path getFile(String url) throws SonarHostException {
    try {
      Path temp = Files.createTempFile("delphilint-server", ".tmp");
      return getResponse(hostUrl + url, BodyHandlers.ofFile(temp));
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
