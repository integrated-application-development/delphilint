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
package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.databind.JsonNode;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse.BodyHandler;
import java.util.function.Supplier;

public class ApiConnection {
  private final HttpClient http;
  private final String hostUrl;

  public ApiConnection(String hostUrl) {
    http = HttpClient.newHttpClient();
    this.hostUrl = hostUrl;
  }

  private <T> T getResponse(String url, BodyHandler<Supplier<T>> handler) {
    var request = HttpRequest.newBuilder(URI.create(url)).build();

    try {
      var response = http.send(request, handler);
      return response.body().get();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
    return null;
  }

  public JsonNode getJson(String url) {
    return getResponse(hostUrl + url, new JsonBodyHandler());
  }

  public <T> T getXml(String url, Class<T> clazz) {
    return getResponse(hostUrl + url, new XmlBodyHandler<>(clazz));
  }
}
