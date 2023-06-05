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
