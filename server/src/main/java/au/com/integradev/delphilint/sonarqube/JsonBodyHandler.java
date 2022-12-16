package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandler;
import java.net.http.HttpResponse.BodySubscriber;
import java.net.http.HttpResponse.ResponseInfo;
import java.nio.charset.StandardCharsets;
import java.util.function.Supplier;

public class JsonBodyHandler implements BodyHandler<Supplier<JsonNode>> {
  private static final ObjectMapper mapper = new ObjectMapper();

  @Override
  public BodySubscriber<Supplier<JsonNode>> apply(ResponseInfo responseInfo) {
    return asJson();
  }

  private static BodySubscriber<Supplier<JsonNode>> asJson() {
    var upstream = HttpResponse.BodySubscribers.ofString(StandardCharsets.UTF_8);

    return HttpResponse.BodySubscribers.mapping(
        upstream,
        (String body) ->
            (Supplier<JsonNode>)
                () -> {
                  try {
                    return mapper.readTree(body);
                  } catch (JsonProcessingException e) {
                    return null;
                  }
                });
  }
}
