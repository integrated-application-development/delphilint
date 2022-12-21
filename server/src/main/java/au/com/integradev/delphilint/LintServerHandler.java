package au.com.integradev.delphilint;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import au.com.integradev.delphilint.messaging.RequestCategory;
import au.com.integradev.delphilint.messaging.Response;
import java.io.IOException;
import java.util.function.BiFunction;

public class LintServerHandler implements HttpHandler {
  private final BiFunction<RequestCategory, Object, Response> onMessage;
  private final ObjectMapper mapper;

  public LintServerHandler(BiFunction<RequestCategory, Object, Response> onMessage) {
    this.onMessage = onMessage;
    mapper = new ObjectMapper();
  }

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    var requestBody = mapper.readTree(exchange.getRequestBody());

    var categoryNode = requestBody.get("category");
    if (categoryNode != null) {
      var category = mapper.treeToValue(categoryNode, RequestCategory.class);

      var dataNode = requestBody.get("data");
      Object data = null;
      if (dataNode != null && category.getDataClass() != null) {
        data = mapper.treeToValue(dataNode, category.getDataClass());
      }

      Response responseMessage = onMessage.apply(category, data);

      if (responseMessage != null) {
        exchange.sendResponseHeaders(200, 0);
        mapper.writeValue(exchange.getResponseBody(), responseMessage);
      }
    }
  }
}
