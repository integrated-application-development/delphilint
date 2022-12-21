package au.com.integradev.delphilint;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
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

  private void sendJsonResponse(HttpExchange exchange, Response response) throws IOException {
    exchange.getResponseHeaders().set("Content-Type", "application/json");
    exchange.sendResponseHeaders(200, 0);
    mapper.writeValue(exchange.getResponseBody(), response);
  }

  private JsonNode getRequestBodyAsJsonNode(HttpExchange exchange) throws IllegalArgumentException {
    try {
      return mapper.readTree(exchange.getRequestBody());
    } catch (IOException e) {
      throw new IllegalArgumentException("Invalid JSON in request");
    }
  }

  private RequestCategory getRequestCategory(JsonNode requestBody) throws IllegalArgumentException {
    var categoryNode = requestBody.get("category");
    if (categoryNode == null) {
      throw new IllegalArgumentException("No category specified");
    }

    try {
      return mapper.treeToValue(categoryNode, RequestCategory.class);
    } catch (JsonProcessingException e) {
      throw new IllegalArgumentException("Specified category is unknown");
    }
  }

  private Object getRequestData(JsonNode requestBody, RequestCategory category)
      throws IllegalArgumentException {
    var dataNode = requestBody.get("data");

    if ((dataNode == null || dataNode.isNull()) && category.getDataClass() != null) {
      throw new IllegalArgumentException("Data in this category cannot be null");
    } else if (category.getDataClass() != null) {
      try {
        return mapper.treeToValue(dataNode, category.getDataClass());
      } catch (JsonProcessingException e) {
        throw new IllegalArgumentException("Data is in an incorrect format");
      }
    } else {
      return null;
    }
  }

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    JsonNode requestBody;
    RequestCategory category;
    Object data;
    try {
      requestBody = getRequestBodyAsJsonNode(exchange);
      category = getRequestCategory(requestBody);
      data = getRequestData(requestBody, category);
    } catch (IllegalArgumentException e) {
      sendJsonResponse(exchange, Response.invalidRequest(e.getMessage()));
      return;
    }

    Response responseMessage = onMessage.apply(category, data);

    if (responseMessage != null) {
      sendJsonResponse(exchange, responseMessage);
    }
  }
}
