package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Response {
  @JsonProperty private Category category;
  @JsonProperty private Object data;

  public Response(Category category, Object data) {
    this.category = category;
    this.data = data;
  }

  public static Response uninitialized() {
    return new Response(Category.UNINITIALIZED, null);
  }

  public static Response initialized() {
    return new Response(Category.INITIALIZED, null);
  }

  public static Response unexpectedError(String message) {
    return new Response(Category.UNEXPECTED_ERROR, message);
  }

  public static Response analyzeResult(ResponseAnalyzeResult result) {
    return new Response(Category.ANALYZE_RESULT, result);
  }

  public static Response analyzeError(String message) {
    return new Response(Category.ANALYZE_ERROR, message);
  }

  public static Response pong(String data) {
    return new Response(Category.PONG, data);
  }

  public static Response invalidRequest(String message) {
    return new Response(Category.INVALID_REQUEST, message);
  }

  public Category getCategory() {
    return category;
  }

  public Object getData() {
    return data;
  }
}
