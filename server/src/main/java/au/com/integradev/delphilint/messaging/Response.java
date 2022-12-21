package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Response {
  @JsonProperty private ResponseCategory category;
  @JsonProperty private Object data;

  private Response(ResponseCategory category, Object data) {
    this.category = category;
    this.data = data;
  }

  public static Response uninitialized() {
    return new Response(ResponseCategory.UNINITIALIZED, null);
  }

  public static Response initialized() {
    return new Response(ResponseCategory.INITIALIZED, null);
  }

  public static Response unexpectedError(String message) {
    return new Response(ResponseCategory.UNEXPECTED_ERROR, message);
  }

  public static Response analyzeResult(ResponseAnalyzeResult result) {
    return new Response(ResponseCategory.ANALYZE_RESULT, result);
  }

  public static Response analyzeError(String message) {
    return new Response(ResponseCategory.ANALYZE_ERROR, message);
  }

  public static Response pong(String data) {
    return new Response(ResponseCategory.PONG, data);
  }

  public static Response invalidRequest(String message) {
    return new Response(ResponseCategory.INVALID_REQUEST, message);
  }
}
