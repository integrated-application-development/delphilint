package au.com.integradev.delphilint.messaging;

public enum ResponseCategory {
  UNEXPECTED_ERROR(String.class),
  ANALYZE_RESULT(ResponseAnalyzeResult.class),
  ANALYZE_ERROR(String.class),
  UNINITIALIZED,
  INITIALIZED,
  PONG(String.class);

  private final Class<?> dataClass;

  ResponseCategory() {
    this.dataClass = null;
  }

  ResponseCategory(Class<?> clazz) {
    this.dataClass = clazz;
  }

  public Class<?> getDataClass() {
    return dataClass;
  }
}
