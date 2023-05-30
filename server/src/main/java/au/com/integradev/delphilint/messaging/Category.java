package au.com.integradev.delphilint.messaging;

public enum Category {
  PING(1, String.class),
  PONG(5, String.class),
  QUIT(15),
  INITIALIZE(20, RequestInitialize.class),
  INITIALIZED(25),
  UNINITIALIZED(26),
  ANALYZE(30, RequestAnalyze.class),
  ANALYZE_RESULT(35, ResponseAnalyzeResult.class),
  ANALYZE_ERROR(36, String.class),
  INVALID_REQUEST(241, String.class),
  UNEXPECTED_ERROR(242, String.class);

  private int code;
  private final Class<?> dataClass;

  Category(int code) {
    this.code = code;
    this.dataClass = null;
  }

  Category(int code, Class<?> clazz) {
    this.code = code;
    this.dataClass = clazz;
  }

  public static Category fromCode(int code) {
    for (var category : values()) {
      if (category.getCode() == code) {
        return category;
      }
    }

    return null;
  }

  public int getCode() {
    return code;
  }

  public Class<?> getDataClass() {
    return dataClass;
  }
}
