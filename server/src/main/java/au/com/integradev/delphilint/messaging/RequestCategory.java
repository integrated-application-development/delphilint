package au.com.integradev.delphilint.messaging;

public enum RequestCategory {
  INITIALIZE(RequestInitialize.class),
  ANALYZE(RequestAnalyze.class),
  PING(String.class);

  private final Class<?> dataClass;

  RequestCategory(Class<?> clazz) {
    this.dataClass = clazz;
  }

  public Class<?> getDataClass() {
    return dataClass;
  }
}
