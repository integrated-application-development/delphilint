package au.com.integradev.delphilint.sonarqube;

public class UncheckedApiException extends RuntimeException {
  public UncheckedApiException(Throwable err) {
    super(err);
  }
}
