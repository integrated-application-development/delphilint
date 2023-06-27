package au.com.integradev.delphilint.remote;

public class UncheckedSonarHostException extends RuntimeException {
  public UncheckedSonarHostException(Throwable err) {
    super(err);
  }
}
