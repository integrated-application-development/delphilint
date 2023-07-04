package au.com.integradev.delphilint.remote;

public class SonarHostStatusCodeException extends SonarHostException {
  public SonarHostStatusCodeException(int statusCode) {
    super("API call returned unexpected status code " + statusCode);
  }
}
