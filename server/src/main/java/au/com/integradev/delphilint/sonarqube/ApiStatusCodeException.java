package au.com.integradev.delphilint.sonarqube;

public class ApiStatusCodeException extends ApiException {
  public ApiStatusCodeException(int statusCode) {
    super("API call returned unexpected status code " + statusCode);
  }
}
