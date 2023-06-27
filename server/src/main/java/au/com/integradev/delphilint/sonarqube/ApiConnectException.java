package au.com.integradev.delphilint.sonarqube;

public class ApiConnectException extends ApiException {
  public ApiConnectException() {
    super("Could not establish a connection");
  }
}
