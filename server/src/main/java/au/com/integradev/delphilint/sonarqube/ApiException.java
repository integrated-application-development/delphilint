package au.com.integradev.delphilint.sonarqube;

public class ApiException extends Exception {
  public ApiException(String message) {
    super(message);
  }
}
