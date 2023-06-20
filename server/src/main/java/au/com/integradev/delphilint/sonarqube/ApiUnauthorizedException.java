package au.com.integradev.delphilint.sonarqube;

public class ApiUnauthorizedException extends ApiStatusCodeException {
  public ApiUnauthorizedException() {
    super(401);
  }
}
