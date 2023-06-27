package au.com.integradev.delphilint.remote;

public class SonarHostUnauthorizedException extends SonarHostStatusCodeException {
  public SonarHostUnauthorizedException() {
    super(401);
  }
}
