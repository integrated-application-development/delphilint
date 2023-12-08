package au.com.integradev.delphilint.remote;

public class SonarHostBadRequestException extends SonarHostStatusCodeException {
  public SonarHostBadRequestException() {
    super(400);
  }
}
