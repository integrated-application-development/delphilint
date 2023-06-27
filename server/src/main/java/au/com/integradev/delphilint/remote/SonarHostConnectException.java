package au.com.integradev.delphilint.remote;

public class SonarHostConnectException extends SonarHostException {
  public SonarHostConnectException() {
    super("Could not establish a connection");
  }
}
