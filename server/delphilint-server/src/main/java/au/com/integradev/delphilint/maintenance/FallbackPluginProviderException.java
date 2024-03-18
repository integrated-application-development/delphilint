package au.com.integradev.delphilint.maintenance;

public class FallbackPluginProviderException extends RuntimeException {
  public FallbackPluginProviderException(String message) {
    super(message);
  }

  public FallbackPluginProviderException(String message, Throwable cause) {
    super(message, cause);
  }
}
