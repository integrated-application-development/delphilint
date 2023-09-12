package au.com.integradev.delphilint.remote;

public class SonarCharacteristics {
  private final boolean codeAttributes;
  private boolean supported;

  public SonarCharacteristics(boolean usesCodeAttributes) {
    this.codeAttributes = usesCodeAttributes;
    this.supported = true;
  }

  public static SonarCharacteristics unsupported() {
    var version = new SonarCharacteristics(false);
    version.supported = false;
    return version;
  }

  public boolean usesCodeAttributes() {
    return codeAttributes;
  }

  public boolean isSupported() {
    return supported;
  }
}
