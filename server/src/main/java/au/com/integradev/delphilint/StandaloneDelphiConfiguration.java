package au.com.integradev.delphilint;

public class StandaloneDelphiConfiguration implements DelphiConfiguration {
  private final String bdsPath;
  private final String compilerVersion;

  public StandaloneDelphiConfiguration(String bdsPath, String compilerVersion) {
    this.bdsPath = bdsPath;
    this.compilerVersion = compilerVersion;
  }

  public String getBdsPath() {
    return bdsPath;
  }

  public String getCompilerVersion() {
    return compilerVersion;
  }
}
