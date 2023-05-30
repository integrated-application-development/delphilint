package au.com.integradev.delphilint.messaging;

public class RequestInitialize {
  private String bdsPath;
  private String compilerVersion;
  private String sonarDelphiJarPath;

  public String getBdsPath() {
    return bdsPath;
  }

  public String getCompilerVersion() {
    return compilerVersion;
  }

  public String getSonarDelphiJarPath() {
    return sonarDelphiJarPath;
  }
}
