package au.com.integradev.delphilint.messaging;

public class RequestInitialize {
  private String bdsPath;
  private String compilerVersion;
  private String sonarHostUrl;
  private String projectKey;
  private String languageKey;

  public String getBdsPath() {
    return bdsPath;
  }

  public String getCompilerVersion() {
    return compilerVersion;
  }

  public String getSonarHostUrl() {
    return sonarHostUrl;
  }

  public String getProjectKey() {
    return projectKey;
  }

  public String getLanguageKey() {
    return languageKey;
  }
}
