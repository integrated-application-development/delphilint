package au.com.integradev.delphilint.messaging;

import java.nio.file.Path;
import java.util.Set;

public class RequestAnalyze {
  private Path baseDir;
  private Set<Path> inputFiles;
  private String sonarHostUrl;
  private String projectKey;

  public Path getBaseDir() {
    return baseDir;
  }

  public Set<Path> getInputFiles() {
    return inputFiles;
  }

  public String getSonarHostUrl() {
    return sonarHostUrl;
  }

  public String getProjectKey() {
    return projectKey;
  }
}
