package au.com.integradev.delphilint.analysis;

import java.nio.file.Path;

public class StandaloneDelphiConfiguration implements DelphiConfiguration {
  private final String bdsPath;
  private final String compilerVersion;
  private final Path sonarDelphiJarPath;

  public StandaloneDelphiConfiguration(
      String bdsPath, String compilerVersion, Path sonarDelphiJarPath) {
    this.bdsPath = bdsPath;
    this.compilerVersion = compilerVersion;
    this.sonarDelphiJarPath = sonarDelphiJarPath;
  }

  public String getBdsPath() {
    return bdsPath;
  }

  public String getCompilerVersion() {
    return compilerVersion;
  }

  public Path getSonarDelphiJarPath() {
    return sonarDelphiJarPath;
  }
}
