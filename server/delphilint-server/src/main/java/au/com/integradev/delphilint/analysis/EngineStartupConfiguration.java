package au.com.integradev.delphilint.analysis;

import java.nio.file.Path;
import java.util.Map;

public class EngineStartupConfiguration {
  private final String bdsPath;
  private final String compilerVersion;
  private final Path sonarDelphiJarPath;

  public EngineStartupConfiguration(
      String bdsPath, String compilerVersion, Path sonarDelphiJarPath) {
    this.bdsPath = bdsPath;
    this.compilerVersion = compilerVersion;
    this.sonarDelphiJarPath = sonarDelphiJarPath;
  }

  public Path getSonarDelphiJarPath() {
    return sonarDelphiJarPath;
  }

  public Map<String, String> getBaseProperties() {
    return Map.of(
        "sonar.delphi.bds.path", bdsPath,
        "sonar.delphi.compiler.version", compilerVersion);
  }
}
