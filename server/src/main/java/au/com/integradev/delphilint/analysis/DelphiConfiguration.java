package au.com.integradev.delphilint.analysis;

import java.nio.file.Path;

public interface DelphiConfiguration {
  public String getBdsPath();

  public String getCompilerVersion();
  public Path getSonarDelphiJarPath();
}
