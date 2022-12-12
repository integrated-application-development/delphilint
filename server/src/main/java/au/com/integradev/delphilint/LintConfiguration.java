package au.com.integradev.delphilint;

import java.util.Optional;
import org.sonar.api.config.Configuration;

public class LintConfiguration implements Configuration {

  @Override
  public Optional<String> get(String s) {
    return Optional.empty();
  }

  @Override
  public boolean hasKey(String s) {
    return false;
  }

  @Override
  public String[] getStringArray(String s) {
    return new String[0];
  }
}
