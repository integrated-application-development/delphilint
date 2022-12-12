package au.com.integradev.delphilint;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.sonar.api.config.Configuration;

public class LintConfiguration implements Configuration {
  private final Map<String, String> values;
  private final Map<String, String[]> arrayValues;

  public LintConfiguration() {
    values = new HashMap<>();
    values.put(
        "sonar.delphi.bds.path",
        "{PATH REMOVED} Files (x86)/Embarcadero/Studio/22.0");
    arrayValues = new HashMap<>();
    arrayValues.put(
        "sonar.delphi.sources.searchPath",
        new String[] {"{PATH REMOVED}"});
  }

  @Override
  public Optional<String> get(String s) {
    return Optional.ofNullable(values.getOrDefault(s, null));
  }

  @Override
  public boolean hasKey(String s) {
    return values.containsKey(s) || arrayValues.containsKey(s);
  }

  @Override
  public String[] getStringArray(String s) {
    return arrayValues.getOrDefault(s, new String[] {});
  }
}
