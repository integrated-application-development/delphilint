package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class QualityProfile {
  private String key;
  private String name;
  private String language;
  private int activeRuleCount;

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public String getLanguage() {
    return language;
  }

  public int getActiveRuleCount() {
    return activeRuleCount;
  }
}
