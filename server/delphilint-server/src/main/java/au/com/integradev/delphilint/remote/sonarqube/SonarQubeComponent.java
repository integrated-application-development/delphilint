package au.com.integradev.delphilint.remote.sonarqube;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SonarQubeComponent {
  @JsonProperty private String key;
  @JsonProperty private String path;

  public String getKey() {
    return key;
  }

  public String getPath() {
    return path;
  }
}
