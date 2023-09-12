package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.ImpactSeverity;
import au.com.integradev.delphilint.remote.SoftwareQuality;
import com.fasterxml.jackson.annotation.JsonProperty;

public class SonarQubeQualityImpact {
  @JsonProperty private SoftwareQuality softwareQuality;
  @JsonProperty private ImpactSeverity severity;

  public SoftwareQuality getSoftwareQuality() {
    return softwareQuality;
  }

  public ImpactSeverity getSeverity() {
    return severity;
  }
}
