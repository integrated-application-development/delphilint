package au.com.integradev.delphilint.remote;

import java.util.Map;

public class RemoteCleanCode {
  private final CleanCodeAttribute attribute;
  private final Map<SoftwareQuality, ImpactSeverity> impactedQualities;

  public RemoteCleanCode(
      CleanCodeAttribute attribute, Map<SoftwareQuality, ImpactSeverity> impactedQualities) {
    this.attribute = attribute;
    this.impactedQualities = impactedQualities;
  }

  public CleanCodeAttribute getAttribute() {
    return attribute;
  }

  public Map<SoftwareQuality, ImpactSeverity> getImpactedQualities() {
    return impactedQualities;
  }
}
