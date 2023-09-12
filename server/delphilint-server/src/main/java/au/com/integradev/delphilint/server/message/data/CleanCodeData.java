package au.com.integradev.delphilint.server.message.data;

import au.com.integradev.delphilint.remote.CleanCodeAttribute;
import au.com.integradev.delphilint.remote.CleanCodeAttributeCategory;
import au.com.integradev.delphilint.remote.ImpactSeverity;
import au.com.integradev.delphilint.remote.RemoteCleanCode;
import au.com.integradev.delphilint.remote.SoftwareQuality;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;

public class CleanCodeData {
  @JsonProperty private CleanCodeAttribute attribute;
  @JsonProperty private CleanCodeAttributeCategory category;
  @JsonProperty private Map<SoftwareQuality, ImpactSeverity> impacts;

  public CleanCodeData(RemoteCleanCode cleanCode) {
    this.attribute = cleanCode.getAttribute();
    this.category = this.attribute.getCategory();
    this.impacts = cleanCode.getImpactedQualities();
  }

  public CleanCodeAttribute getAttribute() {
    return attribute;
  }

  public CleanCodeAttributeCategory getCategory() {
    return category;
  }

  public Map<SoftwareQuality, ImpactSeverity> getImpacts() {
    return impacts;
  }
}
