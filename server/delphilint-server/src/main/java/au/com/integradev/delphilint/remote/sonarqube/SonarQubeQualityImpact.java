/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
