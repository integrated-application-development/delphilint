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
package au.com.integradev.delphilint.server.message;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.nio.file.Path;
import java.util.Set;

public class RequestAnalyze {
  @JsonProperty private Path baseDir;
  @JsonProperty private Set<Path> inputFiles;
  @JsonProperty private String sonarHostUrl;
  @JsonProperty private String projectKey;
  @JsonProperty private String apiToken;
  @JsonProperty private String projectPropertiesPath;
  @JsonProperty private Set<String> disabledRules;

  public Path getBaseDir() {
    return baseDir;
  }

  public Set<Path> getInputFiles() {
    return inputFiles;
  }

  public String getSonarHostUrl() {
    return sonarHostUrl;
  }

  public String getProjectKey() {
    return projectKey;
  }

  public String getApiToken() {
    return apiToken;
  }

  public String getProjectProperties() {
    return projectPropertiesPath;
  }

  public Set<String> getDisabledRules() {
    return disabledRules;
  }
}
