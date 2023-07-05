/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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
package au.com.integradev.delphilint.analysis;

import java.nio.file.Path;
import java.util.Map;

public class EngineStartupConfiguration {
  private final String bdsPath;
  private final String compilerVersion;
  private final Path sonarDelphiJarPath;

  public EngineStartupConfiguration(
      String bdsPath, String compilerVersion, Path sonarDelphiJarPath) {
    this.bdsPath = bdsPath;
    this.compilerVersion = compilerVersion;
    this.sonarDelphiJarPath = sonarDelphiJarPath;
  }

  public Path getSonarDelphiJarPath() {
    return sonarDelphiJarPath;
  }

  public Map<String, String> getBaseProperties() {
    return Map.of(
        "sonar.delphi.bds.path", bdsPath,
        "sonar.delphi.compiler.version", compilerVersion);
  }
}
