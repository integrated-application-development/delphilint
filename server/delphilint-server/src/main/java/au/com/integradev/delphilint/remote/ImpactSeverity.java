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
package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;

public enum ImpactSeverity {
  INFO,
  LOW,
  MEDIUM,
  HIGH,
  BLOCKER;

  public static ImpactSeverity fromSonarLintImpactSeverity(
      org.sonarsource.sonarlint.core.commons.ImpactSeverity severity) {
    switch (severity) {
      case LOW:
        return LOW;
      case MEDIUM:
        return MEDIUM;
      case HIGH:
        return HIGH;
      default:
        throw new NotImplementedException("Unknown impact severity");
    }
  }
}
