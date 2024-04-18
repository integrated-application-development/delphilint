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
package au.com.integradev.delphilint.server;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.sonarsource.sonarlint.core.commons.log.ClientLogOutput;

public class SonarDelphiLogOutput implements ClientLogOutput {
  private static final Logger LOG = LogManager.getLogger(SonarDelphiLogOutput.class);

  private String errorMessage = "";

  public boolean containsError() {
    return !errorMessage.isEmpty();
  }

  public String getError() {
    return errorMessage;
  }

  @Override
  public void log(String s, Level level) {
    LOG.debug("[{}] {}", level, s);

    if (level == Level.ERROR) {
      errorMessage = s;
    }
  }
}
