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
package au.com.integradev.delphilint;

import au.com.integradev.delphilint.server.LintServer;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {
  private static final Logger LOG = LogManager.getLogger(App.class);

  public static void main(String[] args) throws IOException {
    Path settingsPath = Path.of(System.getenv("APPDATA"), "DelphiLint");
    Path pluginsPath = settingsPath.resolve("plugins");

    if (!Files.exists(pluginsPath)) {
      Files.createDirectory(pluginsPath);
    }

    var server = new LintServer(pluginsPath);

    if (args.length > 0) {
      File portFile = new File(args[0]);
      if (portFile.exists()) {
        Files.write(
            portFile.toPath(), String.valueOf(server.getPort()).getBytes(StandardCharsets.UTF_8));
        LOG.info("Server port written to port file at {}", portFile.toPath());
      } else {
        LOG.info("Port file at {} does not exist", portFile.toPath());
      }
    }

    server.run();
    LOG.info("Application stopped");
  }
}
