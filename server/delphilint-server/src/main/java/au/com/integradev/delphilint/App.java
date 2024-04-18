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
package au.com.integradev.delphilint;

import au.com.integradev.delphilint.maintenance.LogCleaner;
import au.com.integradev.delphilint.server.AnalysisServer;
import au.com.integradev.delphilint.server.TlvConnection;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {
  private static final int DEFAULT_PORT = 14000;
  private static final Logger LOG = LogManager.getLogger(App.class);
  private static final Duration LOG_CUTOFF_DURATION = Duration.ofDays(7);

  public static void main(String[] args) {
    try {
      var settingsPath = Path.of(System.getenv("APPDATA"), "DelphiLint");
      var pluginsPath = settingsPath.resolve("plugins");
      var logPath = settingsPath.resolve("logs");

      if (!Files.exists(pluginsPath)) {
        Files.createDirectory(pluginsPath);
      }

      if (Files.exists(logPath)) {
        cleanLogs(logPath);
      }

      TlvConnection connection;
      AnalysisServer server = new AnalysisServer(pluginsPath);

      if (args.length > 0) {
        connection = new TlvConnection(server);

        var portFile = Path.of(args[0]);
        if (Files.exists(portFile)) {
          Files.write(
              portFile, String.valueOf(connection.getPort()).getBytes(StandardCharsets.UTF_8));
          LOG.info("Server port written to port file at {}", portFile);
        } else {
          LOG.info("Port file at {} does not exist", portFile);
          connection = new TlvConnection(server, DEFAULT_PORT);
        }
      } else {
        connection = new TlvConnection(server, DEFAULT_PORT);
      }

      connection.run();
      LOG.info("Application stopped");
    } catch (Exception e) {
      LOG.error(e);
      System.exit(1);
    }
  }

  private static void cleanLogs(Path logPath) {
    try {
      new LogCleaner(Instant.now().minus(LOG_CUTOFF_DURATION)).clean(logPath);
    } catch (IOException e) {
      LOG.error("Could not clean logs", e);
    }
  }
}
