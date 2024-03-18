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
package au.com.integradev.delphilint.maintenance;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LogCleaner {
  private static final Logger LOG = LogManager.getLogger(LogCleaner.class);
  private final Instant logCutoff;

  public LogCleaner(Instant cutoff) {
    logCutoff = cutoff;
  }

  public void clean(Path logPath) throws IOException {
    LOG.debug("Looking for old log files to clean...");
    List<Path> logFiles;
    try (Stream<Path> logStream = Files.list(logPath)) {
      logFiles = logStream.filter(this::isOldLog).collect(Collectors.toList());
    }

    LOG.debug("Cleaning {} old log files...", logFiles.size());
    var deletedFiles = 0;
    for (Path logFile : logFiles) {
      try {
        Files.delete(logFile);
        deletedFiles++;
      } catch (IOException e) {
        LOG.warn(e);
      }
    }
    LOG.info("{} old log files deleted", deletedFiles);
  }

  private boolean isOldLog(Path log) {
    try {
      return getLastModifiedTime(log).isBefore(logCutoff);
    } catch (IOException e) {
      LOG.debug(e);
      return false;
    }
  }

  private static Instant getLastModifiedTime(Path log) throws IOException {
    return Files.getLastModifiedTime(log).toInstant();
  }
}
