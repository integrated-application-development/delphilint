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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Duration;
import java.time.Instant;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class LogCleanerTest {
  private static final Instant NOW = Instant.parse("2024-02-15T12:34:56.00Z");
  private final Set<Path> tempDirs = new HashSet<>();

  @AfterEach
  void afterEach() throws IOException {
    for (Path path : tempDirs) {
      FileUtils.deleteDirectory(path.toFile());
    }
    tempDirs.clear();
  }

  private Path createTestLogs(Duration... logDurations) throws IOException {
    Path logDir = Files.createTempDirectory("DelphiLintServer_LogCleaner");

    tempDirs.add(logDir);

    for (Duration duration : logDurations) {
      Path file = Files.createTempFile(logDir, "log_" + duration.toString(), "");
      Files.setLastModifiedTime(file, FileTime.from(NOW.minus(duration)));
    }

    return logDir;
  }

  @Test
  void testCleansOldLogs() throws IOException {
    Path logDir =
        createTestLogs(
            Duration.parse("P7DT20H"),
            Duration.parse("P7DT1M"),
            Duration.parse("P14D"),
            Duration.parse("P365D"));

    new LogCleaner(NOW.minus(Duration.ofDays(7))).clean(logDir);

    try (Stream<Path> logs = Files.list(logDir)) {
      assertEquals(0, logs.count());
    }
  }

  @Test
  void testDoesNotCleanExactlyOnBoundary() throws IOException {
    Path logDir = createTestLogs(Duration.ofDays(7));
    new LogCleaner(NOW.minus(Duration.ofDays(7))).clean(logDir);

    try (Stream<Path> logs = Files.list(logDir)) {
      assertEquals(1, logs.count());
    }
  }

  @Test
  void testDoesNotCleanRecentOldLogs() throws IOException {
    Path logDir =
        createTestLogs(
            Duration.parse("P4D"),
            Duration.parse("P6DT23H59M"),
            Duration.parse("P1D"),
            Duration.parse("PT2H"));

    new LogCleaner(NOW.minus(Duration.ofDays(7))).clean(logDir);

    try (Stream<Path> logs = Files.list(logDir)) {
      assertEquals(4, logs.count());
    }
  }
}
