package au.com.integradev.delphilint.logclean;

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
    int deletedFiles = 0;
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
