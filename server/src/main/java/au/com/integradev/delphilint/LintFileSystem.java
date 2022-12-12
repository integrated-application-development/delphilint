package au.com.integradev.delphilint;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.sonar.api.batch.fs.internal.DefaultFileSystem;
import org.sonar.api.batch.fs.internal.DefaultIndexedFile;
import org.sonar.api.batch.fs.internal.DefaultInputFile;
import org.sonar.plugins.delphi.core.DelphiLanguage;

public class LintFileSystem extends DefaultFileSystem {

  public LintFileSystem(Path baseDir) {
    super(baseDir);
    setWorkDir(baseDir);
    setEncoding(Charset.defaultCharset());
    var pasFiles = Arrays.stream(baseDir.toFile().listFiles()).filter(file -> file.getName().endsWith("pas")).collect(
        Collectors.toUnmodifiableList());
    for (var file : pasFiles) {
      add(
          new DefaultInputFile(
              new DefaultIndexedFile(
                  "temp",
                  baseDir,
                  baseDir.relativize(file.toPath()).toString(),
                  DelphiLanguage.KEY),
              f -> {
                f.setMetadata()

              }));
    }
  }
}
