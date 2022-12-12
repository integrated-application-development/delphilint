package au.com.integradev.delphilint;

import java.io.File;
import java.nio.charset.Charset;
import java.util.SortedSet;
import org.jetbrains.annotations.Nullable;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FilePredicates;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputDir;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.DefaultFilePredicates;

public class LintFileSystem implements FileSystem {

  @Override
  public File baseDir() {
    return null;
  }

  @Override
  public Charset encoding() {
    return null;
  }

  @Override
  public File workDir() {
    return new File(System.getProperty("java.io.tmpdir"));
  }

  @Override
  public FilePredicates predicates() {
    return new DefaultFilePredicates(workDir().toPath());
  }

  @Nullable
  @Override
  public InputFile inputFile(FilePredicate filePredicate) {
    return null;
  }

  @SuppressWarnings("deprecation")
  @Nullable
  @Override
  public InputDir inputDir(File file) {
    return null;
  }

  @Override
  public Iterable<InputFile> inputFiles(FilePredicate filePredicate) {
    return null;
  }

  @Override
  public boolean hasFiles(FilePredicate filePredicate) {
    return false;
  }

  @SuppressWarnings("deprecation")
  @Override
  public Iterable<File> files(FilePredicate filePredicate) {
    return null;
  }

  @Override
  public SortedSet<String> languages() {
    return null;
  }

  @Override
  public File resolvePath(String s) {
    return null;
  }
}
