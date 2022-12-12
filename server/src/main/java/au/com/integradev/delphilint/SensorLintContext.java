package au.com.integradev.delphilint;

import java.io.Serializable;
import org.sonar.api.SonarRuntime;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.InputModule;
import org.sonar.api.batch.rule.ActiveRules;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.code.NewSignificantCode;
import org.sonar.api.batch.sensor.coverage.NewCoverage;
import org.sonar.api.batch.sensor.cpd.NewCpdTokens;
import org.sonar.api.batch.sensor.error.NewAnalysisError;
import org.sonar.api.batch.sensor.highlighting.NewHighlighting;
import org.sonar.api.batch.sensor.issue.NewExternalIssue;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.measure.NewMeasure;
import org.sonar.api.batch.sensor.rule.NewAdHocRule;
import org.sonar.api.batch.sensor.symbol.NewSymbolTable;
import org.sonar.api.config.Configuration;
import org.sonar.api.config.Settings;
import org.sonar.api.scanner.fs.InputProject;
import org.sonar.api.utils.Version;

public class SensorLintContext implements SensorContext {
  private final LintConfiguration configuration;
  private final LintActiveRules activeRules;
  private final LintFileSystem fs;

  public SensorLintContext() {
    configuration = new LintConfiguration();
    activeRules = new LintActiveRules();
    fs = new LintFileSystem();
  }

  @SuppressWarnings("deprecation")
  @Override
  public Settings settings() {
    return null;
  }

  @Override
  public Configuration config() {
    return configuration;
  }

  @Override
  public FileSystem fileSystem() {
    return fs;
  }

  @Override
  public ActiveRules activeRules() {
    return activeRules;
  }

  @SuppressWarnings("deprecation")
  @Override
  public InputModule module() {
    return null;
  }

  @Override
  public InputProject project() {
    return null;
  }

  @Override
  public Version getSonarQubeVersion() {
    return null;
  }

  @Override
  public SonarRuntime runtime() {
    return null;
  }

  @Override
  public boolean isCancelled() {
    return false;
  }

  @Override
  public <G extends Serializable> NewMeasure<G> newMeasure() {
    return null;
  }

  @Override
  public NewIssue newIssue() {
    return null;
  }

  @Override
  public NewExternalIssue newExternalIssue() {
    return null;
  }

  @Override
  public NewAdHocRule newAdHocRule() {
    return null;
  }

  @Override
  public NewHighlighting newHighlighting() {
    return null;
  }

  @Override
  public NewSymbolTable newSymbolTable() {
    return null;
  }

  @Override
  public NewCoverage newCoverage() {
    return null;
  }

  @Override
  public NewCpdTokens newCpdTokens() {
    return null;
  }

  @Override
  public NewAnalysisError newAnalysisError() {
    return null;
  }

  @Override
  public NewSignificantCode newSignificantCode() {
    return null;
  }

  @Override
  public void addContextProperty(String s, String s1) {

  }

  @Override
  public void markForPublishing(InputFile inputFile) {

  }
}
