package au.com.integradev.delphilint;

import org.sonar.api.batch.sensor.code.internal.DefaultSignificantCode;
import org.sonar.api.batch.sensor.coverage.internal.DefaultCoverage;
import org.sonar.api.batch.sensor.cpd.internal.DefaultCpdTokens;
import org.sonar.api.batch.sensor.error.AnalysisError;
import org.sonar.api.batch.sensor.highlighting.internal.DefaultHighlighting;
import org.sonar.api.batch.sensor.internal.SensorStorage;
import org.sonar.api.batch.sensor.issue.Issue;
import org.sonar.api.batch.sensor.issue.internal.DefaultExternalIssue;
import org.sonar.api.batch.sensor.measure.Measure;
import org.sonar.api.batch.sensor.rule.internal.DefaultAdHocRule;
import org.sonar.api.batch.sensor.symbol.internal.DefaultSymbolTable;

public class LintStorage implements SensorStorage {

  @Override
  public void store(Measure measure) {

  }

  @Override
  public void store(Issue issue) {

  }

  @Override
  public void store(DefaultExternalIssue defaultExternalIssue) {

  }

  @Override
  public void store(DefaultAdHocRule defaultAdHocRule) {

  }

  @Override
  public void store(DefaultHighlighting defaultHighlighting) {

  }

  @Override
  public void store(DefaultCoverage defaultCoverage) {

  }

  @Override
  public void store(DefaultCpdTokens defaultCpdTokens) {

  }

  @Override
  public void store(DefaultSymbolTable defaultSymbolTable) {

  }

  @Override
  public void store(AnalysisError analysisError) {

  }

  @Override
  public void storeProperty(String s, String s1) {

  }

  @Override
  public void store(DefaultSignificantCode defaultSignificantCode) {

  }
}
