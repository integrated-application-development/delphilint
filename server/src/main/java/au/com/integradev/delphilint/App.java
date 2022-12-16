package au.com.integradev.delphilint;

import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
import java.nio.file.Path;
import java.util.Set;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.commons.log.SonarLintLogger;

public class App {
  public static void main(String[] args) {
    var logOutput = new DelphiLintLogOutput();
    SonarLintLogger.setTarget(logOutput);

    // TODO: Get BDS path and compiler version from IDE
    var delphiConfig =
        new StandaloneDelphiConfiguration(
            "{PATH REMOVED} Files (x86)/Embarcadero/Studio/22.0", "VER350");

    Set<Issue> issues;

    var sonarqube =
        new SonarQubeConnection("{URL REMOVED}", "", Language.DELPHI.getLanguageKey());

    try (var engine = new DelphiAnalysisEngine(delphiConfig, sonarqube)) {
      // TODO: Get paths from IDE, including main dproj file
      issues =
          engine.analyze(
              Path.of("{PATH REMOVED}"),
              Set.of(
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}"),
                  Path.of(
                      "{PATH REMOVED}")),
              null);
    }

    System.out.println("Identified " + issues.size() + " issues.");
    for (var issue : issues) {
      System.out.println(
          issue.getRuleKey()
              + " at line "
              + issue.getStartLine()
              + " in "
              + issue.getInputFile().relativePath());
    }
  }
}
