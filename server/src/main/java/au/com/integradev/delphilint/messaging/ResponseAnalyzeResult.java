package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;
import au.com.integradev.delphilint.analysis.DelphiLintIssue;
import au.com.integradev.delphilint.analysis.TextRange;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Collectors;

public class ResponseAnalyzeResult {
  @JsonProperty private Set<DelphiLintIssue> issues;

  private ResponseAnalyzeResult(Set<DelphiLintIssue> issues) {
    this.issues = issues;
  }

  public void convertPathsToAbsolute(Path baseDir) {
    issues.forEach(issue -> issue.setFile(baseDir.resolve(issue.getFile()).toString()));
  }

  public static ResponseAnalyzeResult fromIssueSet(
      Set<org.sonarsource.sonarlint.core.analysis.api.Issue> sonarIssues) {
    Set<DelphiLintIssue> issues =
        sonarIssues.stream()
            .map(
                sonarIssue -> {
                  TextRange range = null;
                  if (sonarIssue.getTextRange() != null) {
                    range =
                        new TextRange(
                            sonarIssue.getTextRange().getStartLine(),
                            sonarIssue.getTextRange().getStartLineOffset(),
                            sonarIssue.getTextRange().getEndLine(),
                            sonarIssue.getTextRange().getEndLineOffset());
                  }

                  String path = "";
                  if (sonarIssue.getInputFile() != null) {
                    path = sonarIssue.getInputFile().relativePath();
                  }

                  return new DelphiLintIssue(
                      sonarIssue.getRuleKey(), sonarIssue.getMessage(), path, range);
                })
            .collect(Collectors.toSet());

    return new ResponseAnalyzeResult(issues);
  }
}
