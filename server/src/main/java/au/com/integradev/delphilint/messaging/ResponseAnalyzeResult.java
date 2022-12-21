package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Set;
import java.util.stream.Collectors;

public class ResponseAnalyzeResult {
  @JsonProperty private Set<Issue> issues;

  private ResponseAnalyzeResult(Set<Issue> issues) {
    this.issues = issues;
  }

  public static ResponseAnalyzeResult fromIssueSet(
      Set<org.sonarsource.sonarlint.core.analysis.api.Issue> sonarIssues) {
    Set<Issue> issues =
        sonarIssues.stream()
            .map(
                sonarIssue -> {
                  Range range = null;
                  if (sonarIssue.getTextRange() != null) {
                    range =
                        new Range(
                            sonarIssue.getTextRange().getStartLine(),
                            sonarIssue.getTextRange().getStartLineOffset(),
                            sonarIssue.getTextRange().getEndLine(),
                            sonarIssue.getTextRange().getEndLineOffset());
                  }

                  String path = "";
                  if (sonarIssue.getInputFile() != null) {
                    path = sonarIssue.getInputFile().relativePath();
                  }

                  return new Issue(sonarIssue.getRuleKey(), sonarIssue.getMessage(), path, range);
                })
            .collect(Collectors.toSet());

    return new ResponseAnalyzeResult(issues);
  }

  public static class Issue {
    @JsonProperty private String ruleKey;
    @JsonProperty private String message;
    @JsonProperty private String file;
    @JsonProperty private Range range;

    public Issue(String ruleKey, String message, String file, Range range) {
      this.ruleKey = ruleKey;
      this.message = message;
      this.file = file;
      this.range = range;
    }
  }

  public static class Range {
    @JsonProperty private final int startLine;
    @JsonProperty private final int startLineOffset;
    @JsonProperty private final int endLine;
    @JsonProperty private final int endLineOffset;

    public Range(int startLine, int startLineOffset, int endLine, int endLineOffset) {
      this.startLine = startLine;
      this.startLineOffset = startLineOffset;
      this.endLine = endLine;
      this.endLineOffset = endLineOffset;
    }
  }
}
