/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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
package au.com.integradev.delphilint.analysis;

import au.com.integradev.delphilint.sonarqube.SonarQubeIssue;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.codec.digest.DigestUtils;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.commons.HotspotReviewStatus;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;
import org.sonarsource.sonarlint.core.commons.TextRange;
import org.sonarsource.sonarlint.core.commons.TextRangeWithHash;
import org.sonarsource.sonarlint.core.issuetracking.Trackable;

public class TrackableWrappers {
  private TrackableWrappers() {
    // Utility class
  }

  public static class ClientTrackable implements Trackable<Issue> {
    private static final Pattern MATCH_ALL_WHITESPACES = Pattern.compile("\\s");

    private final Issue issue;

    public ClientTrackable(Issue issue) {
      this.issue = issue;
    }

    @Override
    public Issue getClientObject() {
      return issue;
    }

    @Override
    public String getRuleKey() {
      return issue.getRuleKey();
    }

    @Override
    public IssueSeverity getSeverity() {
      return IssueSeverity.INFO;
    }

    @Override
    public String getMessage() {
      return issue.getMessage();
    }

    @Override
    public RuleType getType() {
      return RuleType.CODE_SMELL;
    }

    @Override
    public Integer getLine() {
      return issue.getStartLine();
    }

    @Override
    public String getLineHash() {
      if (issue.getTextRange() == null) return null;

      try (BufferedReader reader =
          new BufferedReader(new InputStreamReader(issue.getInputFile().inputStream()))) {

        StringBuilder codeSnippetBuilder = new StringBuilder();

        String line = reader.readLine();
        if (line == null) {
          return null;
        }
        int i = 1;

        do {
          processLine(codeSnippetBuilder, i, line);

          line = reader.readLine();
          i++;
        } while (line != null);

        Matcher matchAllWhitespaces = MATCH_ALL_WHITESPACES.matcher(codeSnippetBuilder.toString());
        return DigestUtils.md5Hex(matchAllWhitespaces.replaceAll(""));
      } catch (IOException e) {
        return null;
      }
    }

    private void processLine(StringBuilder codeSnippet, int lineNumber, String line) {
      // The SonarQube source code suggests that it hashes only the characters in
      // the text range. Experimental results have shown this not to be the case -
      // the server provides MD5 hashes of the entire start line.
      TextRange textRange = issue.getTextRange();
      if (textRange != null && textRange.getStartLine() == lineNumber) {
        codeSnippet.append(line);
      }
    }

    @Override
    public TextRangeWithHash getTextRange() {
      return new TextRangeWithHash(
          issue.getStartLine(),
          issue.getStartLineOffset(),
          issue.getEndLine(),
          issue.getEndLineOffset(),
          getLineHash());
    }

    @Override
    public Long getCreationDate() {
      return null;
    }

    @Override
    public String getServerIssueKey() {
      return "";
    }

    @Override
    public boolean isResolved() {
      return false;
    }

    @Override
    public HotspotReviewStatus getReviewStatus() {
      return null;
    }
  }

  public static class ServerTrackable implements Trackable<SonarQubeIssue> {
    private final SonarQubeIssue issue;

    public ServerTrackable(SonarQubeIssue issue) {
      this.issue = issue;
    }

    @Override
    public SonarQubeIssue getClientObject() {
      return issue;
    }

    @Override
    public String getRuleKey() {
      return issue.getRuleKey();
    }

    @Override
    public IssueSeverity getSeverity() {
      return issue.getSeverity();
    }

    @Override
    public String getMessage() {
      return issue.getMessage();
    }

    @Override
    public RuleType getType() {
      return issue.getType();
    }

    @Override
    public Integer getLine() {
      return issue.getLine();
    }

    @Override
    public String getLineHash() {
      return issue.getLineHash();
    }

    @Override
    public TextRangeWithHash getTextRange() {
      return new TextRangeWithHash(
          issue.getTextRange().getStartLine(),
          issue.getTextRange().getStartOffset(),
          issue.getTextRange().getEndLine(),
          issue.getTextRange().getEndOffset(),
          issue.getLineHash());
    }

    @Override
    public Long getCreationDate() {
      return null;
    }

    @Override
    public String getServerIssueKey() {
      return issue.getServerIssueKey();
    }

    @Override
    public boolean isResolved() {
      return true;
    }

    @Override
    public HotspotReviewStatus getReviewStatus() {
      return null;
    }
  }
}
