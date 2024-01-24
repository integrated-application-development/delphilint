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
package au.com.integradev.delphilint.remote;

import au.com.integradev.delphilint.analysis.TextRange;
import java.nio.file.Path;
import java.util.Map;

public final class RemoteIssue {
  private final String rule;
  private final int line;
  private final String hash;
  private TextRange textRange;
  private final String message;
  private final RuleSeverity severity;
  private final RuleType type;
  private final IssueStatus status;
  private final IssueLikeType likeType;
  private final RemoteCleanCode cleanCode;
  private final String assignee;
  private final String creationDate;
  private final String resolution;

  private RemoteIssue(
      String rule,
      int line,
      String hash,
      TextRange textRange,
      String message,
      RuleSeverity severity,
      RuleType type,
      RemoteCleanCode cleanCode,
      IssueStatus status,
      IssueLikeType likeType,
      String assignee,
      String creationDate,
      String resolution) {
    this.rule = rule;
    this.line = line;
    this.hash = hash;
    this.textRange = textRange;
    this.message = message;
    this.severity = severity;
    this.type = type;
    this.status = status;
    this.cleanCode = cleanCode;
    this.likeType = likeType;
    this.assignee = assignee;
    this.creationDate = creationDate;
    this.resolution = resolution;
  }

  public String getRuleKey() {
    return rule;
  }

  public RuleSeverity getSeverity() {
    return severity;
  }

  public String getMessage() {
    return message;
  }

  public RuleType getType() {
    return type;
  }

  public Integer getLine() {
    return line;
  }

  public String getLineHash() {
    return hash;
  }

  public TextRange getTextRange() {
    if (textRange == null) {
      textRange = new TextRange();
    }
    return textRange;
  }

  public IssueStatus getStatus() {
    return status;
  }

  public String getAssignee() {
    return assignee;
  }

  public String getCreationDate() {
    return creationDate;
  }

  public String getResolution() {
    return resolution;
  }

  public IssueLikeType getLikeType() {
    return likeType;
  }

  public RemoteCleanCode getCleanCode() {
    return cleanCode;
  }

  public static class Builder {
    private String rule = "";
    private int line = -1;
    private String hash;
    private Path hashPath;
    private TextRange textRange;
    private String message = "";
    private RuleSeverity severity = RuleSeverity.MAJOR;
    private RuleType type = RuleType.CODE_SMELL;
    private IssueStatus status = IssueStatus.OPEN;
    private IssueLikeType likeType = IssueLikeType.ISSUE;
    private String assignee = "";
    private String creationDate = "";
    private String resolution = "";
    private RemoteCleanCode cleanCode;

    public Builder withRuleKey(String ruleKey) {
      this.rule = ruleKey;
      return this;
    }

    public Builder withMessage(String message) {
      this.message = message;
      return this;
    }

    public Builder withRange(TextRange textRange) {
      this.textRange = textRange;
      if (textRange != null) {
        this.line = textRange.getStartLine();
      }
      return this;
    }

    public Builder withHash(String hash) {
      this.hash = hash;
      return this;
    }

    public Builder withHashFrom(Path filePath) {
      this.hashPath = filePath;
      return this;
    }

    public Builder withRange(int startLine, int startOffset, int endLine, int endOffset) {
      this.textRange = new TextRange(startLine, startOffset, endLine, endOffset);
      this.line = textRange.getStartLine();
      return this;
    }

    public Builder withSeverity(RuleSeverity severity) {
      this.severity = severity;
      return this;
    }

    public Builder withType(RuleType type) {
      this.type = type;
      return this;
    }

    public Builder withCleanCode(
        CleanCodeAttribute attribute, Map<SoftwareQuality, ImpactSeverity> impactedQualities) {
      this.cleanCode = new RemoteCleanCode(attribute, impactedQualities);
      return this;
    }

    public Builder withLikeType(IssueLikeType likeType) {
      this.likeType = likeType;
      return this;
    }

    public Builder withStatus(IssueStatus status) {
      this.status = status;
      return this;
    }

    public Builder withServerMetadata(String assignee, String creationDate) {
      this.assignee = assignee;
      this.creationDate = creationDate;
      return this;
    }

    public Builder withResolution(String resolution) {
      this.resolution = resolution;
      return this;
    }

    public RemoteIssue build() {
      if (hash == null && hashPath != null) {
        if (textRange == null) {
          throw new IllegalStateException(
              "Cannot build hash from file as text range has not been provided");
        }

        hash = SonarHasher.hashFileRange(hashPath, textRange);
      }

      return new RemoteIssue(
          rule,
          line,
          hash,
          textRange,
          message,
          severity,
          type,
          cleanCode,
          status,
          likeType,
          assignee,
          creationDate,
          resolution);
    }
  }
}
