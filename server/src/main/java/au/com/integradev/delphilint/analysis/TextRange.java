package au.com.integradev.delphilint.analysis;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TextRange {
  @JsonProperty private final int startLine;
  @JsonProperty private final int startOffset;
  @JsonProperty private final int endLine;
  @JsonProperty private final int endOffset;

  public TextRange() {
    this.startLine = 0;
    this.startOffset = 0;
    this.endLine = 0;
    this.endOffset = 0;
  }

  public TextRange(int startLine, int startLineOffset, int endLine, int endLineOffset) {
    this.startLine = startLine;
    this.startOffset = startLineOffset;
    this.endLine = endLine;
    this.endOffset = endLineOffset;
  }

  public int getStartLine() {
    return startLine;
  }

  public int getStartOffset() {
    return startOffset;
  }

  public int getEndLine() {
    return endLine;
  }

  public int getEndOffset() {
    return endOffset;
  }
}
