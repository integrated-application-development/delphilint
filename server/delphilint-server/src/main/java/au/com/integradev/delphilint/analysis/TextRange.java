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

  public TextRange(org.sonarsource.sonarlint.core.commons.TextRange textRange) {
    this(
        textRange.getStartLine(),
        textRange.getStartLineOffset(),
        textRange.getEndLine(),
        textRange.getEndLineOffset());
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
