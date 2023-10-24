/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
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

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.sonarsource.sonarlint.core.analysis.api.ClientInputFileEdit;
import org.sonarsource.sonarlint.core.analysis.api.QuickFix;

public class DelphiQuickFix {
  private static final Logger LOG = LogManager.getLogger(DelphiQuickFix.class);

  private final String message;
  private final List<DelphiTextEdit> textEdits;

  public DelphiQuickFix(QuickFix quickFix) {
    message = quickFix.message();

    if (quickFix.inputFileEdits().size() > 1) {
      LOG.warn("A quick fix attempted to edit more than one file, which is unsupported");
    }

    textEdits =
        quickFix.inputFileEdits().stream()
            .findFirst()
            .map(DelphiQuickFix::convertInputFileEdit)
            .orElse(Collections.emptyList());
  }

  private static List<DelphiTextEdit> convertInputFileEdit(ClientInputFileEdit fileEdit) {
    return fileEdit.textEdits().stream()
        .map(edit -> new DelphiTextEdit(edit.newText(), new TextRange(edit.range())))
        .collect(Collectors.toList());
  }

  public String message() {
    return message;
  }

  public List<DelphiTextEdit> textEdits() {
    return textEdits;
  }
}
