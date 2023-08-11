/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
 *
 * SonarLint Core - Commons
 * Copyright (C) 2016-2023 SonarSource SA
 * mailto:info AT sonarsource DOT com
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
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonarsource.sonarlint.core.commons;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;

public enum Language {
  DELPHI(
      "delphi",
      "communitydelphi",
      "Delphi",
      new String[] {".pas", ".dpr", ".dpk"},
      "sonar.delphi.file.suffixes"),
  // Some SonarLint code relies on specific language values being present.
  JS;

  private final String languageKey;
  private final String pluginKey;
  private final String label;
  private final String[] defaultFileSuffixes;
  private final String fileSuffixesPropKey;

  Language() {
    this("", "", "", new String[0], "");
  }

  Language(
      String languageKey,
      String pluginKey,
      String label,
      String[] defaultFileSuffixes,
      String fileSuffixesPropKey) {
    this.languageKey = languageKey;
    this.pluginKey = pluginKey;
    this.label = label;
    this.defaultFileSuffixes = defaultFileSuffixes;
    this.fileSuffixesPropKey = fileSuffixesPropKey;
  }

  public String getLanguageKey() {
    return languageKey;
  }

  public String getPluginKey() {
    return pluginKey;
  }

  public String getLabel() {
    return label;
  }

  public String[] getDefaultFileSuffixes() {
    return defaultFileSuffixes;
  }

  public String getFileSuffixesPropKey() {
    return fileSuffixesPropKey;
  }

  public boolean shouldSyncInConnectedMode() {
    return true;
  }

  public static Set<Language> getLanguagesByPluginKey(String pluginKey) {
    if (pluginKey.equals(DELPHI.pluginKey)) {
      return Set.of(Language.DELPHI);
    } else {
      return Collections.emptySet();
    }
  }

  public static boolean containsPlugin(String pluginKey) {
    return pluginKey.equals(DELPHI.pluginKey);
  }

  public static Optional<Language> forKey(String languageKey) {
    if (languageKey.equals(DELPHI.languageKey)) {
      return Optional.of(DELPHI);
    } else {
      return Optional.empty();
    }
  }

  @Override
  public String toString() {
    return getLabel();
  }
}
