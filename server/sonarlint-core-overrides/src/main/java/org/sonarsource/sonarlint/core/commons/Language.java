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
  DELPHI;

  private static final String LANGUAGE_KEY = "delph";
  private static final String PLUGIN_KEY = "delphi";
  private static final String LABEL = "Delphi";
  private static final String[] DEFAULT_FILE_SUFFIXES = new String[] {".pas", ".dpr", ".dpk"};
  private static final String FILE_SUFFIXES_PROP_KEY = "sonar.delphi.file.suffixes";

  public String getLanguageKey() {
    return LANGUAGE_KEY;
  }

  public String getPluginKey() {
    return PLUGIN_KEY;
  }

  public String getLabel() {
    return LABEL;
  }

  public String[] getDefaultFileSuffixes() {
    return DEFAULT_FILE_SUFFIXES;
  }

  public String getFileSuffixesPropKey() {
    return FILE_SUFFIXES_PROP_KEY;
  }

  public boolean shouldSyncInConnectedMode() {
    return true;
  }

  public static Set<Language> getLanguagesByPluginKey(String pluginKey) {
    if (pluginKey.equals(PLUGIN_KEY)) {
      return Set.of(Language.DELPHI);
    } else {
      return Collections.emptySet();
    }
  }

  public static boolean containsPlugin(String pluginKey) {
    return pluginKey.equals(PLUGIN_KEY);
  }

  public static Optional<Language> forKey(String languageKey) {
    if (languageKey.equals(LANGUAGE_KEY)) {
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
