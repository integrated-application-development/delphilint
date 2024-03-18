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
package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;

public enum CleanCodeAttribute {
  FORMATTED(CleanCodeAttributeCategory.CONSISTENT),
  CONVENTIONAL(CleanCodeAttributeCategory.CONSISTENT),
  IDENTIFIABLE(CleanCodeAttributeCategory.CONSISTENT),
  CLEAR(CleanCodeAttributeCategory.INTENTIONAL),
  LOGICAL(CleanCodeAttributeCategory.INTENTIONAL),
  COMPLETE(CleanCodeAttributeCategory.INTENTIONAL),
  EFFICIENT(CleanCodeAttributeCategory.INTENTIONAL),
  FOCUSED(CleanCodeAttributeCategory.ADAPTABLE),
  DISTINCT(CleanCodeAttributeCategory.ADAPTABLE),
  MODULAR(CleanCodeAttributeCategory.ADAPTABLE),
  TESTED(CleanCodeAttributeCategory.ADAPTABLE),
  LAWFUL(CleanCodeAttributeCategory.RESPONSIBLE),
  TRUSTWORTHY(CleanCodeAttributeCategory.RESPONSIBLE),
  RESPECTFUL(CleanCodeAttributeCategory.RESPONSIBLE);

  private final CleanCodeAttributeCategory category;

  CleanCodeAttribute(CleanCodeAttributeCategory category) {
    this.category = category;
  }

  public CleanCodeAttributeCategory getCategory() {
    return category;
  }

  public static CleanCodeAttribute fromSonarLintCleanCodeAttribute(
      org.sonarsource.sonarlint.core.commons.CleanCodeAttribute attr) {
    switch (attr) {
      case CONVENTIONAL:
        return CONVENTIONAL;
      case FORMATTED:
        return FORMATTED;
      case IDENTIFIABLE:
        return IDENTIFIABLE;
      case CLEAR:
        return CLEAR;
      case COMPLETE:
        return COMPLETE;
      case EFFICIENT:
        return EFFICIENT;
      case LOGICAL:
        return LOGICAL;
      case DISTINCT:
        return DISTINCT;
      case FOCUSED:
        return FOCUSED;
      case MODULAR:
        return MODULAR;
      case TESTED:
        return TESTED;
      case LAWFUL:
        return LAWFUL;
      case RESPECTFUL:
        return RESPECTFUL;
      case TRUSTWORTHY:
        return TRUSTWORTHY;
      default:
        throw new NotImplementedException("Unknown clean code attribute");
    }
  }
}
