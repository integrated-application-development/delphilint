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
package au.com.integradev.delphilint.remote.sonarqube;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Version implements Comparable<Version> {
  private final List<Integer> versionParts;

  public Version(String version) throws NumberFormatException {
    this.versionParts =
        Arrays.stream(version.split("\\.", -1)).map(Integer::parseInt).collect(Collectors.toList());
  }

  @Override
  public String toString() {
    return versionParts.stream().map(Object::toString).collect(Collectors.joining("."));
  }

  @Override
  public int compareTo(Version other) {
    int maxLength = Math.max(this.versionParts.size(), other.versionParts.size());
    for (int i = 0; i < maxLength; i++) {
      int thisPart = i < this.versionParts.size() ? this.versionParts.get(i) : 0;
      int otherPart = i < other.versionParts.size() ? other.versionParts.get(i) : 0;

      if (thisPart < otherPart) {
        return -1;
      } else if (thisPart > otherPart) {
        return 1;
      }
    }
    return 0;
  }

  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    } else if (other == null) {
      return false;
    } else if (this.getClass() != other.getClass()) {
      return false;
    } else {
      return this.compareTo((Version) other) == 0;
    }
  }

  @Override
  public int hashCode() {
    return Objects.hash(versionParts);
  }
}
