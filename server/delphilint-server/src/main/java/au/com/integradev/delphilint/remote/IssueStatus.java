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

public enum IssueStatus {
  OPEN,
  CONFIRMED,
  REOPENED,
  RESOLVED,
  CLOSED,
  ACCEPTED,
  TO_REVIEW,
  REVIEWED;

  public static IssueStatus fromSonarQubeIssueStatus(String status) {
    switch (status) {
      case "OPEN":
        return IssueStatus.OPEN;
      case "CONFIRMED":
        return IssueStatus.CONFIRMED;
      case "REOPENED":
        return IssueStatus.REOPENED;
      case "RESOLVED":
        return IssueStatus.RESOLVED;
      case "CLOSED":
        return IssueStatus.CLOSED;
      case "ACCEPTED":
        return IssueStatus.ACCEPTED;
      case "TO_REVIEW":
        return IssueStatus.TO_REVIEW;
      case "REVIEWED":
        return IssueStatus.REVIEWED;
      default:
        throw new NotImplementedException("Unknown issue status " + status);
    }
  }
}
