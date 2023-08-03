package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;

public enum IssueStatus {
  OPEN,
  CONFIRMED,
  REOPENED,
  RESOLVED,
  CLOSED,
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
      case "TO_REVIEW":
        return IssueStatus.TO_REVIEW;
      case "REVIEWED":
        return IssueStatus.REVIEWED;
      default:
        throw new NotImplementedException("Unknown issue status " + status);
    }
  }
}
