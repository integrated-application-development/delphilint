package au.com.integradev.delphilint.server.message.data;

import au.com.integradev.delphilint.remote.IssueStatus;
import com.fasterxml.jackson.annotation.JsonProperty;

public class IssueMetadataData {
  @JsonProperty private String assignee;
  @JsonProperty private String creationDate;
  @JsonProperty private IssueStatus status;

  public IssueMetadataData(String assignee, String creationDate, IssueStatus status) {
    this.assignee = assignee;
    this.creationDate = creationDate;
    this.status = status;
  }

  public String getAssignee() {
    return assignee;
  }

  public String getCreationDate() {
    return creationDate;
  }

  public IssueStatus getStatus() {
    return status;
  }
}
