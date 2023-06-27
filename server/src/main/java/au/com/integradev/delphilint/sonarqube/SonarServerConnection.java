package au.com.integradev.delphilint.sonarqube;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public interface SonarServerConnection {
  Map<String, String> getRuleNamesByRuleKey() throws ApiException;

  Set<RemoteRule> getRules() throws ApiException;

  Collection<RemoteIssue> getResolvedIssues(Set<String> relativeFilePaths);

  Set<RemoteActiveRule> getActiveRules() throws ApiException;
}
