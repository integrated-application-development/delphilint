package au.com.integradev.delphilint.remote;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public interface SonarHost {
  Map<String, String> getRuleNamesByRuleKey() throws SonarHostException;

  Set<RemoteRule> getRules() throws SonarHostException;

  Collection<RemoteIssue> getResolvedIssues(Set<String> relativeFilePaths);

  Set<RemoteActiveRule> getActiveRules() throws SonarHostException;
}
