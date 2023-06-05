package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.sonarlint.core.analysis.api.ActiveRule;

public class SonarQubeConnection {
  private static final Logger LOG = Loggers.get(SonarQubeConnection.class);
  private final String projectKey;
  private final String languageKey;
  private final ObjectMapper jsonMapper;
  private final ApiConnection api;

  public SonarQubeConnection(String hostUrl, String projectKey, String languageKey) {
    this.api = new ApiConnection(hostUrl);
    this.projectKey = projectKey;
    this.languageKey = languageKey;
    this.jsonMapper = new ObjectMapper();
  }

  public QualityProfile getQualityProfile() {
    String url = "/api/qualityprofiles/search?language=" + languageKey;
    if (projectKey.isEmpty()) {
      url += "&defaults=true";
    } else {
      url += "&project=" + projectKey;
    }

    var rootNode = api.getJson(url);

    if (rootNode != null) {
      var profilesArray = rootNode.get("profiles");
      if (profilesArray != null && profilesArray.size() > 0) {
        var profile = profilesArray.get(0);
        try {
          return jsonMapper.treeToValue(profile, QualityProfile.class);
        } catch (JsonProcessingException e) {
          LOG.error("Problem parsing quality profile json: " + e.getMessage());
          return null;
        }
      } else {
        var errorsArray = rootNode.get("errors");
        if (errorsArray != null && errorsArray.size() > 0) {
          var errorMessage = errorsArray.get(0).get("msg");
          if (errorMessage != null) {
            throw new IllegalArgumentException(
                "Error retrieving quality profile: " + errorMessage.asText());
          }
        }
        LOG.error("Malformed quality profile response from SonarQube");
      }
    } else {
      LOG.error("No default quality profile for language key " + languageKey);
    }

    LOG.error("Null quality profile");
    return null;
  }

  public Map<String, String> getRuleNamesByRuleKey() {
    var profile = getQualityProfile();
    if (profile == null) return Collections.emptyMap();

    var rootNode =
        api.getJson(
            "/api/rules/search?ps=500&activation=true&f=name&language="
                + languageKey
                + "&qprofile="
                + profile.getKey());
    if (rootNode == null) return Collections.emptyMap();

    var rulesArray = rootNode.get("rules");
    var ruleMap = new HashMap<String, String>();

    for (var rule : rulesArray) {
      ruleMap.put(rule.get("key").asText(), rule.get("name").asText());
    }

    return ruleMap;
  }

  public ConnectedList<ServerIssue> getResolvedIssues() {
    if (projectKey.isEmpty()) {
      return null;
    }

    return new ConnectedList<>(
        api,
        "/api/issues/search?resolved=true&componentKeys=" + projectKey,
        "issues",
        ServerIssue.class);
  }

  public Set<ActiveRule> getActiveRules() {
    var profile = getQualityProfile();
    if (profile == null) return Collections.emptySet();

    var rootNode =
        api.getJson(
            "/api/rules/search?ps=500&activation=true&f=actives,templateKey&language="
                + languageKey
                + "&qprofile="
                + profile.getKey());
    if (rootNode == null) return Collections.emptySet();

    var rulesArray = rootNode.get("rules");

    var activesArray = rootNode.get("actives");

    var paramsMap = new HashMap<String, Map<String, String>>();

    activesArray
        .fieldNames()
        .forEachRemaining(
            activeKey -> {
              var activeNode = activesArray.get(activeKey);
              var paramsArray = activeNode.get(0).get("params");

              if (paramsArray != null && paramsArray.size() > 0) {
                var thisParamsMap = new HashMap<String, String>();

                for (var paramNode : paramsArray) {
                  thisParamsMap.put(paramNode.get("key").asText(), paramNode.get("value").asText());
                }

                paramsMap.put(activeKey, thisParamsMap);
              }
            });

    var activeRules = new HashSet<ActiveRule>();
    for (var ruleNode : rulesArray) {
      var ruleKey = ruleNode.get("key").asText();
      var rule = new ActiveRule(ruleKey, languageKey);

      var templateKeyNode = ruleNode.get("templateKey");
      if (templateKeyNode != null) {
        rule.setTemplateRuleKey(templateKeyNode.asText());
      }

      if (paramsMap.containsKey(ruleKey)) {
        rule.setParams(paramsMap.get(ruleKey));
      }

      activeRules.add(rule);
    }

    return activeRules;
  }
}
