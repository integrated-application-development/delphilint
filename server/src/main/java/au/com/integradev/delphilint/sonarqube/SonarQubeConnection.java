/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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

  private QualityProfile getQualityProfile() {
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

  public Set<SonarQubeRule> getRules() {
    String apiPath =
        "/api/rules/search?ps=500&activation=true"
            + "&f=name,htmlDesc,severity"
            + "&language="
            + languageKey;

    QualityProfile profile = getQualityProfile();
    if (profile != null) {
      apiPath += "&qprofile=" + profile.getKey();
    }

    var rootNode = api.getJson(apiPath);
    if (rootNode == null) {
      LOG.error("No JSON response from SonarQube for rule information retrieval");
      return Collections.emptySet();
    }

    var rulesArray = rootNode.get("rules");
    Set<SonarQubeRule> ruleSet = new HashSet<>();

    for (var rule : rulesArray) {
      try {
        SonarQubeRule sonarQubeRule = jsonMapper.treeToValue(rule, SonarQubeRule.class);
        ruleSet.add(sonarQubeRule);
      } catch (JsonProcessingException e) {
        LOG.error("Malformed rule info response from SonarQube: " + e.getMessage());
        return Collections.emptySet();
      }
    }

    return ruleSet;
  }

  public ConnectedList<SonarQubeIssue> getResolvedIssues() {
    if (projectKey.isEmpty()) {
      return null;
    }

    return new ConnectedList<>(
        api,
        "/api/issues/search?resolved=true&componentKeys=" + projectKey,
        "issues",
        SonarQubeIssue.class);
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
