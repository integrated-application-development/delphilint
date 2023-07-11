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
package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.RemoteActiveRule;
import au.com.integradev.delphilint.remote.RemoteIssue;
import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;
import au.com.integradev.delphilint.remote.SonarHost;
import au.com.integradev.delphilint.remote.SonarHostException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SonarQubeHost implements SonarHost {
  private static final Logger LOG = LogManager.getLogger(SonarQubeHost.class);

  private final String projectKey;
  private final String languageKey;
  private final String pluginKey;
  private final ObjectMapper jsonMapper;
  private final ApiConnection api;

  public SonarQubeHost(
      String hostUrl, String projectKey, String languageKey, String pluginKey, String apiToken) {
    this.api = new ApiConnection(hostUrl, apiToken);
    this.projectKey = projectKey;
    this.languageKey = languageKey;
    this.pluginKey = pluginKey;
    this.jsonMapper = new ObjectMapper();
  }

  public String getName() {
    return "SonarQube instance at " + api.getHostUrl();
  }

  private SonarQubeQualityProfile getQualityProfile() throws SonarHostException {
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
          return jsonMapper.treeToValue(profile, SonarQubeQualityProfile.class);
        } catch (JsonProcessingException e) {
          throw new SonarHostException("Problem parsing quality profile JSON: " + e.getMessage());
        }
      } else {
        var errorsArray = rootNode.get("errors");
        if (errorsArray != null && errorsArray.size() > 0) {
          var errorMessage = errorsArray.get(0).get("msg");
          if (errorMessage != null) {
            throw new SonarHostException(
                "SonarQube error when retrieving quality profile: " + errorMessage.asText());
          }
        }

        throw new SonarHostException("No quality profile found for project " + projectKey);
      }
    } else {
      throw new SonarHostException("Could not retrieve quality profile from SonarQube");
    }
  }

  public Map<String, String> getRuleNamesByRuleKey() throws SonarHostException {
    var profile = getQualityProfile();
    if (profile == null) return Collections.emptyMap();

    var rootNode =
        api.getJson(
            "/api/rules/search?ps=500&activation=true&f=name&language="
                + languageKey
                + "&qprofile="
                + profile.getKey());
    if (rootNode == null) {
      throw new SonarHostException(
          "Could not retrieve rule names from SonarQube for quality profile " + profile.getName());
    }

    var rulesArray = rootNode.get("rules");
    var ruleMap = new HashMap<String, String>();

    for (var rule : rulesArray) {
      ruleMap.put(rule.get("key").asText(), rule.get("name").asText());
    }

    return ruleMap;
  }

  public Set<RemoteRule> getRules() throws SonarHostException {
    String apiPath =
        "/api/rules/search?ps=500&activation=true"
            + "&f=name,htmlDesc,severity"
            + "&language="
            + languageKey;

    SonarQubeQualityProfile profile = getQualityProfile();
    if (profile != null) {
      apiPath += "&qprofile=" + profile.getKey();
    }

    var rootNode = api.getJson(apiPath);
    if (rootNode == null) {
      throw new SonarHostException(
          "No JSON response from SonarQube for rule information retrieval");
    }

    var rulesArray = rootNode.get("rules");
    Set<RemoteRule> ruleSet = new HashSet<>();

    for (var rule : rulesArray) {
      try {
        SonarQubeRule sonarQubeRule = jsonMapper.treeToValue(rule, SonarQubeRule.class);
        ruleSet.add(
            new RemoteRule(
                sonarQubeRule.getKey(),
                sonarQubeRule.getName(),
                sonarQubeRule.getHtmlDesc(),
                RuleSeverity.fromSonarLintSeverity(sonarQubeRule.getSeverity()),
                RuleType.fromSonarLintRuleType(sonarQubeRule.getType())));
      } catch (JsonProcessingException e) {
        throw new SonarHostException(
            "Malformed rule info response from SonarQube: " + e.getMessage());
      }
    }

    return ruleSet;
  }

  public Collection<RemoteIssue> getResolvedIssues(Set<String> relativeFilePaths) {
    if (projectKey.isEmpty()) {
      return null;
    }

    String componentKeysStr =
        relativeFilePaths.stream()
            .map(filePath -> projectKey + ":" + filePath)
            .collect(Collectors.joining(","));

    LOG.info("Getting resolved issues for component keys: {}", componentKeysStr);

    ConnectedList<SonarQubeIssue> resolvedIssues =
        new ConnectedList<>(
            api,
            "/api/issues/search?resolved=true&componentKeys=" + componentKeysStr,
            "issues",
            SonarQubeIssue.class);

    Set<RemoteIssue> remoteIssues = new HashSet<>();

    for (SonarQubeIssue sqIssue : resolvedIssues) {
      remoteIssues.add(
          new RemoteIssue(
              sqIssue.getServerIssueKey(),
              sqIssue.getRuleKey(),
              sqIssue.getLine(),
              sqIssue.getLineHash(),
              sqIssue.getTextRange(),
              sqIssue.getMessage(),
              RuleSeverity.fromSonarLintIssueSeverity(sqIssue.getSeverity()),
              RuleType.fromSonarLintRuleType(sqIssue.getType())));
    }

    return remoteIssues;
  }

  public Set<RemoteActiveRule> getActiveRules() throws SonarHostException {
    var profile = getQualityProfile();
    if (profile == null) return Collections.emptySet();

    var rootNode =
        api.getJson(
            "/api/rules/search?ps=500&activation=true&f=actives,templateKey&language="
                + languageKey
                + "&qprofile="
                + profile.getKey());
    if (rootNode == null) {
      throw new SonarHostException("Could not retrieve active rules from SonarQube");
    }

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

    var activeRules = new HashSet<RemoteActiveRule>();
    for (var ruleNode : rulesArray) {
      var ruleKey = ruleNode.get("key").asText();

      String templateRuleKey = null;
      var templateKeyNode = ruleNode.get("templateKey");
      if (templateKeyNode != null) {
        templateRuleKey = templateKeyNode.asText();
      }

      Map<String, String> thisRuleParams = Collections.emptyMap();
      if (paramsMap.containsKey(ruleKey)) {
        thisRuleParams = Map.copyOf(paramsMap.get(ruleKey));
      }

      activeRules.add(new RemoteActiveRule(ruleKey, languageKey, templateRuleKey, thisRuleParams));
    }

    return activeRules;
  }

  public Optional<Path> getPluginJar() throws SonarHostException {
    return Optional.ofNullable(api.getFile("/api/plugins/download?plugin=" + pluginKey));
  }

  public Optional<String> getPluginJarName() throws SonarHostException {
    var rootNode = api.getJson("/api/plugins/installed");
    if (rootNode == null) {
      throw new SonarHostException("Could not retrieve installed plugins from SonarQube");
    }

    String pluginJarName = "";

    var pluginsArray = rootNode.get("plugins");
    for (var plugin : pluginsArray) {
      var keyProp = plugin.get("key");
      var filenameProp = plugin.get("filename");
      if (keyProp != null && filenameProp != null && pluginKey.equals(keyProp.asText())) {
        pluginJarName = filenameProp.asText();
        break;
      }
    }

    if (pluginJarName.isEmpty()) {
      return Optional.empty();
    } else {
      return Optional.of(pluginJarName);
    }
  }
}
