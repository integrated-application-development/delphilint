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

import au.com.integradev.delphilint.remote.ImpactSeverity;
import au.com.integradev.delphilint.remote.IssueLikeType;
import au.com.integradev.delphilint.remote.IssueStatus;
import au.com.integradev.delphilint.remote.RemoteActiveRule;
import au.com.integradev.delphilint.remote.RemoteCleanCode;
import au.com.integradev.delphilint.remote.RemoteIssue;
import au.com.integradev.delphilint.remote.RemotePlugin;
import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;
import au.com.integradev.delphilint.remote.SoftwareQuality;
import au.com.integradev.delphilint.remote.SonarCharacteristics;
import au.com.integradev.delphilint.remote.SonarHost;
import au.com.integradev.delphilint.remote.SonarHostException;
import au.com.integradev.delphilint.remote.UncheckedSonarHostException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

public class SonarQubeHost implements SonarHost {
  private static final Logger LOG = LogManager.getLogger(SonarQubeHost.class);

  private static final String URL_SERVER_VERSION = "/api/server/version";
  private static final String URL_QUALITY_PROFILES_SEARCH = "/api/qualityprofiles/search";
  private static final String URL_RULES_SEARCH = "/api/rules/search";
  private static final String URL_PLUGINS_DOWNLOAD = "/api/plugins/download";
  private static final String URL_PLUGINS_INSTALLED = "/api/plugins/installed";
  private static final String URL_COMPONENTS_TREE = "/api/components/tree";

  private static final String PARAM_PAGE_SIZE = "ps";
  private static final String PARAM_LANGUAGE = "language";
  private static final String PARAM_LANGUAGES = "languages";
  private static final String PARAM_QUALITY_PROFILE = "qprofile";
  private static final String PARAM_FIELDS = "f";
  private static final String PARAM_PROJECT = "project";
  private static final String PARAM_ACTIVATION = "activation";

  private static final String RULES_ARRAY_NAME = "rules";

  private final String projectKey;
  private final String languageKey;
  private final String pluginKey;
  private final String pluginKeyDiscriminator;
  private final ObjectMapper jsonMapper;
  private final SonarApi api;
  private SonarCharacteristics characteristics;

  public SonarQubeHost(
      SonarApi api,
      String projectKey,
      String languageKey,
      String pluginKey,
      String pluginKeyDiscriminator) {
    this.api = api;
    this.projectKey = projectKey;
    this.languageKey = languageKey;
    this.pluginKey = pluginKey;
    this.pluginKeyDiscriminator = pluginKeyDiscriminator;
    this.jsonMapper = new ObjectMapper();
  }

  public String getName() {
    return "SonarQube instance at " + api.getHostUrl();
  }

  private SonarCharacteristics calculateCharacteristics() throws SonarHostException {
    String versionStr = api.getText(URL_SERVER_VERSION);
    var version = new Version(versionStr);
    return new SonarCharacteristics(version);
  }

  public SonarCharacteristics getCharacteristics() throws SonarHostException {
    if (this.characteristics == null) {
      this.characteristics = calculateCharacteristics();
    }
    return this.characteristics;
  }

  public SonarQubeQualityProfile getQualityProfile() throws SonarHostException {
    Map<String, String> params = new LinkedHashMap<>();
    params.put(PARAM_LANGUAGE, languageKey);
    if (projectKey.isEmpty()) {
      params.put("defaults", "true");
    } else {
      params.put(PARAM_PROJECT, projectKey);
    }

    var rootNode = api.getJson(URL_QUALITY_PROFILES_SEARCH, params);

    if (rootNode == null) {
      throw new SonarHostException("Could not retrieve quality profile from SonarQube");
    }

    var profilesArray = rootNode.get("profiles");
    if (profilesArray == null || profilesArray.isEmpty()) {
      String errorMessage = getErrorMessageFromJson(rootNode);
      if (errorMessage != null) {
        throw new SonarHostException(
            "SonarQube error when retrieving quality profile: " + errorMessage);
      } else {
        throw new SonarHostException("No quality profile found for project " + projectKey);
      }
    }

    var profile = profilesArray.get(0);
    try {
      return jsonMapper.treeToValue(profile, SonarQubeQualityProfile.class);
    } catch (JsonProcessingException e) {
      LOG.error(e);
      throw new SonarHostException("Problem parsing quality profile JSON: " + e.getMessage());
    }
  }

  @Nullable
  private static String getErrorMessageFromJson(JsonNode rootNode) {
    var errorsArray = rootNode.get("errors");
    if (errorsArray != null && !errorsArray.isEmpty() && errorsArray.get(0).has("msg")) {
      return errorsArray.get(0).get("msg").asText();
    } else {
      return null;
    }
  }

  public Map<String, String> getRuleNamesByRuleKey() throws SonarHostException {
    var profile = getQualityProfile();
    if (profile == null) {
      return Collections.emptyMap();
    }

    Map<String, String> params = new LinkedHashMap<>();
    params.put(PARAM_PAGE_SIZE, "500");
    params.put(PARAM_ACTIVATION, "true");
    params.put(PARAM_FIELDS, "name");
    params.put(PARAM_LANGUAGE, languageKey);
    params.put(PARAM_QUALITY_PROFILE, profile.getKey());

    var rootNode = api.getJson(URL_RULES_SEARCH, params);
    if (rootNode == null) {
      throw new SonarHostException(
          "Could not retrieve rule names from SonarQube for quality profile " + profile.getName());
    }

    var rulesArray = rootNode.get(RULES_ARRAY_NAME);
    var ruleMap = new HashMap<String, String>();

    for (var rule : rulesArray) {
      ruleMap.put(rule.get("key").asText(), rule.get("name").asText());
    }

    return ruleMap;
  }

  public Set<RemoteRule> getRules() throws SonarHostException {
    SonarQubeQualityProfile profile = getQualityProfile();

    Map<String, String> params = new LinkedHashMap<>();
    params.put(PARAM_PAGE_SIZE, "500");
    params.put(PARAM_ACTIVATION, "true");
    params.put(PARAM_LANGUAGES, languageKey);
    params.put(PARAM_QUALITY_PROFILE, profile.getKey());

    if (getCharacteristics().usesCodeAttributes()) {
      params.put(PARAM_FIELDS, "name,htmlDesc,severity,cleanCodeAttribute");
    } else {
      params.put(PARAM_FIELDS, "name,htmlDesc,severity");
    }

    var rootNode = api.getJson(URL_RULES_SEARCH, params);
    if (rootNode == null) {
      throw new SonarHostException(
          "No JSON response from SonarQube for rule information retrieval");
    }

    var rulesArray = rootNode.get(RULES_ARRAY_NAME);
    Set<RemoteRule> ruleSet = new HashSet<>();

    for (var rule : rulesArray) {
      try {
        SonarQubeRule sonarQubeRule = jsonMapper.treeToValue(rule, SonarQubeRule.class);

        RemoteCleanCode cleanCode = null;

        if (sonarQubeRule.getCleanCodeAttribute() != null) {
          Map<SoftwareQuality, ImpactSeverity> impacts =
              sonarQubeRule.getImpacts().stream()
                  .collect(
                      Collectors.toMap(
                          SonarQubeQualityImpact::getSoftwareQuality,
                          SonarQubeQualityImpact::getSeverity));
          cleanCode = new RemoteCleanCode(sonarQubeRule.getCleanCodeAttribute(), impacts);
        }

        ruleSet.add(
            new RemoteRule(
                sonarQubeRule.getKey(),
                sonarQubeRule.getName(),
                sonarQubeRule.getHtmlDesc(),
                RuleSeverity.fromSonarLintIssueSeverity(sonarQubeRule.getSeverity()),
                RuleType.fromSonarLintRuleType(sonarQubeRule.getType()),
                cleanCode));
      } catch (JsonProcessingException e) {
        LOG.error(e);
        throw new SonarHostException(
            "Malformed rule info response from SonarQube: " + e.getMessage());
      }
    }

    return ruleSet;
  }

  private static List<String> joinStringsWithLimit(
      Collection<String> values, UnaryOperator<String> mapper, int maxChars) {
    List<String> strings = new ArrayList<>();
    var stringBuilder = new StringBuilder();

    for (String value : values) {
      stringBuilder.append(mapper.apply(value));
      if (stringBuilder.length() < maxChars) {
        stringBuilder.append(",");
      } else {
        strings.add(stringBuilder.toString());
        stringBuilder.setLength(0);
      }
    }

    if (stringBuilder.length() > 0) {
      strings.add(stringBuilder.toString());
    }

    return strings;
  }

  private static RemoteIssue sqIssueToRemote(SonarQubeIssueLike sqIssue) {
    var issueBuilder =
        new RemoteIssue.Builder()
            .withRuleKey(sqIssue.getRuleKey())
            .withRange(sqIssue.getTextRange())
            .withMessage(sqIssue.getMessage())
            .withType(RuleType.fromSonarLintRuleType(sqIssue.getIssueType()))
            .withSeverity(RuleSeverity.fromSonarLintIssueSeverity(sqIssue.getSeverity()))
            .withServerMetadata(sqIssue.getAssignee(), sqIssue.getCreationDate())
            .withStatus(IssueStatus.fromSonarQubeIssueStatus(sqIssue.getStatus()))
            .withLikeType(sqIssue.getLikeType())
            .withResolution(sqIssue.getResolution());

    if (sqIssue.getCleanCodeAttribute() != null) {
      Map<SoftwareQuality, ImpactSeverity> impacts =
          sqIssue.getImpacts().stream()
              .collect(
                  Collectors.toMap(
                      SonarQubeQualityImpact::getSoftwareQuality,
                      SonarQubeQualityImpact::getSeverity));

      issueBuilder.withCleanCode(sqIssue.getCleanCodeAttribute(), impacts);
    }

    if (sqIssue instanceof SonarQubeIssue) {
      issueBuilder.withHash(((SonarQubeIssue) sqIssue).getLineHash());
    }

    return issueBuilder.build();
  }

  public Set<String> getTestFilePaths() throws SonarHostException {
    Map<String, String> params = new LinkedHashMap<>();
    params.put(PARAM_PAGE_SIZE, "500");
    params.put("component", projectKey);
    params.put("qualifiers", "UTS");

    try {
      ConnectedList<SonarQubeComponent> componentsList =
          new ConnectedList<>(
              api,
              URL_COMPONENTS_TREE + HttpUtils.buildParamString(params),
              "components",
              SonarQubeComponent.class);

      return StreamSupport.stream(componentsList.spliterator(), false)
          .map(SonarQubeComponent::getPath)
          .collect(Collectors.toSet());
    } catch (UncheckedSonarHostException e) {
      throw (SonarHostException) e.getCause();
    }
  }

  private Set<RemoteIssue> getIssuesAndHotspots(
      Collection<String> relativeFilePaths,
      Collection<String> testRelativeFilePaths,
      Collection<String> issueParams,
      Collection<String> hotspotParams)
      throws SonarHostException {
    Set<RemoteIssue> issues = getIssuesAndHotspots(relativeFilePaths, issueParams, hotspotParams);
    if (!testRelativeFilePaths.isEmpty()) {
      issues.addAll(getIssuesAndHotspots(testRelativeFilePaths, issueParams, hotspotParams));
    }

    return issues;
  }

  private Set<RemoteIssue> getIssuesAndHotspots(
      Collection<String> relativeFilePaths,
      Collection<String> issueParams,
      Collection<String> hotspotParams)
      throws SonarHostException {
    if (projectKey.isEmpty()) {
      return Collections.emptySet();
    }

    Set<RemoteIssue> remoteIssues = new HashSet<>();

    List<String> componentKeyBatches =
        joinStringsWithLimit(relativeFilePaths, filePath -> projectKey + ":" + filePath, 1500);

    List<String> dynIssueParams = new ArrayList<>(issueParams);
    dynIssueParams.add(0, "<component keys>");

    for (String componentKeyBatch : componentKeyBatches) {
      LOG.info("Getting issues for component keys: {}", componentKeyBatch);
      dynIssueParams.set(0, "componentKeys=" + componentKeyBatch);

      try {
        ConnectedList<SonarQubeIssue> serverIssues =
            new ConnectedList<>(
                api,
                "/api/issues/search" + HttpUtils.buildParamString(dynIssueParams),
                "issues",
                SonarQubeIssue.class);

        for (SonarQubeIssue sqIssue : serverIssues) {
          remoteIssues.add(sqIssueToRemote(sqIssue));
        }
      } catch (UncheckedSonarHostException e) {
        throw (SonarHostException) e.getCause();
      }
    }

    List<String> filePathBatches = joinStringsWithLimit(relativeFilePaths, s -> s, 1500);
    List<String> dynHotspotParams = new ArrayList<>(hotspotParams);
    dynHotspotParams.add(0, "<files>");
    dynHotspotParams.add(1, "projectKey=" + projectKey);

    for (String filePathBatch : filePathBatches) {
      LOG.info("Getting hotspots for files: {}", filePathBatch);
      dynHotspotParams.set(0, "files=" + filePathBatch);

      try {
        ConnectedList<SonarQubeHotspot> resolvedIssues =
            new ConnectedList<>(
                api,
                "/api/hotspots/search" + HttpUtils.buildParamString(dynHotspotParams),
                "hotspots",
                SonarQubeHotspot.class);

        for (SonarQubeHotspot sqHotspot : resolvedIssues) {
          remoteIssues.add(sqIssueToRemote(sqHotspot));
        }
      } catch (UncheckedSonarHostException e) {
        throw (SonarHostException) e.getCause();
      }
    }

    return remoteIssues;
  }

  public Collection<RemoteIssue> getResolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths)
      throws SonarHostException {
    if (projectKey.isEmpty()) {
      return Collections.emptySet();
    }

    return getIssuesAndHotspots(
            relativeFilePaths,
            testRelativeFilePaths,
            List.of("resolved=true"),
            List.of("status=REVIEWED"))
        .stream()
        // Acknowledged hotspots should not suppress issues
        .filter(
            issue ->
                issue.getLikeType() != IssueLikeType.SECURITY_HOTSPOT
                    || !"ACKNOWLEDGED".equals(issue.getResolution()))
        .collect(Collectors.toSet());
  }

  public Collection<RemoteIssue> getUnresolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths)
      throws SonarHostException {
    if (projectKey.isEmpty()) {
      return Collections.emptySet();
    }

    return getIssuesAndHotspots(
            relativeFilePaths,
            testRelativeFilePaths,
            List.of("resolved=false"),
            Collections.emptyList())
        .stream()
        .filter(
            issue ->
                issue.getLikeType() != IssueLikeType.SECURITY_HOTSPOT
                    || (issue.getStatus() == IssueStatus.TO_REVIEW
                        || "ACKNOWLEDGED".equals(issue.getResolution())))
        .collect(Collectors.toSet());
  }

  public Set<RemoteActiveRule> getActiveRules() throws SonarHostException {
    var profile = getQualityProfile();
    if (profile == null) {
      return Collections.emptySet();
    }

    Map<String, String> params = new LinkedHashMap<>();
    params.put(PARAM_PAGE_SIZE, "500");
    params.put(PARAM_ACTIVATION, "true");
    params.put(PARAM_FIELDS, "actives,templateKey");
    params.put(PARAM_LANGUAGE, languageKey);
    params.put(PARAM_QUALITY_PROFILE, profile.getKey());

    var rootNode = api.getJson(URL_RULES_SEARCH, params);
    if (rootNode == null) {
      throw new SonarHostException("Could not retrieve active rules from SonarQube");
    }

    var rulesArray = rootNode.get(RULES_ARRAY_NAME);

    var activesArray = rootNode.get("actives");

    var paramsMap = new HashMap<String, Map<String, String>>();

    activesArray
        .fieldNames()
        .forEachRemaining(
            activeKey -> {
              var activeNode = activesArray.get(activeKey);
              var paramsArray = activeNode.get(0).get("params");

              if (paramsArray != null && !paramsArray.isEmpty()) {
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

  public Optional<Path> getPluginJar(String pluginKey) throws SonarHostException {
    return Optional.ofNullable(api.getFile(URL_PLUGINS_DOWNLOAD, Map.of("plugin", pluginKey)));
  }

  public Set<RemotePlugin> getDelphiPlugins() throws SonarHostException {
    var rootNode = api.getJson(URL_PLUGINS_INSTALLED);
    if (rootNode == null) {
      throw new SonarHostException("Could not retrieve installed plugins from SonarQube");
    }

    Set<RemotePlugin> plugins = new HashSet<>();

    var pluginsArray = rootNode.get("plugins");
    for (var plugin : pluginsArray) {
      var keyProp = plugin.get("key");
      var filenameProp = plugin.get("filename");

      var isDelphiPlugin = false;

      if (getCharacteristics().supportsPluginRequiredForLanguages()) {
        var requiredForLanguagesProp = plugin.get("requiredForLanguages");
        isDelphiPlugin =
            requiredForLanguagesProp != null
                && StreamSupport.stream(requiredForLanguagesProp.spliterator(), false)
                    .anyMatch(el -> el.asText().equals(languageKey));
      }

      if (!isDelphiPlugin && getCharacteristics().requiresPluginRequiredForLanguagesHeuristic()) {
        // SonarQube 10.3 and below have no way of indicating whether a plugin is tied
        // to a specific language, so we have to guess using a heuristic.
        isDelphiPlugin =
            keyProp != null
                && filenameProp != null
                && StringUtils.containsIgnoreCase(keyProp.asText(), pluginKeyDiscriminator);
      }

      if (isDelphiPlugin) {
        plugins.add(
            new RemotePlugin(
                keyProp.asText(), filenameProp.asText(), pluginKey.equals(keyProp.asText())));
      }
    }

    return plugins;
  }
}
