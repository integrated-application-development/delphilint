package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse.BodyHandler;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.sonarlint.core.analysis.api.ActiveRule;

public class SonarQubeConnection {
  private static final Logger LOG = Loggers.get(SonarQubeConnection.class);
  private final String hostUrl;
  private final String projectKey;
  private final String languageKey;
  private final HttpClient http;
  private final ObjectMapper jsonMapper;
  private final XmlMapper xmlMapper;

  public SonarQubeConnection(String hostUrl, String projectKey, String languageKey) {
    this.hostUrl = hostUrl;
    this.projectKey = projectKey;
    this.languageKey = languageKey;
    this.http = HttpClient.newHttpClient();
    this.jsonMapper = new ObjectMapper();
    this.xmlMapper = new XmlMapper();
  }

  private <T> T getResponse(String url, BodyHandler<Supplier<T>> handler) {
    var request = HttpRequest.newBuilder(URI.create(url)).build();

    LOG.info("Sending request: " + url);

    try {
      var response = http.send(request, handler);
      return response.body().get();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
    return null;
  }

  private JsonNode getJson(String url) {
    return getResponse(url, new JsonBodyHandler());
  }

  private <T> T getXml(String url, Class<T> clazz) {
    return getResponse(url, new XmlBodyHandler<>(clazz));
  }

  public QualityProfile getQualityProfile() {
    var rootNode =
        getJson(hostUrl + "/api/qualityprofiles/search?defaults=true&language=" + languageKey);

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
      }
    }

    return null;
  }

  public Map<String, String> getRuleNamesByRuleKey() {
    var profile = getQualityProfile();
    if (profile == null) return Collections.emptyMap();

    var rootNode =
        getJson(
            hostUrl
                + "/api/rules/search?ps=500&activation=true&f=name&language="
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

  public Set<ActiveRule> getActiveRules() {
    var profile = getQualityProfile();
    if (profile == null) return Collections.emptySet();

    var rootNode =
        getJson(
            hostUrl
                + "/api/rules/search?ps=500&activation=true&f=actives,templateKey&language="
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
