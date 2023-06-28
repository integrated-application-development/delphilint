package au.com.integradev.delphilint.remote.standalone;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import au.com.integradev.delphilint.remote.RemoteActiveRule;
import au.com.integradev.delphilint.remote.RemoteIssue;
import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;
import au.com.integradev.delphilint.remote.SonarHost;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.plugin.commons.LoadedPlugins;
import org.sonarsource.sonarlint.core.rule.extractor.RulesDefinitionExtractor;
import org.sonarsource.sonarlint.core.rule.extractor.SonarLintRuleDefinition;

public class StandaloneSonarHost implements SonarHost {
  private final Set<RemoteRule> rules;
  private final Set<RemoteActiveRule> activeRules;

  private static class StandaloneRulesData {
    @JsonProperty private List<String> actives;

    public static StandaloneRulesData fromStream(InputStreamReader reader) {
      var mapper = new ObjectMapper();
      try {
        return mapper.readValue(reader, StandaloneRulesData.class);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    public List<String> getActives() {
      return actives;
    }
  }

  public StandaloneSonarHost(LoadedPlugins loadedPlugins) {
    var rulesExtractor = new RulesDefinitionExtractor();
    List<SonarLintRuleDefinition> ruleDefs =
        rulesExtractor.extractRules(
            loadedPlugins.getPluginInstancesByKeys(), Set.of(Language.DELPHI), true, false);

    this.rules =
        ruleDefs.stream()
            .map(
                ruleDef ->
                    new RemoteRule(
                        ruleDef.getKey(),
                        ruleDef.getName(),
                        ruleDef.getHtmlDescription(),
                        RuleSeverity.fromSonarLintIssueSeverity(ruleDef.getDefaultSeverity()),
                        RuleType.fromSonarLintRuleType(ruleDef.getType())))
            .collect(Collectors.toSet());

    var standaloneRulesData =
        StandaloneRulesData.fromStream(
            new InputStreamReader(
                getClass().getResourceAsStream("/au/com/integradev/delphilint/standalone_rules.json"),
                StandardCharsets.UTF_8));

    this.activeRules =
        ruleDefs.stream()
            .filter(ruleDef -> standaloneRulesData.getActives().contains(ruleDef.getKey()))
            .map(
                ruleDef ->
                    new RemoteActiveRule(
                        ruleDef.getKey(),
                        ruleDef.getLanguage().getLanguageKey(),
                        null,
                        ruleDef.getDefaultParams()))
            .collect(Collectors.toSet());
  }

  @Override
  public Map<String, String> getRuleNamesByRuleKey() {
    return getRules().stream().collect(Collectors.toMap(RemoteRule::getKey, RemoteRule::getName));
  }

  @Override
  public Set<RemoteRule> getRules() {
    return rules;
  }

  @Override
  public Collection<RemoteIssue> getResolvedIssues(Set<String> relativeFilePaths) {
    return Collections.emptySet();
  }

  @Override
  public Set<RemoteActiveRule> getActiveRules() {
    return activeRules;
  }
}
