package au.com.integradev.delphilint.sonarqube;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang3.NotImplementedException;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.plugin.commons.LoadedPlugins;
import org.sonarsource.sonarlint.core.rule.extractor.RulesDefinitionExtractor;

public class StandaloneSonar implements SonarServerConnection {
  private final LoadedPlugins loadedPlugins;

  public StandaloneSonar(LoadedPlugins loadedPlugins) {
    this.loadedPlugins = loadedPlugins;
  }

  @Override
  public Map<String, String> getRuleNamesByRuleKey() {
    return getRules().stream().collect(Collectors.toMap(RemoteRule::getKey, RemoteRule::getName));
  }

  private RuleType mapRuleType(org.sonarsource.sonarlint.core.commons.RuleType origType) {
    switch (origType) {
      case BUG:
        return RuleType.BUG;
      case VULNERABILITY:
        return RuleType.VULNERABILITY;
      case CODE_SMELL:
        return RuleType.CODE_SMELL;
      case SECURITY_HOTSPOT:
        return RuleType.SECURITY_HOTSPOT;
      default:
        throw new NotImplementedException("Unknown rule type");
    }
  }

  @Override
  public Set<RemoteRule> getRules() {
    var rulesExtractor = new RulesDefinitionExtractor();
    return rulesExtractor
        .extractRules(
            loadedPlugins.getPluginInstancesByKeys(), Set.of(Language.DELPHI), true, false)
        .stream()
        .map(
            ruleDef ->
                new RemoteRule(
                    ruleDef.getKey(),
                    ruleDef.getName(),
                    ruleDef.getHtmlDescription(),
                    RuleSeverity.MINOR,
                    mapRuleType(ruleDef.getType())))
        .collect(Collectors.toSet());
  }

  @Override
  public Collection<RemoteIssue> getResolvedIssues(Set<String> relativeFilePaths) {
    return Collections.emptySet();
  }

  @Override
  public Set<RemoteActiveRule> getActiveRules() {
    return Collections.emptySet();
  }
}
