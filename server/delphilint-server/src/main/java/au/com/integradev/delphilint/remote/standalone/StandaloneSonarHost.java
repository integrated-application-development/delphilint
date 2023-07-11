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
package au.com.integradev.delphilint.remote.standalone;

import au.com.integradev.delphilint.remote.RemoteActiveRule;
import au.com.integradev.delphilint.remote.RemoteIssue;
import au.com.integradev.delphilint.remote.RemoteRule;
import au.com.integradev.delphilint.remote.RuleSeverity;
import au.com.integradev.delphilint.remote.RuleType;
import au.com.integradev.delphilint.remote.SonarHost;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
        throw new UncheckedIOException(e);
      }
    }

    public List<String> getActives() {
      return actives;
    }
  }

  public StandaloneSonarHost() {
    this.rules = Collections.emptySet();
    this.activeRules = Collections.emptySet();
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
                getClass()
                    .getResourceAsStream("/au/com/integradev/delphilint/standalone_rules.json"),
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

  public String getName() {
    return "";
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

  public Optional<Path> getPluginJar() {
    return Optional.empty();
  }

  public Optional<String> getPluginJarName() {
    return Optional.empty();
  }
}
