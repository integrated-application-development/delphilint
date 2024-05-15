/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
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

import au.com.integradev.delphilint.remote.CleanCodeAttribute;
import au.com.integradev.delphilint.remote.ImpactSeverity;
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

public abstract class AbstractStandaloneSonarHost implements SonarHost {
  private Set<RemoteActiveRule> activeRules;
  private final List<SonarLintRuleDefinition> ruleDefinitions;
  private final Set<RemoteRule> rules;

  protected AbstractStandaloneSonarHost() {
    ruleDefinitions = Collections.emptyList();
    rules = Collections.emptySet();
    activeRules = Collections.emptySet();
  }

  protected AbstractStandaloneSonarHost(LoadedPlugins loadedPlugins) {
    var rulesExtractor = new RulesDefinitionExtractor();
    ruleDefinitions =
        rulesExtractor.extractRules(
            loadedPlugins.getPluginInstancesByKeys(), Set.of(Language.DELPHI), true, false);

    rules =
        ruleDefinitions.stream()
            .map(
                ruleDef ->
                    new RemoteRule(
                        ruleDef.getKey(),
                        ruleDef.getName(),
                        ruleDef.getHtmlDescription(),
                        RuleSeverity.fromSonarLintIssueSeverity(ruleDef.getDefaultSeverity()),
                        RuleType.fromSonarLintRuleType(ruleDef.getType()),
                        ruleDef.getCleanCodeAttribute().isPresent()
                            ? new RemoteCleanCode(
                                CleanCodeAttribute.fromSonarLintCleanCodeAttribute(
                                    ruleDef.getCleanCodeAttribute().get()),
                                ruleDef.getDefaultImpacts().entrySet().stream()
                                    .collect(
                                        Collectors.toMap(
                                            e ->
                                                SoftwareQuality.fromSonarLintSoftwareQuality(
                                                    e.getKey()),
                                            e ->
                                                ImpactSeverity.fromSonarLintImpactSeverity(
                                                    e.getValue()))))
                            : null))
            .collect(Collectors.toSet());
  }

  private Set<RemoteActiveRule> getOrComputeActiveRules() {
    if (activeRules == null) {
      activeRules =
          ruleDefinitions.stream()
              .filter(this::isActiveRule)
              .map(
                  ruleDef ->
                      new RemoteActiveRule(
                          ruleDef.getKey(),
                          ruleDef.getLanguage().getLanguageKey(),
                          null,
                          ruleDef.getDefaultParams()))
              .collect(Collectors.toSet());
    }

    return activeRules;
  }

  protected abstract boolean isActiveRule(SonarLintRuleDefinition ruleDef);

  public SonarCharacteristics getCharacteristics() {
    return SonarCharacteristics.latest();
  }

  public abstract String getName();

  @Override
  public Map<String, String> getRuleNamesByRuleKey() {
    return getRules().stream().collect(Collectors.toMap(RemoteRule::getKey, RemoteRule::getName));
  }

  @Override
  public Set<RemoteRule> getRules() {
    return rules;
  }

  @Override
  public Set<String> getTestFilePaths() {
    return Collections.emptySet();
  }

  @Override
  public Collection<RemoteIssue> getResolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths) {
    return Collections.emptySet();
  }

  @Override
  public Collection<RemoteIssue> getUnresolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths) {
    return Collections.emptySet();
  }

  @Override
  public Set<RemoteActiveRule> getActiveRules() {
    return getOrComputeActiveRules();
  }

  @Override
  public Optional<Path> getPluginJar(String pluginKey) {
    return Optional.empty();
  }

  public Set<RemotePlugin> getDelphiPlugins() {
    return Collections.emptySet();
  }
}
