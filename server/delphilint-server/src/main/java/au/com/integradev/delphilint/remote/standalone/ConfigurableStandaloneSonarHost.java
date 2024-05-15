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

import java.util.Set;
import org.sonarsource.sonarlint.core.plugin.commons.LoadedPlugins;
import org.sonarsource.sonarlint.core.rule.extractor.SonarLintRuleDefinition;

public class ConfigurableStandaloneSonarHost extends AbstractStandaloneSonarHost {
  private final Set<String> disabledRuleKeys;

  public ConfigurableStandaloneSonarHost(
      LoadedPlugins loadedPlugins, Set<String> disabledRuleKeys) {
    super(loadedPlugins);
    this.disabledRuleKeys = disabledRuleKeys;
  }

  @Override
  protected boolean isActiveRule(SonarLintRuleDefinition ruleDef) {
    return !disabledRuleKeys.contains(ruleDef.getKey());
  }

  @Override
  public String getName() {
    return String.format("Standalone (%d disabled rules)", disabledRuleKeys.size());
  }
}
