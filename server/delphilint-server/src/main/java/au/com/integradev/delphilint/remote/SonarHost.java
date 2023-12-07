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
package au.com.integradev.delphilint.remote;

import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public interface SonarHost {
  SonarCharacteristics getCharacteristics() throws SonarHostException;

  Map<String, String> getRuleNamesByRuleKey() throws SonarHostException;

  Set<RemoteRule> getRules() throws SonarHostException;

  Set<String> getTestFilePaths() throws SonarHostException;

  Collection<RemoteIssue> getResolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths)
      throws SonarHostException;

  Collection<RemoteIssue> getUnresolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths)
      throws SonarHostException;

  Set<RemoteActiveRule> getActiveRules() throws SonarHostException;

  Optional<Path> getPluginJar(String pluginKey) throws SonarHostException;

  Set<RemotePlugin> getDelphiPlugins() throws SonarHostException;

  String getName();
}
