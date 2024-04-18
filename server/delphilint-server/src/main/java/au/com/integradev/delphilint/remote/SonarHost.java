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
package au.com.integradev.delphilint.remote;

import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public interface SonarHost {
  /**
   * Retrieves salient supported features of the host.
   *
   * @return the characteristics of the host.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  SonarCharacteristics getCharacteristics() throws SonarHostException;

  /**
   * Retrieves a mapping of rule keys to the corresponding rule names.
   *
   * @return a map of rule keys (the key) to rule names (the value).
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Map<String, String> getRuleNamesByRuleKey() throws SonarHostException;

  /**
   * Retrieves metadata on all rules.
   *
   * @return a set containing rule metadata, including descriptions, severities, and categorization.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Set<RemoteRule> getRules() throws SonarHostException;

  /**
   * Retrieves all test files registered on the host for the given project.
   *
   * @return a set of all test file paths, relative to the project base directory.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Set<String> getTestFilePaths() throws SonarHostException;

  /**
   * Retrieves all resolved issues for a set of given file paths.
   *
   * @param relativeFilePaths a set of all main file paths to retrieve resolved issues for, relative
   *     to the project base directory.
   * @param testRelativeFilePaths a set of all test file paths to retrieve resolved issues for,
   *     relative to the project base directory.
   * @return a collection of all retrieved issue metadata.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Collection<RemoteIssue> getResolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths)
      throws SonarHostException;

  /**
   * Retrieves all unresolved issues for a set of given file paths.
   *
   * @param relativeFilePaths a set of all main file paths to retrieve unresolved issues for,
   *     relative to the project base directory.
   * @param testRelativeFilePaths a set of all test file paths to retrieve unresolved issues for,
   *     relative to the project base directory.
   * @return a collection of all retrieved issue metadata.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Collection<RemoteIssue> getUnresolvedIssues(
      Collection<String> relativeFilePaths, Collection<String> testRelativeFilePaths)
      throws SonarHostException;

  /**
   * Retrieves all active rules registered for the current project.
   *
   * @return a set of all active rules.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Set<RemoteActiveRule> getActiveRules() throws SonarHostException;

  /**
   * Downloads a plugin jar by a given key.
   *
   * @param pluginKey the key of the plugin to download.
   * @return a path to a temporary file containing the plugin jar contents.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Optional<Path> getPluginJar(String pluginKey) throws SonarHostException;

  /**
   * Retrieves metadata for all Delphi-related plugins on the host.
   *
   * @return a set of retrieved plugin metadata.
   * @throws SonarHostException if an error occurs during communication with the host.
   */
  Set<RemotePlugin> getDelphiPlugins() throws SonarHostException;

  /**
   * Gets a descriptive name for the host.
   *
   * @return the name of the host.
   */
  String getName();
}
