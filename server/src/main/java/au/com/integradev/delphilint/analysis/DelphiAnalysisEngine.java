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
package au.com.integradev.delphilint.analysis;

import au.com.integradev.delphilint.analysis.TrackableWrappers.ClientTrackable;
import au.com.integradev.delphilint.sonarqube.ConnectedList;
import au.com.integradev.delphilint.sonarqube.ServerIssue;
import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
import au.com.integradev.delphilint.sonarqube.SonarQubeUtils;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collectors;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.sonarlint.core.analysis.api.ActiveRule;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisEngineConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.analysis.container.global.GlobalAnalysisContainer;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.commons.progress.ClientProgressMonitor;
import org.sonarsource.sonarlint.core.commons.progress.ProgressMonitor;
import org.sonarsource.sonarlint.core.issuetracking.Tracker;
import org.sonarsource.sonarlint.core.plugin.commons.PluginInstancesRepository;

public class DelphiAnalysisEngine implements AutoCloseable {
  private static final Logger LOG = Loggers.get(DelphiAnalysisEngine.class);
  private final GlobalAnalysisContainer globalContainer;

  public DelphiAnalysisEngine(DelphiConfiguration delphiConfig) {
    var engineConfig =
        AnalysisEngineConfiguration.builder()
            .setWorkDir(Path.of(System.getProperty("java.io.tmpdir")))
            .addEnabledLanguage(Language.DELPHI)
            .setExtraProperties(
                Map.of(
                    "sonar.delphi.bds.path", delphiConfig.getBdsPath(),
                    "sonar.delphi.compiler.version", delphiConfig.getCompilerVersion()))
            .build();

    var pluginInstances =
        new PluginInstancesRepository(
            new PluginInstancesRepository.Configuration(
                Set.of(delphiConfig.getSonarDelphiJarPath()),
                engineConfig.getEnabledLanguages(),
                Optional.empty()));

    globalContainer = new GlobalAnalysisContainer(engineConfig, pluginInstances);
    globalContainer.startComponents();
  }

  private AnalysisConfiguration buildConfiguration(
      Path baseDir, Set<Path> inputFiles, SonarQubeConnection connection) {
    var configBuilder =
        AnalysisConfiguration.builder()
            .setBaseDir(baseDir)
            .addInputFiles(
                inputFiles.stream()
                    .map(
                        possiblyAbsolutePath -> {
                          if (possiblyAbsolutePath.isAbsolute()) {
                            return baseDir.relativize(possiblyAbsolutePath);
                          } else {
                            return possiblyAbsolutePath;
                          }
                        })
                    .map(relativePath -> new DelphiLintInputFile(baseDir, relativePath))
                    .collect(Collectors.toUnmodifiableList()));

    // TODO: Have a local set of rules
    if (connection != null) {
      Set<ActiveRule> activeRules =
          connection.getActiveRules().stream()
              .filter(rule -> !RuleUtils.isIncompatible(rule.getRuleKey()))
              .collect(Collectors.toSet());
      configBuilder.addActiveRules(activeRules);
      LOG.info("Added " + activeRules.size() + " active rules");
    }

    return configBuilder.build();
  }

  public Set<Issue> analyze(
      Path baseDir,
      Set<Path> inputFiles,
      ClientProgressMonitor progressMonitor,
      SonarQubeConnection connection) {

    AnalysisConfiguration config = buildConfiguration(baseDir, inputFiles, connection);

    var moduleContainer =
        globalContainer.getModuleRegistry().createTransientContainer(config.inputFiles());
    Set<Issue> issues = new HashSet<>();

    moduleContainer.analyze(config, issues::add, new ProgressMonitor(progressMonitor));

    if (connection != null) {
      issues = postProcessIssues(issues, connection);
    }

    return issues;
  }

  private Set<Issue> postProcessIssues(Set<Issue> issues, SonarQubeConnection connection) {
    Queue<ClientTrackable> clientTrackables =
        SonarQubeUtils.populateIssueMessages(connection, issues).stream()
            .map(TrackableWrappers.ClientTrackable::new)
            .collect(Collectors.toCollection(LinkedList::new));
    Set<TrackableWrappers.ServerTrackable> serverTrackables = new HashSet<>();

    ConnectedList<ServerIssue> resolvedIssues = connection.getResolvedIssues();
    for (ServerIssue resolvedIssue : resolvedIssues) {
      serverTrackables.add(new TrackableWrappers.ServerTrackable(resolvedIssue));
    }

    Tracker<TrackableWrappers.ClientTrackable, TrackableWrappers.ServerTrackable> tracker =
        new Tracker<>();
    var tracking = tracker.track(() -> clientTrackables, () -> serverTrackables);

    Set<Issue> returnIssues = new HashSet<>();
    tracking
        .getUnmatchedRaws()
        .iterator()
        .forEachRemaining(trackable -> returnIssues.add(trackable.getClientObject()));
    return returnIssues;
  }

  @Override
  public void close() {
    globalContainer.stopComponents();
  }
}
