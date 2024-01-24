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

import au.com.integradev.delphilint.remote.RemoteActiveRule;
import au.com.integradev.delphilint.remote.SonarHost;
import au.com.integradev.delphilint.remote.SonarHostException;
import au.com.integradev.delphilint.remote.SonarServerUtils;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.sonarsource.sonarlint.core.analysis.api.ActiveRule;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisEngineConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.analysis.container.global.GlobalAnalysisContainer;
import org.sonarsource.sonarlint.core.analysis.container.module.ModuleContainer;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.commons.progress.ClientProgressMonitor;
import org.sonarsource.sonarlint.core.commons.progress.ProgressMonitor;
import org.sonarsource.sonarlint.core.plugin.commons.LoadedPlugins;
import org.sonarsource.sonarlint.core.plugin.commons.PluginsLoadResult;
import org.sonarsource.sonarlint.core.plugin.commons.PluginsLoader;
import org.springframework.util.StringUtils;

public class AnalysisOrchestrator implements AutoCloseable {
  private static final Logger LOG = LogManager.getLogger(AnalysisOrchestrator.class);
  private final GlobalAnalysisContainer globalContainer;
  private final LoadedPlugins loadedPlugins;

  public AnalysisOrchestrator(EngineStartupConfiguration startupConfig) {
    var engineConfig =
        AnalysisEngineConfiguration.builder()
            .setWorkDir(Path.of(System.getProperty("java.io.tmpdir")))
            .setExtraProperties(startupConfig.getBaseProperties())
            .build();

    PluginsLoader.Configuration pluginsConfig =
        new PluginsLoader.Configuration(startupConfig.getPluginPaths(), Set.of(Language.DELPHI));
    PluginsLoadResult pluginsLoadResult = new PluginsLoader().load(pluginsConfig);
    loadedPlugins = pluginsLoadResult.getLoadedPlugins();

    globalContainer = new GlobalAnalysisContainer(engineConfig, loadedPlugins);
    globalContainer.startComponents();
    LOG.info("Analysis engine started");
  }

  private static AnalysisConfiguration buildConfiguration(
      Path baseDir, Set<Path> inputFiles, SonarHost connection, Map<String, String> properties)
      throws SonarHostException {
    var configBuilder =
        AnalysisConfiguration.builder()
            .setBaseDir(baseDir)
            .putAllExtraProperties(properties)
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

    LOG.info("Added {} extra properties", properties.size());

    Set<ActiveRule> activeRules =
        connection.getActiveRules().stream()
            .filter(rule -> !RuleUtils.isIncompatible(rule.getRuleKey()))
            .map(RemoteActiveRule::toSonarLintActiveRule)
            .collect(Collectors.toSet());
    configBuilder.addActiveRules(activeRules);
    LOG.info("Added {} active rules", activeRules.size());

    return configBuilder.build();
  }

  private static Set<String> getTestFiles(Set<String> inputFiles, String[] testDirs) {
    Set<String> testFiles = new HashSet<>();

    for (String testBase : testDirs) {
      for (String inputFile : inputFiles) {
        if (StringUtils.startsWithIgnoreCase(inputFile, testBase)) {
          testFiles.add(inputFile);
        }
      }
    }

    return testFiles;
  }

  public Set<DelphiIssue> runAnalysis(
      Path baseDir,
      Set<Path> inputFiles,
      ClientProgressMonitor progressMonitor,
      SonarHost host,
      Map<String, String> properties)
      throws SonarHostException {
    LOG.info("About to analyze {} files", inputFiles.size());
    AnalysisConfiguration config = buildConfiguration(baseDir, inputFiles, host, properties);

    Set<Issue> issues = new HashSet<>();

    ModuleContainer moduleContainer =
        globalContainer.getModuleRegistry().createTransientContainer(config.inputFiles());
    try {
      LOG.info("Starting analysis");
      moduleContainer.analyze(config, issues::add, new ProgressMonitor(progressMonitor));
    } finally {
      moduleContainer.stopComponents();
    }

    LOG.info("Analysis finished");

    Set<String> fileRelativePaths = new HashSet<>();
    config
        .inputFiles()
        .iterator()
        .forEachRemaining(file -> fileRelativePaths.add(file.relativePath()));

    Set<String> testFileRelativePaths;
    if (properties.containsKey("sonar.tests")) {
      testFileRelativePaths =
          getTestFiles(fileRelativePaths, properties.get("sonar.tests").split(",", -1));
    } else if (!properties.isEmpty()) {
      testFileRelativePaths = Collections.emptySet();
    } else {
      testFileRelativePaths = null;
    }

    return SonarServerUtils.postProcessIssues(
        fileRelativePaths, testFileRelativePaths, issues, host);
  }

  public LoadedPlugins getLoadedPlugins() {
    return loadedPlugins;
  }

  @Override
  public void close() {
    globalContainer.stopComponents();
    LOG.info("Analysis engine closed");
  }
}
