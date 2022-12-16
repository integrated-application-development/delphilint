package au.com.integradev.delphilint;

import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisEngineConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.Issue;
import org.sonarsource.sonarlint.core.analysis.container.global.GlobalAnalysisContainer;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.commons.progress.ClientProgressMonitor;
import org.sonarsource.sonarlint.core.commons.progress.ProgressMonitor;
import org.sonarsource.sonarlint.core.plugin.commons.PluginInstancesRepository;

public class DelphiAnalysisEngine implements AutoCloseable {
  private static final Logger LOG = Loggers.get(DelphiAnalysisEngine.class);

  // TODO: Get SonarDelphi jar from the SonarQube server or store locally
  private static final Path DELPHI_PLUGIN_JAR =
      Path.of(
          "{PATH REMOVED}");

  private final GlobalAnalysisContainer globalContainer;
  private final SonarQubeConnection connection;

  public DelphiAnalysisEngine(DelphiConfiguration delphiConfig, SonarQubeConnection connection) {
    this.connection = connection;

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
                Set.of(DELPHI_PLUGIN_JAR), engineConfig.getEnabledLanguages(), Optional.empty()));

    globalContainer = new GlobalAnalysisContainer(engineConfig, pluginInstances);
    globalContainer.startComponents();
  }

  public Set<Issue> analyze(
      Path baseDir, Set<Path> inputFiles, ClientProgressMonitor progressMonitor) {

    var configBuilder =
        AnalysisConfiguration.builder()
            .setBaseDir(baseDir)
            .addInputFiles(
                inputFiles.stream()
                    .map(DelphiLintInputFile::new)
                    .collect(Collectors.toUnmodifiableList()));

    if (connection != null) {
      configBuilder.addActiveRules(connection.getActiveRules());
    }

    var config = configBuilder.build();

    var moduleContainer =
        globalContainer.getModuleRegistry().createTransientContainer(config.inputFiles());

    var issues = new HashSet<Issue>();
    moduleContainer.analyze(config, issues::add, new ProgressMonitor(progressMonitor));
    return issues;
  }

  @Override
  public void close() {
    globalContainer.stopComponents();
  }
}
