package au.com.integradev.delphilint;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.sonarlint.core.analysis.api.ActiveRule;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisEngineConfiguration;
import org.sonarsource.sonarlint.core.analysis.container.global.GlobalAnalysisContainer;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.commons.log.SonarLintLogger;
import org.sonarsource.sonarlint.core.commons.progress.ProgressMonitor;
import org.sonarsource.sonarlint.core.plugin.commons.PluginInstancesRepository;

public class App {
  private static Logger LOG = Loggers.get(App.class);

  public static void main(String[] args) {
    var totalTimeStart = System.currentTimeMillis();
    var logOutput = new DelphiLintLogOutput();
    SonarLintLogger.setTarget(logOutput);

    var analysisGlobalConfig =
        AnalysisEngineConfiguration.builder()
            .setWorkDir(Path.of("{PATH REMOVED}"))
            .addEnabledLanguage(Language.DELPHI)
            .setExtraProperties(
                Map.of(
                    "sonar.delphi.bds.path", "{PATH REMOVED} Files (x86)/Embarcadero/Studio/22.0",
                    "sonar.delphi.compiler.version", "VER350"))
            .build();

    var pluginConfig =
        new PluginInstancesRepository.Configuration(
            Set.of(
                Path.of(
                    "{PATH REMOVED}")),
            analysisGlobalConfig.getEnabledLanguages(),
            Optional.empty());

    var loadedPlugins = new PluginInstancesRepository(pluginConfig);

    var globalContainer = new GlobalAnalysisContainer(analysisGlobalConfig, loadedPlugins);
    globalContainer.startComponents();

    var configuration =
        AnalysisConfiguration.builder()
            .setBaseDir(Path.of("{PATH REMOVED}"))
            .addInputFiles(
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()),
                new DelphiLintInputFile(
                    Path.of(
                        "{PATH REMOVED}"),
                    Charset.defaultCharset()))
            .addActiveRules(
                new ActiveRule("delph:EmptyMethodRule", Language.DELPHI.getLanguageKey()))
            .build();

    var moduleContainer =
        globalContainer.getModuleRegistry().createTransientContainer(configuration.inputFiles());
    var analysisTimeStart = System.currentTimeMillis();
    moduleContainer.analyze(
        configuration, issue -> System.out.println(issue.toString()), new ProgressMonitor(null));
    LOG.info("Analysis took " + (System.currentTimeMillis() - analysisTimeStart) + " ms.");

    globalContainer.stopComponents();
    LOG.info("Total run took " + (System.currentTimeMillis() - totalTimeStart) + " ms.");
  }
}
