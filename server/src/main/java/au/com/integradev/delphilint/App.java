package au.com.integradev.delphilint;

import java.io.Console;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisConfiguration;
import org.sonarsource.sonarlint.core.analysis.api.AnalysisEngineConfiguration;
import org.sonarsource.sonarlint.core.analysis.container.global.GlobalAnalysisContainer;
import org.sonarsource.sonarlint.core.analysis.container.module.ModuleContainer;
import org.sonarsource.sonarlint.core.commons.progress.ProgressMonitor;
import org.sonarsource.sonarlint.core.plugin.commons.PluginInstancesRepository;

/**
 * Hello world!
 *
 */
public class App {
    public static void main( String[] args ) {
        var analysisGlobalConfig = AnalysisEngineConfiguration.builder()
            .setWorkDir(Path.of("{PATH REMOVED}"))
            .build();

        var pluginConfig =
            new PluginInstancesRepository.Configuration(
                Set.of(
                    Path.of("{PATH REMOVED}")),
                Collections.emptySet(),
                Optional.empty());

        var loadedPlugins = new PluginInstancesRepository(pluginConfig);

        var globalContainer = new GlobalAnalysisContainer(analysisGlobalConfig, loadedPlugins);
        globalContainer.startComponents();

        System.out.println("Global container started");

        var moduleContainer = new ModuleContainer(globalContainer, true);
        moduleContainer.startComponents();

        System.out.println("Module container started");

        var configuration = AnalysisConfiguration.builder()
            .build();

        System.out.println("Starting analysis");

        moduleContainer.analyze(configuration, issue -> System.out.println(issue.toString()), new ProgressMonitor(null));

        System.out.println("Analysis complete");

        moduleContainer.stopComponents();

        System.out.println("Module container stopped");

        globalContainer.stopComponents();
        System.out.println("Global container stopped");
    }
}
