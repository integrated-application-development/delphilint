package au.com.integradev.delphilint;

import org.sonar.plugins.delphi.*;
import org.sonar.plugins.delphi.enviroment.DefaultEnvironmentVariableProvider;
import org.sonar.plugins.delphi.executor.DelphiMasterExecutor;
import org.sonar.plugins.delphi.executor.DelphiPmdExecutor;
import org.sonar.plugins.delphi.executor.DelphiSymbolTableExecutor;
import org.sonar.plugins.delphi.msbuild.DelphiProjectHelper;
import org.sonar.plugins.delphi.pmd.DelphiPmdConfiguration;
import org.sonar.plugins.delphi.pmd.profile.DelphiPmdRuleSetDefinitionProvider;
import org.sonar.plugins.delphi.pmd.violation.DelphiPmdViolationRecorder;

/**
 * Hello world!
 *
 */
public class App {
    public static void main( String[] args ) {
        SensorLintContext context = new SensorLintContext();

        DelphiProjectHelper helper = new DelphiProjectHelper(
            context.config(),
            context.fileSystem(),
            new DefaultEnvironmentVariableProvider()
        );

        DelphiMasterExecutor executor = new DelphiMasterExecutor(
            new DelphiSymbolTableExecutor(),
            new DelphiPmdExecutor(
                context,
                context.activeRules(),
                new DelphiPmdConfiguration(
                    context.fileSystem(),
                    context.config(),
                    new DelphiPmdRuleSetDefinitionProvider()
                ),
                new DelphiPmdViolationRecorder(
                    helper,
                    context.activeRules()
                )
            )
        );

        DelphiSensor sensor = new DelphiSensor(helper, executor);
        sensor.execute(context);
    }
}
