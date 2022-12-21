package au.com.integradev.delphilint;

import au.com.integradev.delphilint.analysis.DelphiLintLogOutput;
import java.io.IOException;
import java.net.InetSocketAddress;
import org.sonarsource.sonarlint.core.commons.log.SonarLintLogger;

public class App {
  public static void main(String[] args) throws IOException {
    var logOutput = new DelphiLintLogOutput();
    SonarLintLogger.setTarget(logOutput);

    var server = new LintServer(new InetSocketAddress(14000));
    server.run();
  }
}
