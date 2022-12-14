package au.com.integradev.delphilint;

import org.sonarsource.sonarlint.core.commons.log.ClientLogOutput;

public class DelphiLintLogOutput implements ClientLogOutput {
  @Override
  public void log(String s, Level level) {
    System.out.println(String.format("[%s] %s", level.toString(), s));
  }
}
