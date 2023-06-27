package au.com.integradev.delphilint.sonarqube;

public class RemoteRule {
  private final String key;
  private final String name;
  private final String htmlDesc;
  private final RuleSeverity severity;
  private final RuleType type;

  public RemoteRule(
      String key, String name, String htmlDesc, RuleSeverity severity, RuleType type) {
    this.key = key;
    this.name = name;
    this.htmlDesc = htmlDesc;
    this.severity = severity;
    this.type = type;
  }

  public String getKey() {
    return key;
  }

  public String getName() {
    return name;
  }

  public String getHtmlDesc() {
    return htmlDesc;
  }

  public RuleSeverity getSeverity() {
    return severity;
  }

  public RuleType getType() {
    return type;
  }
}
