package au.com.integradev.delphilint.sonarqube;

import org.apache.commons.lang3.NotImplementedException;

public enum RuleType {
  CODE_SMELL,
  BUG,
  VULNERABILITY,
  SECURITY_HOTSPOT;

  public static RuleType fromSonarLintRuleType(
      org.sonarsource.sonarlint.core.commons.RuleType ruleType) {
    switch (ruleType) {
      case BUG:
        return RuleType.BUG;
      case CODE_SMELL:
        return RuleType.CODE_SMELL;
      case VULNERABILITY:
        return RuleType.VULNERABILITY;
      case SECURITY_HOTSPOT:
        return RuleType.SECURITY_HOTSPOT;
      default:
        throw new NotImplementedException("Unknown rule type");
    }
  }
}
