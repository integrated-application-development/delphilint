package au.com.integradev.delphilint.analysis;

import java.util.List;

public class RuleUtils {
  private RuleUtils() {
    // Utility class
  }

  private static final List<String> INCOMPATIBLE_RULES =
      List.of(
          "delph:UnusedMethodsRule",
          "delph:UnusedConstantsRule",
          "delph:UnusedGlobalVariablesRule",
          "delph:UnusedPropertiesRule",
          "delph:UnusedTypesRule");

  public static boolean isIncompatible(String ruleKey) {
    return INCOMPATIBLE_RULES.contains(ruleKey);
  }
}
