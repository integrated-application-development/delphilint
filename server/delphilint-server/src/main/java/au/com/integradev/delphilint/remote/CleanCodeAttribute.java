package au.com.integradev.delphilint.remote;

import org.apache.commons.lang3.NotImplementedException;

public enum CleanCodeAttribute {
  FORMATTED(CleanCodeAttributeCategory.CONSISTENT),
  CONVENTIONAL(CleanCodeAttributeCategory.CONSISTENT),
  IDENTIFIABLE(CleanCodeAttributeCategory.CONSISTENT),
  CLEAR(CleanCodeAttributeCategory.INTENTIONAL),
  LOGICAL(CleanCodeAttributeCategory.INTENTIONAL),
  COMPLETE(CleanCodeAttributeCategory.INTENTIONAL),
  EFFICIENT(CleanCodeAttributeCategory.INTENTIONAL),
  FOCUSED(CleanCodeAttributeCategory.ADAPTABLE),
  DISTINCT(CleanCodeAttributeCategory.ADAPTABLE),
  MODULAR(CleanCodeAttributeCategory.ADAPTABLE),
  TESTED(CleanCodeAttributeCategory.ADAPTABLE),
  LAWFUL(CleanCodeAttributeCategory.RESPONSIBLE),
  TRUSTWORTHY(CleanCodeAttributeCategory.RESPONSIBLE),
  RESPECTFUL(CleanCodeAttributeCategory.RESPONSIBLE);

  private final CleanCodeAttributeCategory category;

  CleanCodeAttribute(CleanCodeAttributeCategory category) {
    this.category = category;
  }

  public CleanCodeAttributeCategory getCategory() {
    return category;
  }

  public static CleanCodeAttribute fromSonarLintCleanCodeAttribute(
      org.sonarsource.sonarlint.core.commons.CleanCodeAttribute attr) {
    switch (attr) {
      case CONVENTIONAL:
        return CONVENTIONAL;
      case FORMATTED:
        return FORMATTED;
      case IDENTIFIABLE:
        return IDENTIFIABLE;
      case CLEAR:
        return CLEAR;
      case COMPLETE:
        return COMPLETE;
      case EFFICIENT:
        return EFFICIENT;
      case LOGICAL:
        return LOGICAL;
      case DISTINCT:
        return DISTINCT;
      case FOCUSED:
        return FOCUSED;
      case MODULAR:
        return MODULAR;
      case TESTED:
        return TESTED;
      case LAWFUL:
        return LAWFUL;
      case RESPECTFUL:
        return RESPECTFUL;
      case TRUSTWORTHY:
        return TRUSTWORTHY;
      default:
        throw new NotImplementedException("Unknown clean code attribute");
    }
  }
}
