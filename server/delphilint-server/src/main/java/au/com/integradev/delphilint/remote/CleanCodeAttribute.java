package au.com.integradev.delphilint.remote;

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
}
