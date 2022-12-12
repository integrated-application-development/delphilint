package au.com.integradev.delphilint;

import java.util.ArrayList;
import java.util.Collection;
import org.jetbrains.annotations.Nullable;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.rule.ActiveRules;
import org.sonar.api.rule.RuleKey;

public class LintActiveRules implements ActiveRules {

  @Nullable
  @Override
  public ActiveRule find(RuleKey ruleKey) {
    return null;
  }

  @Override
  public Collection<ActiveRule> findAll() {
    return new ArrayList<>();
  }

  @Override
  public Collection<ActiveRule> findByRepository(String s) {
    return new ArrayList<>();
  }

  @Override
  public Collection<ActiveRule> findByLanguage(String s) {
    return new ArrayList<>();
  }

  @Nullable
  @Override
  public ActiveRule findByInternalKey(String s, String s1) {
    return null;
  }
}
