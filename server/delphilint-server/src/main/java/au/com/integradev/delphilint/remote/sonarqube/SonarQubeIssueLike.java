package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.analysis.TextRange;
import au.com.integradev.delphilint.remote.CleanCodeAttribute;
import au.com.integradev.delphilint.remote.IssueLikeType;
import java.util.List;
import org.sonarsource.sonarlint.core.commons.IssueSeverity;
import org.sonarsource.sonarlint.core.commons.RuleType;

public interface SonarQubeIssueLike {
  String getRuleKey();

  TextRange getTextRange();

  String getMessage();

  String getAssignee();

  String getCreationDate();

  String getStatus();

  String getResolution();

  List<SonarQubeQualityImpact> getImpacts();

  CleanCodeAttribute getCleanCodeAttribute();

  IssueSeverity getSeverity();

  RuleType getIssueType();

  IssueLikeType getLikeType();
}
