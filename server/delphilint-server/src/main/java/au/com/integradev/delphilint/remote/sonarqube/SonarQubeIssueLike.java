/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
