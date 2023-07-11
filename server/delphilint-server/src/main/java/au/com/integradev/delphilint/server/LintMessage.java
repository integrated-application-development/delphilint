/*
 * DelphiLint Server
 * Copyright (C) 2023 Integrated Application Development
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
package au.com.integradev.delphilint.server;

import au.com.integradev.delphilint.server.message.ResponseAnalyzeResult;
import au.com.integradev.delphilint.server.message.ResponseRuleRetrieveResult;
import com.fasterxml.jackson.annotation.JsonProperty;

public class LintMessage {
  @JsonProperty private MessageCategory category;
  @JsonProperty private Object data;

  public LintMessage(MessageCategory category, Object data) {
    this.category = category;
    this.data = data;
  }

  public static LintMessage initialized() {
    return new LintMessage(MessageCategory.INITIALIZED, null);
  }

  public static LintMessage initializeError(String message) {
    return new LintMessage(MessageCategory.INITIALIZE_ERROR, message);
  }

  public static LintMessage unexpectedError(String message) {
    return new LintMessage(MessageCategory.UNEXPECTED_ERROR, message);
  }

  public static LintMessage analyzeResult(ResponseAnalyzeResult result) {
    return new LintMessage(MessageCategory.ANALYZE_RESULT, result);
  }

  public static LintMessage analyzeError(String message) {
    return new LintMessage(MessageCategory.ANALYZE_ERROR, message);
  }

  public static LintMessage ruleRetrieveResult(ResponseRuleRetrieveResult result) {
    return new LintMessage(MessageCategory.RULE_RETRIEVE_RESULT, result);
  }

  public static LintMessage ruleRetrieveError(String message) {
    return new LintMessage(MessageCategory.RULE_RETRIEVE_ERROR, message);
  }

  public static LintMessage pong(String data) {
    return new LintMessage(MessageCategory.PONG, data);
  }

  public static LintMessage invalidRequest(String message) {
    return new LintMessage(MessageCategory.INVALID_REQUEST, message);
  }

  public MessageCategory getCategory() {
    return category;
  }

  public Object getData() {
    return data;
  }
}
