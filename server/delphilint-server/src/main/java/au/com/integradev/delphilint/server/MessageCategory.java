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

import au.com.integradev.delphilint.server.message.RequestAnalyze;
import au.com.integradev.delphilint.server.message.RequestInitialize;
import au.com.integradev.delphilint.server.message.RequestRuleRetrieve;
import au.com.integradev.delphilint.server.message.ResponseAnalyzeResult;
import au.com.integradev.delphilint.server.message.ResponseRuleRetrieveResult;

public enum MessageCategory {
  PING(1, String.class),
  PONG(5, String.class),
  QUIT(15),
  INITIALIZE(20, RequestInitialize.class),
  INITIALIZED(25),
  UNINITIALIZED(26),
  ANALYZE(30, RequestAnalyze.class),
  ANALYZE_RESULT(35, ResponseAnalyzeResult.class),
  ANALYZE_ERROR(36, String.class),
  RULE_RETRIEVE(40, RequestRuleRetrieve.class),
  RULE_RETRIEVE_RESULT(45, ResponseRuleRetrieveResult.class),
  RULE_RETRIEVE_ERROR(46, String.class),
  INVALID_REQUEST(241, String.class),
  UNEXPECTED_ERROR(242, String.class);

  private final byte code;
  private final Class<?> dataClass;

  MessageCategory(int code) {
    this.code = (byte) code;
    this.dataClass = null;
  }

  MessageCategory(int code, Class<?> clazz) {
    this.code = (byte) code;
    this.dataClass = clazz;
  }

  public static MessageCategory fromCode(int code) {
    for (var category : values()) {
      if (category.getCode() == code) {
        return category;
      }
    }

    return null;
  }

  public byte getCode() {
    return code;
  }

  public Class<?> getDataClass() {
    return dataClass;
  }
}
