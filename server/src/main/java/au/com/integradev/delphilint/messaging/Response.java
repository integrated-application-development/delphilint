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
package au.com.integradev.delphilint.messaging;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Response {
  @JsonProperty private Category category;
  @JsonProperty private Object data;

  public Response(Category category, Object data) {
    this.category = category;
    this.data = data;
  }

  public static Response uninitialized() {
    return new Response(Category.UNINITIALIZED, null);
  }

  public static Response initialized() {
    return new Response(Category.INITIALIZED, null);
  }

  public static Response unexpectedError(String message) {
    return new Response(Category.UNEXPECTED_ERROR, message);
  }

  public static Response analyzeResult(ResponseAnalyzeResult result) {
    return new Response(Category.ANALYZE_RESULT, result);
  }

  public static Response analyzeError(String message) {
    return new Response(Category.ANALYZE_ERROR, message);
  }

  public static Response pong(String data) {
    return new Response(Category.PONG, data);
  }

  public static Response invalidRequest(String message) {
    return new Response(Category.INVALID_REQUEST, message);
  }

  public Category getCategory() {
    return category;
  }

  public Object getData() {
    return data;
  }
}
