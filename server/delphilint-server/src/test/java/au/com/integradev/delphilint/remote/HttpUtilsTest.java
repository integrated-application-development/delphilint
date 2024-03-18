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
package au.com.integradev.delphilint.remote;

import static org.junit.jupiter.api.Assertions.assertEquals;

import au.com.integradev.delphilint.remote.sonarqube.HttpUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class HttpUtilsTest {
  @Test
  void testNoParamMapStringIsEmpty() {
    assertEquals("", HttpUtils.buildParamString(Collections.emptyMap()));
  }

  @Test
  void testSingleParamMapString() {
    assertEquals("?myKey=myValue", HttpUtils.buildParamString(Map.of("myKey", "myValue")));
  }

  @Test
  void testMultiParamMapString() {
    Map<String, String> params = new LinkedHashMap<>();
    params.put("myKeyA", "myValueA");
    params.put("myKeyB", "myValueB");
    params.put("myKeyC", "myValueC");

    assertEquals(
        "?myKeyA=myValueA&myKeyB=myValueB&myKeyC=myValueC", HttpUtils.buildParamString(params));
  }

  @Test
  void testNoParamListStringIsEmpty() {
    assertEquals("", HttpUtils.buildParamString(Collections.emptyMap()));
  }

  @Test
  void testSingleParamListString() {
    assertEquals("?myKey=myValue", HttpUtils.buildParamString(List.of("myKey=myValue")));
  }

  @Test
  void testMultiParamListString() {
    List<String> params = new ArrayList<>();
    params.add("myKeyA=myValueA");
    params.add("myKeyB=myValueB");
    params.add("myKeyC=myValueC");

    assertEquals(
        "?myKeyA=myValueA&myKeyB=myValueB&myKeyC=myValueC", HttpUtils.buildParamString(params));
  }

  @Test
  void testHtmlEncodesParamList() {
    List<String> params = new ArrayList<>();
    params.add("my=Ke yA=my=Value A");
    params.add("my&KeyB=myValue&B");

    assertEquals(
        "?my=Ke+yA%3Dmy%3DValue+A&my%26KeyB=myValue%26B", HttpUtils.buildParamString(params));
  }

  @Test
  void testHtmlEncodesParamMap() {
    Map<String, String> params = new LinkedHashMap<>();
    params.put("my=Ke yA", "my=Value A");
    params.put("my&KeyB", "myValue&B");

    assertEquals(
        "?my%3DKe+yA=my%3DValue+A&my%26KeyB=myValue%26B", HttpUtils.buildParamString(params));
  }
}
