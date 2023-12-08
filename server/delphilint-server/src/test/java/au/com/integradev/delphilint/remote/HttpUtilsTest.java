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
  void noParamMapStringIsEmpty() {
    assertEquals("", HttpUtils.buildParamString(Collections.emptyMap()));
  }

  @Test
  void singleParamMapString() {
    assertEquals("?myKey=myValue", HttpUtils.buildParamString(Map.of("myKey", "myValue")));
  }

  @Test
  void multiParamMapString() {
    Map<String, String> params = new LinkedHashMap<>();
    params.put("myKeyA", "myValueA");
    params.put("myKeyB", "myValueB");
    params.put("myKeyC", "myValueC");

    assertEquals(
        "?myKeyA=myValueA&myKeyB=myValueB&myKeyC=myValueC", HttpUtils.buildParamString(params));
  }

  @Test
  void noParamListStringIsEmpty() {
    assertEquals("", HttpUtils.buildParamString(Collections.emptyMap()));
  }

  @Test
  void singleParamListString() {
    assertEquals("?myKey=myValue", HttpUtils.buildParamString(List.of("myKey=myValue")));
  }

  @Test
  void multiParamListString() {
    List<String> params = new ArrayList<>();
    params.add("myKeyA=myValueA");
    params.add("myKeyB=myValueB");
    params.add("myKeyC=myValueC");

    assertEquals(
        "?myKeyA=myValueA&myKeyB=myValueB&myKeyC=myValueC", HttpUtils.buildParamString(params));
  }

  @Test
  void htmlEncodesParamList() {
    List<String> params = new ArrayList<>();
    params.add("my=Ke yA=my=Value A");
    params.add("my&KeyB=myValue&B");

    assertEquals(
        "?my=Ke+yA%3Dmy%3DValue+A&my%26KeyB=myValue%26B", HttpUtils.buildParamString(params));
  }

  @Test
  void htmlEncodesParamMap() {
    Map<String, String> params = new LinkedHashMap<>();
    params.put("my=Ke yA", "my=Value A");
    params.put("my&KeyB", "myValue&B");

    assertEquals(
        "?my%3DKe+yA=my%3DValue+A&my%26KeyB=myValue%26B", HttpUtils.buildParamString(params));
  }
}
