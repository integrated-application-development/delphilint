package au.com.integradev.delphilint.remote.sonarqube;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class HttpUtils {
  private HttpUtils() {
    // Utility class
  }

  public static String buildParamString(Map<String, String> params) {
    if (params.isEmpty()) {
      return "";
    }

    return "?"
        + params.entrySet().stream()
            .map(entry -> entry.getKey() + "=" + entry.getValue())
            .collect(Collectors.joining("&"));
  }

  public static String buildParamString(List<String> params) {
    if (params.isEmpty()) {
      return "";
    }

    return "?" + String.join("&", params);
  }
}
