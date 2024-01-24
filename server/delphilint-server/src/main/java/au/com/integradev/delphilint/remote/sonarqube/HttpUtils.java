package au.com.integradev.delphilint.remote.sonarqube;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;

public final class HttpUtils {
  private HttpUtils() {
    // Utility class
  }

  public static String buildParamString(Map<String, String> params) {
    if (params.isEmpty()) {
      return "";
    }

    return "?"
        + params.entrySet().stream()
            .map(
                entry ->
                    URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8)
                        + "="
                        + URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8))
            .collect(Collectors.joining("&"));
  }

  public static String buildParamString(List<String> params) {
    if (params.isEmpty()) {
      return "";
    }

    List<String> encodedParams =
        params.stream()
            .map(
                param -> {
                  String[] split = param.split("=", 2);
                  String result = URLEncoder.encode(split[0], StandardCharsets.UTF_8);

                  if (split.length > 1) {
                    return result + '=' + URLEncoder.encode(split[1], StandardCharsets.UTF_8);
                  } else {
                    return result;
                  }
                })
            .collect(Collectors.toList());

    return "?" + StringUtils.join(encodedParams, "&");
  }
}
