package au.com.integradev.delphilint.server;

import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

public class SonarProjectProperties {
  private final Path projectPropertiesPath;

  public SonarProjectProperties(Path projectPropertiesPath) {
    this.projectPropertiesPath = projectPropertiesPath;
  }

  private static Properties fileToProperties(Path path) {
    Properties props = new Properties();
    try (InputStream in = new FileInputStream(path.toFile())) {
      props.load(in);
      for (String key : props.stringPropertyNames()) {
        props.setProperty(key, props.getProperty(key).trim());
      }
      return props;
    } catch (Exception e) {
      throw new IllegalStateException("Could not load properties file: " + path.toString(), e);
    }
  }

  public Map<String, String> toMap() {
    Properties properties = fileToProperties(projectPropertiesPath);
    return properties.entrySet().stream()
        .collect(
            Collectors.toMap(entry -> (String) entry.getKey(), entry -> (String) entry.getValue()));
  }
}
