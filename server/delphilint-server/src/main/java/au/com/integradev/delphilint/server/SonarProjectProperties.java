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
