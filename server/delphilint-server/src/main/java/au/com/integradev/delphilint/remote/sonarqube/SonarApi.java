package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.SonarHostException;
import com.fasterxml.jackson.databind.JsonNode;
import java.nio.file.Path;
import java.util.Map;

public interface SonarApi {
  String getHostUrl();

  JsonNode getJson(String url) throws SonarHostException;

  JsonNode getJson(String url, Map<String, String> params) throws SonarHostException;

  Path getFile(String url) throws SonarHostException;

  Path getFile(String url, Map<String, String> params) throws SonarHostException;

  String getText(String url) throws SonarHostException;

  String getText(String url, Map<String, String> params) throws SonarHostException;
}
