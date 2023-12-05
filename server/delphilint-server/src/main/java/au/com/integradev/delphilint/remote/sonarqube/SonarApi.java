package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.SonarHostException;
import com.fasterxml.jackson.databind.JsonNode;
import java.nio.file.Path;
import java.util.Map;

public interface SonarApi {
  public String getHostUrl();

  public JsonNode getJson(String url) throws SonarHostException;

  public JsonNode getJson(String url, Map<String, String> params) throws SonarHostException;

  public Path getFile(String url) throws SonarHostException;

  public Path getFile(String url, Map<String, String> params) throws SonarHostException;

  public String getText(String url) throws SonarHostException;

  public String getText(String url, Map<String, String> params) throws SonarHostException;
}
