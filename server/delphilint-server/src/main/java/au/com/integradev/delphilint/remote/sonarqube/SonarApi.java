package au.com.integradev.delphilint.remote.sonarqube;

import au.com.integradev.delphilint.remote.SonarHostException;
import com.fasterxml.jackson.databind.JsonNode;
import java.nio.file.Path;

public interface SonarApi {
  public String getHostUrl();

  public JsonNode getJson(String url) throws SonarHostException;

  public Path getFile(String url) throws SonarHostException;

  public String getText(String url) throws SonarHostException;
}
