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

import au.com.integradev.delphilint.analysis.DelphiAnalysisEngine;
import au.com.integradev.delphilint.analysis.EngineStartupConfiguration;
import au.com.integradev.delphilint.analysis.SonarDelphiUtils;
import au.com.integradev.delphilint.remote.SonarHost;
import au.com.integradev.delphilint.remote.SonarHostConnectException;
import au.com.integradev.delphilint.remote.SonarHostException;
import au.com.integradev.delphilint.remote.SonarHostUnauthorizedException;
import au.com.integradev.delphilint.remote.UncheckedSonarHostException;
import au.com.integradev.delphilint.remote.sonarqube.HttpSonarApi;
import au.com.integradev.delphilint.remote.sonarqube.SonarQubeHost;
import au.com.integradev.delphilint.remote.standalone.StandaloneSonarHost;
import au.com.integradev.delphilint.server.message.RequestAnalyze;
import au.com.integradev.delphilint.server.message.RequestInitialize;
import au.com.integradev.delphilint.server.message.RequestRuleRetrieve;
import au.com.integradev.delphilint.server.message.ResponseAnalyzeResult;
import au.com.integradev.delphilint.server.message.ResponseRuleRetrieveResult;
import au.com.integradev.delphilint.server.message.data.RuleData;
import au.com.integradev.delphilint.server.plugin.CachingPluginDownloader;
import au.com.integradev.delphilint.server.plugin.DownloadedPlugin;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.sonarsource.sonarlint.core.commons.Language;
import org.sonarsource.sonarlint.core.commons.log.SonarLintLogger;

public class LintServer {
  private static final Logger LOG = LogManager.getLogger(LintServer.class);
  private final ServerSocket serverSocket;
  private DelphiAnalysisEngine engine;
  private final CachingPluginDownloader pluginDownloader;
  private Set<DownloadedPlugin> pluginGroup;
  private final ObjectMapper mapper;
  private boolean running;

  public LintServer(Path pluginsPath) throws IOException {
    this(pluginsPath, 0);
  }

  public LintServer(Path pluginsPath, int port) throws IOException {
    engine = null;
    serverSocket = new ServerSocket(port);
    running = false;
    mapper = new ObjectMapper();
    pluginDownloader = new CachingPluginDownloader(pluginsPath);
    pluginGroup = null;

    LOG.info("Server started on port {}", serverSocket.getLocalPort());
  }

  public void run() throws IOException {
    LOG.info("Awaiting socket connection");

    Socket clientSocket = serverSocket.accept();

    LOG.info("Socket connected");

    var out = clientSocket.getOutputStream();
    var in = clientSocket.getInputStream();

    running = true;
    while (running) {
      LOG.debug("Awaiting next message");
      readStream(in, out);
    }

    LOG.info("Terminating lint server");

    out.close();
    in.close();
    clientSocket.close();
  }

  public int getPort() {
    return serverSocket.getLocalPort();
  }

  private void writeStream(OutputStream out, int id, LintMessage response) {
    LOG.info("Sending {}", response.getCategory());

    String dataString;
    try {
      dataString = mapper.writeValueAsString(response.getData());
    } catch (JsonProcessingException e) {
      LOG.error("Unexpected error while converting outgoing message data to a JSON string", e);
      writeStream(out, id, LintMessage.unexpectedError(e.getMessage()));
      return;
    }

    var dataBytes = dataString.getBytes(StandardCharsets.UTF_8);

    try {
      out.write(
          ByteBuffer.allocate(9)
              .put(response.getCategory().getCode())
              .putInt(id)
              .putInt(dataBytes.length)
              .array());
      out.write(dataBytes);
    } catch (IOException e) {
      LOG.error("Unexpected IO exception while writing message data to stream", e);
      throw new UncheckedIOException(e);
    }
  }

  private void readStream(InputStream in, OutputStream out) {
    MessageCategory category;
    int id;
    int length;
    String dataString;
    try {
      category = MessageCategory.fromCode(in.read());
      id = ByteBuffer.wrap(in.readNBytes(4)).getInt();
      length = ByteBuffer.wrap(in.readNBytes(4)).getInt();
      dataString = new String(in.readNBytes(length), StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    LOG.info("Received {}", category);

    Consumer<LintMessage> sendMessage = (response -> writeStream(out, id, response));

    if (category == null) {
      LOG.warn("Received message with an unrecognised category");
      sendMessage.accept(LintMessage.invalidRequest("Unrecognised category"));
      return;
    }

    Object data = null;

    if (category.getDataClass() != null) {
      if (length == 0) {
        LOG.warn("Received message of type {}, but no data was supplied", category);
        sendMessage.accept(LintMessage.invalidRequest("No data supplied"));
        return;
      }

      try {
        data = mapper.readValue(dataString, category.getDataClass());
      } catch (JsonProcessingException e) {
        LOG.warn(
            "Received message of type {} with data in an incorrect format: {}",
            category,
            e.getMessage());
        sendMessage.accept(LintMessage.invalidRequest("Data is in an incorrect format"));
        return;
      }
    }

    try {
      processRequest(new LintMessage(category, data), sendMessage);
    } catch (Exception e) {
      LOG.warn("Unexpected error during message processing", e);
      sendMessage.accept(LintMessage.unexpectedError(e.getMessage()));
    }
  }

  private void processRequest(LintMessage message, Consumer<LintMessage> sendMessage) {
    switch (message.getCategory()) {
      case INITIALIZE:
        handleInitialize((RequestInitialize) message.getData(), sendMessage);
        break;
      case ANALYZE:
        handleAnalyze((RequestAnalyze) message.getData(), sendMessage);
        break;
      case RULE_RETRIEVE:
        handleRuleRetrieve((RequestRuleRetrieve) message.getData(), sendMessage);
        break;
      case QUIT:
        LOG.info("Quit received, shutting down.");
        running = false;
        break;
      case PING:
        sendMessage.accept(LintMessage.pong((String) message.getData()));
        break;
      default:
        LOG.warn("TCP request has unhandled category {}", message.getCategory());
        sendMessage.accept(LintMessage.invalidRequest("Unhandled request category"));
    }
  }

  private void handleAnalyze(RequestAnalyze requestAnalyze, Consumer<LintMessage> sendMessage) {
    if (engine == null) {
      sendMessage.accept(
          LintMessage.unexpectedError("Please initialize before attempting to analyze"));
      return;
    }

    SonarHost sonarHost =
        getSonarHost(
            requestAnalyze.getSonarHostUrl(),
            requestAnalyze.getProjectKey(),
            requestAnalyze.getApiToken());

    Map<String, String> properties = Collections.emptyMap();
    if (!requestAnalyze.getProjectProperties().isEmpty()) {
      Path projectPropertiesPath = Path.of(requestAnalyze.getProjectProperties());
      if (Files.exists(projectPropertiesPath)) {
        properties = new SonarProjectProperties(projectPropertiesPath).toMap();
      }
    }

    try {
      var logOutput = new SonarLintLogOutput();
      SonarLintLogger.setTarget(logOutput);

      var issues =
          engine.analyze(
              requestAnalyze.getBaseDir(),
              requestAnalyze.getInputFiles(),
              null,
              sonarHost,
              properties);

      if (logOutput.containsError()) {
        LOG.error("Error logged during SonarDelphi analysis: {}", logOutput.getError());
        String friendlyError = SonarDelphiUtils.convertSonarDelphiError(logOutput.getError());
        sendMessage.accept(LintMessage.analyzeError(friendlyError));
      } else {
        ResponseAnalyzeResult result = ResponseAnalyzeResult.fromIssueSet(issues);
        result.convertPathsToAbsolute(requestAnalyze.getBaseDir());
        sendMessage.accept(LintMessage.analyzeResult(result));
      }
    } catch (SonarHostUnauthorizedException e) {
      LOG.warn("API returned an unauthorized response", e);
      sendMessage.accept(
          LintMessage.analyzeError(
              "Authorization is required to access the configured SonarQube instance. Please"
                  + " provide an appropriate authorization token"));
    } catch (SonarHostConnectException e) {
      LOG.warn("API could not be accessed", e);
      sendMessage.accept(
          LintMessage.analyzeError(
              "Could not connect to the configured SonarQube instance. Please confirm that the URL"
                  + " is correct and the instance is running"));
    } catch (SonarHostException | UncheckedSonarHostException e) {
      LOG.warn("API returned an unexpected response", e);
      sendMessage.accept(
          LintMessage.analyzeError(
              "The configured SonarQube instance could not be accessed: " + e.getMessage()));
    } catch (Exception e) {
      LOG.error("Unknown error during analysis", e);
      e.printStackTrace();
      sendMessage.accept(
          LintMessage.analyzeError(
              "Unknown error during analysis: "
                  + e.getMessage()
                  + " ("
                  + e.getClass().getSimpleName()
                  + ")"));
    } finally {
      SonarLintLogger.setTarget(null);
    }

    // I'd rather not have to call this, but the server gets unacceptably large without it
    System.gc();
  }

  private void handleInitialize(
      RequestInitialize requestInitialize, Consumer<LintMessage> sendMessage) {
    try {
      SonarHost host =
          getSonarHost(requestInitialize.getSonarHostUrl(), "", requestInitialize.getApiToken());
      Set<DownloadedPlugin> desiredPluginGroup =
          pluginDownloader.getRemotePluginJars(host).orElse(null);

      if (!Objects.equals(pluginGroup, desiredPluginGroup) || engine == null) {
        pluginGroup = desiredPluginGroup;
        LOG.info("Starting analysis engine with new plugins");

        var delphiConfig =
            new EngineStartupConfiguration(
                requestInitialize.getBdsPath(),
                requestInitialize.getCompilerVersion(),
                pluginGroup == null
                    ? Set.of(Path.of(requestInitialize.getDefaultSonarDelphiJarPath()))
                    : pluginGroup.stream()
                        .map(DownloadedPlugin::getPath)
                        .collect(Collectors.toSet()));

        engine = new DelphiAnalysisEngine(delphiConfig);
      }
      sendMessage.accept(LintMessage.initialized());
    } catch (SonarHostUnauthorizedException e) {
      LOG.warn("API returned an unauthorized response", e);
      sendMessage.accept(
          LintMessage.initializeError(
              "Authorization is required to access the configured SonarQube instance. Please"
                  + " provide an appropriate authorization token."));
    } catch (SonarHostConnectException e) {
      LOG.warn("API could not be accessed", e);
      sendMessage.accept(
          LintMessage.initializeError(
              "Could not connect to the configured SonarQube instance. Please confirm that the URL"
                  + " is correct and the instance is running."));
    } catch (SonarHostException | UncheckedSonarHostException e) {
      LOG.warn("API returned an unexpected response", e);
      sendMessage.accept(
          LintMessage.initializeError(
              "The configured SonarQube instance could not be accessed: " + e));
    } catch (Exception e) {
      LOG.error("Unknown error during analysis", e);
      e.printStackTrace();
      sendMessage.accept(
          LintMessage.initializeError(
              "Unknown error during analysis: "
                  + e.getMessage()
                  + " ("
                  + e.getClass().getSimpleName()
                  + ")"));
    }
  }

  private void handleRuleRetrieve(
      RequestRuleRetrieve requestRuleRetrieve, Consumer<LintMessage> sendMessage) {
    try {
      SonarHost host =
          getSonarHost(
              requestRuleRetrieve.getSonarHostUrl(),
              requestRuleRetrieve.getProjectKey(),
              requestRuleRetrieve.getApiToken());
      Map<String, RuleData> ruleInfoMap =
          host.getRules().stream()
              .map(RuleData::new)
              .collect(Collectors.toMap(RuleData::getKey, x -> x));
      LOG.info("Retrieved {} rules", ruleInfoMap.size());
      sendMessage.accept(
          LintMessage.ruleRetrieveResult(new ResponseRuleRetrieveResult(ruleInfoMap)));
    } catch (Exception e) {
      LOG.error("Error encountered during rule retrieval", e);
      e.printStackTrace();
      sendMessage.accept(
          LintMessage.ruleRetrieveError(e.getClass().getSimpleName() + ": " + e.getMessage()));
    }
  }

  private SonarHost getSonarHost(String url, String projectKey, String apiToken) {
    if (url.isEmpty()) {
      if (engine == null) {
        LOG.info("Using stub standalone mode - analysis engine is not initialized");
        return new StandaloneSonarHost();
      } else {
        LOG.info("Using standalone mode");
        return new StandaloneSonarHost(engine.getLoadedPlugins());
      }
    } else {
      LOG.info("Using connected mode");
      return new SonarQubeHost(
          new HttpSonarApi(url, apiToken),
          projectKey,
          Language.DELPHI.getLanguageKey(),
          Language.DELPHI.getPluginKey(),
          Language.DELPHI.getLanguageKey());
    }
  }
}
