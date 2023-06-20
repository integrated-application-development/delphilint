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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import au.com.integradev.delphilint.analysis.DelphiAnalysisEngine;
import au.com.integradev.delphilint.analysis.DelphiConfiguration;
import au.com.integradev.delphilint.server.message.RequestAnalyze;
import au.com.integradev.delphilint.server.message.RequestInitialize;
import au.com.integradev.delphilint.server.message.RequestRuleRetrieve;
import au.com.integradev.delphilint.server.message.ResponseAnalyzeResult;
import au.com.integradev.delphilint.server.message.ResponseRuleRetrieveResult;
import au.com.integradev.delphilint.server.message.data.RuleData;
import au.com.integradev.delphilint.sonarqube.ApiException;
import au.com.integradev.delphilint.sonarqube.ApiUnauthorizedException;
import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
import au.com.integradev.delphilint.sonarqube.UncheckedApiException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;

public class LintServer {
  private static final String LANGUAGE_KEY = "delph";
  private static final Logger LOG = Loggers.get(LintServer.class);
  private final ServerSocket serverSocket;
  private DelphiAnalysisEngine engine;
  private final ObjectMapper mapper;
  private boolean running;

  public LintServer(int port) throws IOException {
    engine = null;
    serverSocket = new ServerSocket(port);
    running = false;
    mapper = new ObjectMapper();
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

  private void writeStream(OutputStream out, int id, LintMessage response) {
    LOG.info("Sending {}", response.getCategory());

    String dataString;
    try {
      dataString = mapper.writeValueAsString(response.getData());
    } catch (JsonProcessingException e) {
      writeStream(out, id, LintMessage.unexpectedError(e.getMessage()));
      return;
    }

    var dataBytes = dataString.getBytes(StandardCharsets.UTF_8);

    try {
      out.write(response.getCategory().getCode());
      out.write(ByteBuffer.allocate(4).putInt(id).array());
      out.write(ByteBuffer.allocate(4).putInt(dataBytes.length).array());
      out.write(dataBytes);
    } catch (IOException e) {
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
      sendMessage.accept(LintMessage.invalidRequest("Unrecognised category"));
      return;
    }

    Object data = null;

    if (category.getDataClass() != null) {
      if (length == 0) {
        sendMessage.accept(LintMessage.invalidRequest("No data supplied"));
        return;
      }

      try {
        data = mapper.readValue(dataString, category.getDataClass());
      } catch (JsonProcessingException e) {
        sendMessage.accept(LintMessage.invalidRequest("Data is in an incorrect format"));
        return;
      }
    }

    try {
      processRequest(new LintMessage(category, data), sendMessage);
    } catch (Exception e) {
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
        sendMessage.accept(LintMessage.invalidRequest("Unhandled request category"));
    }
  }

  private void handleAnalyze(RequestAnalyze requestAnalyze, Consumer<LintMessage> sendMessage) {
    if (engine == null) {
      sendMessage.accept(LintMessage.uninitialized());
      return;
    }

    SonarQubeConnection sonarqube = null;
    if (!requestAnalyze.getSonarHostUrl().isEmpty()) {
      sonarqube =
          new SonarQubeConnection(
              requestAnalyze.getSonarHostUrl(),
              requestAnalyze.getProjectKey(),
              LANGUAGE_KEY,
              requestAnalyze.getApiToken());
    }

    try {
      var issues =
          engine.analyze(
              requestAnalyze.getBaseDir(), requestAnalyze.getInputFiles(), null, sonarqube);

      ResponseAnalyzeResult result = ResponseAnalyzeResult.fromIssueSet(issues);
      result.convertPathsToAbsolute(requestAnalyze.getBaseDir());
      sendMessage.accept(LintMessage.analyzeResult(result));
    } catch (ApiUnauthorizedException e) {
      sendMessage.accept(
          LintMessage.analyzeError(
              "Authorization is required to access the configured SonarQube instance. Please"
                  + " provide an appropriate authorization token."));
    } catch (ApiException | UncheckedApiException e) {
      sendMessage.accept(
          LintMessage.analyzeError(
              "The configured SonarQube instance could not be accessed: " + e.getMessage()));
    } catch (Exception e) {
      e.printStackTrace();
      sendMessage.accept(
          LintMessage.analyzeError(
              "Unknown error during analysis: "
                  + e.getMessage()
                  + " ("
                  + e.getClass().getSimpleName()
                  + ")"));
    }

    // I'd rather not have to call this, but the server gets unacceptably large without it
    System.gc();
  }

  private void handleInitialize(
      RequestInitialize requestInitialize, Consumer<LintMessage> sendMessage) {
    if (engine == null) {
      var delphiConfig =
          new DelphiConfiguration(
              requestInitialize.getBdsPath(),
              requestInitialize.getCompilerVersion(),
              Path.of(requestInitialize.getSonarDelphiJarPath()));

      engine = new DelphiAnalysisEngine(delphiConfig);
    }
    sendMessage.accept(LintMessage.initialized());
  }

  private void handleRuleRetrieve(
      RequestRuleRetrieve requestRuleRetrieve, Consumer<LintMessage> sendMessage) {
    try {
      if (!requestRuleRetrieve.getSonarHostUrl().isEmpty()) {
        SonarQubeConnection sonarqube =
            new SonarQubeConnection(
                requestRuleRetrieve.getSonarHostUrl(),
                requestRuleRetrieve.getProjectKey(),
                LANGUAGE_KEY,
                requestRuleRetrieve.getApiToken());
        Map<String, RuleData> ruleInfoMap =
            sonarqube.getRules().stream()
                .map(RuleData::new)
                .collect(Collectors.toMap(RuleData::getKey, x -> x));
        LOG.info("Retrieved " + ruleInfoMap.size() + " rules");
        sendMessage.accept(
            LintMessage.ruleRetrieveResult(new ResponseRuleRetrieveResult(ruleInfoMap)));
      } else {
        LOG.info("No SonarQube connection, returning default ruleset");
        // TODO: Have a local set of rule definitions
        sendMessage.accept(
            LintMessage.ruleRetrieveResult(new ResponseRuleRetrieveResult(Collections.emptyMap())));
      }
    } catch (Exception e) {
      e.printStackTrace();
      sendMessage.accept(
          LintMessage.ruleRetrieveError(e.getClass().getSimpleName() + ": " + e.getMessage()));
    }
  }
}
