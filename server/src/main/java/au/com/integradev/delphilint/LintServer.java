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
package au.com.integradev.delphilint;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import au.com.integradev.delphilint.analysis.DelphiAnalysisEngine;
import au.com.integradev.delphilint.analysis.StandaloneDelphiConfiguration;
import au.com.integradev.delphilint.messaging.Category;
import au.com.integradev.delphilint.messaging.RequestAnalyze;
import au.com.integradev.delphilint.messaging.RequestInitialize;
import au.com.integradev.delphilint.messaging.RequestRuleRetrieve;
import au.com.integradev.delphilint.messaging.Response;
import au.com.integradev.delphilint.messaging.ResponseAnalyzeResult;
import au.com.integradev.delphilint.messaging.ResponseRuleRetrieveResult;
import au.com.integradev.delphilint.sonarqube.RuleInfo;
import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
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

  private void writeStream(OutputStream out, int id, Response response) {
    LOG.info("Sending {}", response.getCategory());

    String dataString;
    try {
      dataString = mapper.writeValueAsString(response.getData());
    } catch (JsonProcessingException e) {
      writeStream(out, id, Response.unexpectedError(e.getMessage()));
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
    Category category;
    int id;
    int length;
    String dataString;
    try {
      category = Category.fromCode(in.read());
      id = ByteBuffer.wrap(in.readNBytes(4)).getInt();
      length = ByteBuffer.wrap(in.readNBytes(4)).getInt();
      dataString = new String(in.readNBytes(length), StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    LOG.info("Received {}", category);

    Consumer<Response> sendMessage = (response -> writeStream(out, id, response));

    if (category == null) {
      sendMessage.accept(Response.invalidRequest("Unrecognised category"));
      return;
    }

    Object data = null;

    if (category.getDataClass() != null) {
      if (length == 0) {
        sendMessage.accept(Response.invalidRequest("No data supplied"));
        return;
      }

      try {
        data = mapper.readValue(dataString, category.getDataClass());
      } catch (JsonProcessingException e) {
        sendMessage.accept(Response.invalidRequest("Data is in an incorrect format"));
        return;
      }
    }

    try {
      processRequest(new Response(category, data), sendMessage);
    } catch (Exception e) {
      sendMessage.accept(Response.unexpectedError(e.getMessage()));
    }
  }

  private void processRequest(Response message, Consumer<Response> sendMessage) {
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
        sendMessage.accept(Response.pong((String) message.getData()));
        break;
      default:
        sendMessage.accept(Response.invalidRequest("Unhandled request category"));
    }
  }

  private void handleAnalyze(RequestAnalyze requestAnalyze, Consumer<Response> sendMessage) {
    if (engine == null) {
      sendMessage.accept(Response.uninitialized());
      return;
    }

    try {
      SonarQubeConnection sonarqube = null;
      if (!requestAnalyze.getSonarHostUrl().isEmpty()) {
        sonarqube =
            new SonarQubeConnection(
                requestAnalyze.getSonarHostUrl(), requestAnalyze.getProjectKey(), LANGUAGE_KEY);
      }

      var issues =
          engine.analyze(
              requestAnalyze.getBaseDir(), requestAnalyze.getInputFiles(), null, sonarqube);

      ResponseAnalyzeResult result = ResponseAnalyzeResult.fromIssueSet(issues);
      result.convertPathsToAbsolute(requestAnalyze.getBaseDir());
      sendMessage.accept(Response.analyzeResult(result));
    } catch (Exception e) {
      e.printStackTrace();
      sendMessage.accept(
          Response.analyzeError(e.getClass().getSimpleName() + ": " + e.getMessage()));
    }

    // I'd rather not have to call this, but the server gets unacceptably large without it
    System.gc();
  }

  private void handleInitialize(
      RequestInitialize requestInitialize, Consumer<Response> sendMessage) {
    if (engine == null) {
      var delphiConfig =
          new StandaloneDelphiConfiguration(
              requestInitialize.getBdsPath(),
              requestInitialize.getCompilerVersion(),
              Path.of(requestInitialize.getSonarDelphiJarPath()));

      engine = new DelphiAnalysisEngine(delphiConfig);
    }
    sendMessage.accept(Response.initialized());
  }

  private void handleRuleRetrieve(
      RequestRuleRetrieve requestRuleRetrieve, Consumer<Response> sendMessage) {
    try {
      if (!requestRuleRetrieve.getSonarHostUrl().isEmpty()) {
        SonarQubeConnection sonarqube =
            new SonarQubeConnection(
                requestRuleRetrieve.getSonarHostUrl(),
                requestRuleRetrieve.getProjectKey(),
                LANGUAGE_KEY);
        Map<String, RuleInfo> ruleInfoMap = sonarqube.getRuleInfosByRuleKey();
        LOG.info("Retrieved " + ruleInfoMap.size() + " rules");
        sendMessage.accept(
            Response.ruleRetrieveResult(new ResponseRuleRetrieveResult(ruleInfoMap)));
      } else {
        LOG.info("No SonarQube connection, returning default ruleset");
        // TODO: Have a local set of rule definitions
        sendMessage.accept(
            Response.ruleRetrieveResult(new ResponseRuleRetrieveResult(Collections.emptyMap())));
      }
    } catch (Exception e) {
      e.printStackTrace();
      sendMessage.accept(
          Response.ruleRetrieveError(e.getClass().getSimpleName() + ": " + e.getMessage()));
    }
  }
}
