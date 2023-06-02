package au.com.integradev.delphilint;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import au.com.integradev.delphilint.analysis.DelphiAnalysisEngine;
import au.com.integradev.delphilint.analysis.StandaloneDelphiConfiguration;
import au.com.integradev.delphilint.messaging.Category;
import au.com.integradev.delphilint.messaging.RequestAnalyze;
import au.com.integradev.delphilint.messaging.RequestInitialize;
import au.com.integradev.delphilint.messaging.Response;
import au.com.integradev.delphilint.messaging.ResponseAnalyzeResult;
import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
import au.com.integradev.delphilint.sonarqube.SonarQubeUtils;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
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

      if (sonarqube != null) {
        issues = SonarQubeUtils.populateIssueMessages(sonarqube, issues);
      }

      ResponseAnalyzeResult result = ResponseAnalyzeResult.fromIssueSet(issues);
      result.convertPathsToAbsolute(requestAnalyze.getBaseDir());
      sendMessage.accept(Response.analyzeResult(result));
    } catch (Exception e) {
      sendMessage.accept(Response.analyzeError(e.getMessage()));
    }
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
}
