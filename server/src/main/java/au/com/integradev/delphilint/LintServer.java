package au.com.integradev.delphilint;

import com.sun.net.httpserver.HttpServer;
import au.com.integradev.delphilint.analysis.DelphiAnalysisEngine;
import au.com.integradev.delphilint.analysis.StandaloneDelphiConfiguration;
import au.com.integradev.delphilint.messaging.RequestAnalyze;
import au.com.integradev.delphilint.messaging.RequestCategory;
import au.com.integradev.delphilint.messaging.RequestInitialize;
import au.com.integradev.delphilint.messaging.Response;
import au.com.integradev.delphilint.messaging.ResponseAnalyzeResult;
import au.com.integradev.delphilint.sonarqube.SonarQubeConnection;
import java.io.IOException;
import java.net.InetSocketAddress;

public class LintServer {
  private final HttpServer server;
  private DelphiAnalysisEngine engine;

  public LintServer(InetSocketAddress socket) throws IOException {
    engine = null;

    server = HttpServer.create(socket, 0);
    server.createContext(
        "/",
        new LintServerHandler(
            (category, data) -> {
              try {
                return processRequest(category, data);
              } catch (Exception e) {
                return Response.unexpectedError(e.getMessage());
              }
            }));
    server.setExecutor(null);
  }

  public void run() {
    server.start();
  }

  private Response processRequest(RequestCategory category, Object data) {
    switch (category) {
      case INITIALIZE:
        return handleInitialize((RequestInitialize) data);
      case ANALYZE:
        return handleAnalyze((RequestAnalyze) data);
      case PING:
        return Response.pong((String) data);
      default:
        return Response.unexpectedError("Unknown request category");
    }
  }

  private Response handleAnalyze(RequestAnalyze requestAnalyze) {
    if (engine == null) return Response.uninitialized();

    try {
      var issues =
          engine.analyze(requestAnalyze.getBaseDir(), requestAnalyze.getInputFiles(), null);
      var result = ResponseAnalyzeResult.fromIssueSet(issues);
      return Response.analyzeResult(result);
    } catch (Exception e) {
      return Response.analyzeError(e.getMessage());
    }
  }

  private Response handleInitialize(RequestInitialize requestInitialize) {
    if (engine == null) {
      var delphiConfig =
          new StandaloneDelphiConfiguration(
              requestInitialize.getBdsPath(), requestInitialize.getCompilerVersion());

      var sonarqube =
          new SonarQubeConnection(
              requestInitialize.getSonarHostUrl(),
              requestInitialize.getProjectKey(),
              requestInitialize.getLanguageKey());

      engine = new DelphiAnalysisEngine(delphiConfig, sonarqube);
    }
    return Response.initialized();
  }
}
