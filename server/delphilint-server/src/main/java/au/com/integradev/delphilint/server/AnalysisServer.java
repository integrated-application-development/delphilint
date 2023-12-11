package au.com.integradev.delphilint.server;

import au.com.integradev.delphilint.analysis.AnalysisOrchestrator;
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

/**
 * As the interface to all of DelphiLint's core functionality, the analysis server manages
 * initialisation of the analysis orchestrator, running analyses, and connection to any external
 * hosts.
 */
public class AnalysisServer {
  private static final Logger LOG = LogManager.getLogger(AnalysisServer.class);
  private AnalysisOrchestrator orchestrator;
  private final CachingPluginDownloader pluginDownloader;
  private Set<DownloadedPlugin> pluginGroup;

  public AnalysisServer(Path pluginsPath) {
    orchestrator = null;
    pluginDownloader = new CachingPluginDownloader(pluginsPath);
    pluginGroup = null;
  }

  /**
   * Attempts to start a Delphi analysis via an initialized orchestrator.
   *
   * <ul>
   *   <li>If the orchestrator is uninitialized, returns an unexpected error (242).
   *   <li>If the orchestrator is initialized and the analysis fails, returns an analysis error
   *       (36).
   *   <li>If the orchestrator is initialized and the analysis succeeds, returns an analysis result
   *       (35).
   * </ul>
   *
   * @param requestAnalyze the parameters to run the analysis with.
   * @param sendMessage a callback to send a tagged message back to the client.
   */
  public void analyze(RequestAnalyze requestAnalyze, Consumer<LintMessage> sendMessage) {
    if (orchestrator == null) {
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
      var logOutput = new SonarDelphiLogOutput();
      SonarLintLogger.setTarget(logOutput);

      var issues =
          orchestrator.runAnalysis(
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

  /**
   * Attempts to initialize the analysis orchestrator.
   *
   * <ul>
   *   <li>If an error occurs or the SonarQube host cannot be reached, returns an initialization
   *       error (26).
   *   <li>If the initialization succeeds, returns an initialization successful (20).
   * </ul>
   *
   * @param requestInitialize the parameters to initialize the orchestrator with.
   * @param sendMessage a callback to send a message back to the client.
   */
  public void initialize(RequestInitialize requestInitialize, Consumer<LintMessage> sendMessage) {
    try {
      SonarHost host =
          getSonarHost(requestInitialize.getSonarHostUrl(), "", requestInitialize.getApiToken());
      Set<DownloadedPlugin> desiredPluginGroup =
          pluginDownloader.getRemotePluginJars(host).orElse(null);

      if (!Objects.equals(pluginGroup, desiredPluginGroup) || orchestrator == null) {
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

        orchestrator = new AnalysisOrchestrator(delphiConfig);
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
      sendMessage.accept(
          LintMessage.initializeError(
              "Unknown error during analysis: "
                  + e.getMessage()
                  + " ("
                  + e.getClass().getSimpleName()
                  + ")"));
    }
  }

  /**
   * Attempts to retrieve rule metadata from a host.
   *
   * <ul>
   *   <li>If an error occurs, returns a rule retrieve error (46).
   *   <li>If the retrieval is successful, returns a rule retrieve result (45).
   * </ul>
   *
   * @param requestRuleRetrieve the parameters to use to retrieve the rule metadata.
   * @param sendMessage a callback to send a message back to the client.
   */
  public void retrieveRules(
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
      sendMessage.accept(
          LintMessage.ruleRetrieveError(e.getClass().getSimpleName() + ": " + e.getMessage()));
    }
  }

  private SonarHost getSonarHost(String url, String projectKey, String apiToken) {
    if (url.isEmpty()) {
      if (orchestrator == null) {
        LOG.info("Using stub standalone mode - analysis engine is not initialized");
        return new StandaloneSonarHost();
      } else {
        LOG.info("Using standalone mode");
        return new StandaloneSonarHost(orchestrator.getLoadedPlugins());
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
