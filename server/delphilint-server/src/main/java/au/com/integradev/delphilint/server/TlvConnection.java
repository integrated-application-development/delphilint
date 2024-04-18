/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
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

import au.com.integradev.delphilint.server.message.RequestAnalyze;
import au.com.integradev.delphilint.server.message.RequestInitialize;
import au.com.integradev.delphilint.server.message.RequestRuleRetrieve;
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
import java.util.function.Consumer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TlvConnection {
  private static final Logger LOG = LogManager.getLogger(TlvConnection.class);
  private final ServerSocket socket;
  private final ObjectMapper mapper;
  private final AnalysisServer server;
  private boolean running;

  public TlvConnection(AnalysisServer server) throws IOException {
    this(server, 0);
  }

  public TlvConnection(AnalysisServer server, int port) throws IOException {
    socket = new ServerSocket(port);
    running = false;
    mapper = new ObjectMapper();
    this.server = server;

    LOG.info("DelphiLint server started on port {}", socket.getLocalPort());
  }

  public void run() throws IOException {
    LOG.info("Awaiting socket connection");

    Socket clientSocket = socket.accept();

    LOG.info("Socket connected");

    var out = clientSocket.getOutputStream();
    var in = clientSocket.getInputStream();

    running = true;
    while (running) {
      LOG.debug("Awaiting next message");
      readStream(in, out);
    }

    LOG.info("Terminating server");

    out.close();
    in.close();
    clientSocket.close();
  }

  public int getPort() {
    return socket.getLocalPort();
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
        server.initialize((RequestInitialize) message.getData(), sendMessage);
        break;
      case ANALYZE:
        server.analyze((RequestAnalyze) message.getData(), sendMessage);
        break;
      case RULE_RETRIEVE:
        server.retrieveRules((RequestRuleRetrieve) message.getData(), sendMessage);
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
}
