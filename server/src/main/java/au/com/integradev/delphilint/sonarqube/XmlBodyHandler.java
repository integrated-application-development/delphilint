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
package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandler;
import java.net.http.HttpResponse.BodySubscriber;
import java.net.http.HttpResponse.ResponseInfo;
import java.nio.charset.StandardCharsets;
import java.util.function.Supplier;

public class XmlBodyHandler<T> implements BodyHandler<Supplier<T>> {
  private final Class<T> clazz;
  private static final XmlMapper mapper = new XmlMapper();

  public XmlBodyHandler(Class<T> clazz) {
    this.clazz = clazz;
  }

  @Override
  public BodySubscriber<Supplier<T>> apply(ResponseInfo responseInfo) {
    return asJson(clazz);
  }

  private static <T> BodySubscriber<Supplier<T>> asJson(Class<T> clazz) {
    var upstream = HttpResponse.BodySubscribers.ofString(StandardCharsets.UTF_8);

    return HttpResponse.BodySubscribers.mapping(
        upstream,
        (String body) ->
            (Supplier<T>)
                () -> {
                  try {
                    return mapper.readValue(body, clazz);
                  } catch (JsonProcessingException e) {
                    return null;
                  }
                });
  }
}
