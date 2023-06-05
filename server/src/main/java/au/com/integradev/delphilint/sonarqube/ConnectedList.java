package au.com.integradev.delphilint.sonarqube;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Queue;

public class ConnectedList<T> implements Iterable<T> {
  private final String arrayName;
  private final String url;
  private final ApiConnection api;
  private final ObjectMapper jsonMapper;
  private final Class<T> clazz;

  public ConnectedList(ApiConnection api, String url, String arrayName, Class<T> clazz) {
    this.arrayName = arrayName;
    this.api = api;
    this.url = url;
    this.jsonMapper = new ObjectMapper();
    this.clazz = clazz;
  }

  private int getPageCount(JsonNode rootNode) {
    if (rootNode == null || rootNode.get("paging") == null) return -1;

    JsonNode paging = rootNode.get("paging");
    JsonNode pageTotal = paging.get("total");

    if (pageTotal == null || !pageTotal.isInt()) return -1;

    return pageTotal.asInt();
  }

  private Collection<T> getArrayContents(JsonNode rootNode) {
    if (rootNode == null || rootNode.get(arrayName) == null) return Collections.emptyList();

    JsonNode array = rootNode.get(arrayName);

    if (array == null || !array.isArray()) return Collections.emptyList();

    try {
      return jsonMapper.treeToValue(
          array, jsonMapper.getTypeFactory().constructCollectionType(List.class, clazz));
    } catch (JsonProcessingException e) {
      return Collections.emptyList();
    }
  }

  @Override
  public Iterator<T> iterator() {
    JsonNode rootNode = api.getJson(url);
    int initialPageCount = getPageCount(rootNode);
    Queue<T> initialContent = new LinkedList<>(getArrayContents(rootNode));

    return new Iterator<>() {
      private int page = 0;
      private int pageCount = initialPageCount;
      private final Queue<T> nextContent = initialContent;

      @Override
      public boolean hasNext() {
        return !nextContent.isEmpty();
      }

      @Override
      public T next() {
        if (nextContent.isEmpty()) {
          throw new NoSuchElementException();
        }

        T element = nextContent.remove();

        if (nextContent.isEmpty() && page + 1 < pageCount) {
          page += 1;
          JsonNode rootNode = api.getJson(url + "&p=" + page);
          pageCount = getPageCount(rootNode);
          nextContent.addAll(getArrayContents(rootNode));
        }

        return element;
      }
    };
  }
}
