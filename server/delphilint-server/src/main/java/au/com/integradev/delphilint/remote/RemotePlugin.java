package au.com.integradev.delphilint.remote;

public class RemotePlugin {
  private final String pluginKey;
  private final String fileName;
  private final boolean corePlugin;

  public RemotePlugin(String pluginKey, String fileName, boolean corePlugin) {
    this.pluginKey = pluginKey;
    this.fileName = fileName;
    this.corePlugin = corePlugin;
  }

  public String getPluginKey() {
    return pluginKey;
  }

  public String getFileName() {
    return fileName;
  }

  public boolean isCorePlugin() {
    return corePlugin;
  }
}
