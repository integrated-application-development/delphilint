# Advanced usage

The DelphiLint menu has a number of options:

| Menu item              | Description                                                                                                                                                   |
|------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Show DelphiLint        | Show the main DelphiLint window. This window shows analysis status and results, including issues in the active file.                                          |
| Analyze This File      | Run an analysis on the file that is currently visible in the editor.                                                                                          |
| Analyze All Open Files | Run an analysis on all project files that are currently open in the IDE.                                                                                      |
| Project Options...     | [Configure settings](#configuration) for the current Delphi project, including analysis root and SonarQube connection information.            |
| Settings...            | Configure settings for the tool in general.                                                                                                                   |
| Restart Server         | Terminate the background analysis server and start a new instance. This can be used if the server is unresponsive.                                            |

## Configuration

Project-level options can be configured via `DelphiLint > Project Options...` and are stored in a `.delphilint` file
next to the Delphi project (`.dproj`) file.

| Option                                                       | Description                                                                                                                                              |
|--------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| Analysis mode                                                | The analysis mode to run in. See [Features](#features) for more details.                                                                                 |
| Analysis settings > Base directory                           | The root directory for the analysis. Only files in this directory or subdirectories will be analyzable.                                                  |
| Analysis settings > Read sonar-project.properties if present | Whether to read a sonar-project.properties file if one is found in the base directory.                                                                   |
| SonarQube connection > Server URL                            | The URL of the SonarQube host to connect to when in connected mode.                                                                                      |
| SonarQube connection > Project key                           | The key of the corresponding SonarQube project on the SonarQube host. Optional.                                                                          |
| SonarQube connection > Authorization token                   | A user token to be used to authenticate with the SonarQube host. Optional, but required if "Force user authentication" is enabled on the SonarQube host. |
| Sonarqube connection > Use server's SonarDelphi version      | Whether to download the server's version of the SonarDelphi plugin or use the version embedded with DelphiLint.                                          |

The default DelphiLint project configuration is Standalone, with the base directory as the directory containing the
Delphi project file. SonarQube settings are ignored when in standalone mode.