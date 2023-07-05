# ![DelphiLint](delphilint-title-dark.png#gh-dark-mode-only)![DelphiLint](delphilint-title-light.png#gh-light-mode-only)

DelphiLint is an IDE package for RAD Studio that provides on-the-fly code analysis and linting, powered by
[SonarDelphi](https://github.com/Integrated-Application-Development/sonar-delphi).

## Features

* Integration with [IntegraDev SonarDelphi](https://github.com/Integrated-Application-Development/sonar-delphi),
  including 100+ code analysis rules to pick up on code smells, bugs, and vulnerabilities
* On-demand analysis in the Delphi IDE, both single-file and multi-file
* Two analysis modes:
   * Standalone - run analyses entirely locally with a default set of active rules
   * Connected - connect to a SonarQube instance, allowing for
      * Automatic synchronization of active rules and configuration from the server's configured quality profiles
      * Suppression of issues that have been resolved in past analyses
* Support for reading `sonar-project.properties` files

## Installation

Before installing, the following prerequisites must be installed on your system:

* RAD Studio 11.2+
* Java 11+

The process for installing is as follows:

1. Download the latest release zip from the releases page.
2. Open RAD Studio and navigate to `Component > Install Packages`, then click the `Add...` button and navigate to
   the `DelphiLintClient.bpl` package file on your filesystem.
3. Copy the `delphilint-server.jar` file to `%APPDATA%\DelphiLint\delphilint-server.jar`.
4. Download the latest SonarDelphi release from the [IntegraDev SonarDelphi repository](https://github.com/Integrated-Application-Development/sonar-delphi)
   and copy the `.jar` file to `%APPDATA%\DelphiLint\sonar-delphi-plugin.jar`.

## Usage

DelphiLint adds a menu item to the main menu with a number of options:

| Menu item              | Description                                                                                                                                                   |
|------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Show DelphiLint        | Show the main DelphiLint window. This window shows analysis status and results, including issues in the active file.                                          |
| Analyze This File      | Run an analysis on the file that is currently visible in the editor.                                                                                          |
| Analyze All Open Files | Run an analysis on all project files that are currently open in the IDE.                                                                                      |
| Project Options...     | Configure [analysis options](#project-configuration) for the current Delphi project, including analysis root and SonarQube connection information.            |
| Settings...            | Configure settings for the tool in general, including server configuration. These options are generally not necessary for the average user to self-configure. |
| Restart Server         | Terminate the background analysis server and start a new instance. This can be used if the server is unresponsive.                                            |

### Project configuration

Project-level options can be configured via `DelphiLint > Project Options...` and are stored in a `.delphilint` file
next to the Delphi project (`.dproj`) file.

| Option                                     | Description                                                                                                                                              |
|--------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| Analysis mode                              | The analysis mode to run in. See [Features](#features) for more details.                                                                                 |
| Analysis settings > Base directory         | The root directory for the analysis. Only files in this directory or subdirectories will be analyzable.                                                  |
| SonarQube connection > Server URL          | The URL of the SonarQube host to connect to when in connected mode.                                                                                      |
| SonarQube connection > Project key         | The key of the corresponding SonarQube project on the SonarQube host. Optional.                                                                          |
| SonarQube connection > Authorization token | A user token to be used to authenticate with the SonarQube host. Optional, but required if "Force user authentication" is enabled on the SonarQube host. |

The default DelphiLint project configuration is Standalone, with the base directory as the directory containing the
Delphi project file. SonarQube settings are ignored when in standalone mode.

## Troubleshooting

#### When I go to analyze a file, it says "File not analyzable" and analysis is greyed out.

Make sure that your project base directory is correctly configured in the options of your current project, and that
the file is a Delphi source file (`.pas`, `.dpr`, `.dpk`).
Only Delphi source files under the base directory (including in subdirectories) are able to be analyzed.

#### "Analyze All Open Files" does not analyze my `.dpr` or `.dpk` file, even though it is open.

This is intentional, as analyzing `.dpr` and `.dpk` files typically raises a large number of erroneous issues due to
dependency analysis limitations. `.dpr` and `.dpk` files can be explicitly analyzed using "Analyze This File".

#### DelphiLint has been stuck in analysis for a long time.

Generally speaking, DelphiLint analyses can take upwards of 30 seconds when dealing with files with many imports. If it
has been a longer time, check the progress of the scan in the logs at
`%APPDATA%\DelphiLint\logs\delphilint-server.log`. If a problem seems to have occurred, the server can be restarted
with `DelphiLint > Restart Server`.

## Contributing

To request a new feature or submit a bug, please create an issue and clearly state your request or problem. Even if
you are planning to submit a pull request, please create an issue first so it can be discussed if necessary.

To contribute, please create a pull request, link it to an existing issue, and clearly state what your change is.
Please ensure that any Delphi code follows the same style as the existing code, and that running `mvn verify` in
the `/server` directory succeeds with no changes generated.

### Building from source

#### Building the client project

The Delphi project is built with and only officially supports Delphi 11.2. Other versions may also be able to compile
the project, but no assurances are made.

To build, open the `DelphiLintClient.dproj` file in RAD Studio and choose `Project > Build DelphiLintClient`. The
output Delphi package file will be generated at `/client/target/DelphiLintClient.bpl`, and can be installed as usual.

#### Building the server project

The server project is built with Maven and requires Java 11 or above.

To build, run `mvn package` in the `/server` directory. The output server jar will be generated at
`/server/delphilint-server/target/delphilint-server-<version>-with-dependencies.jar`.
