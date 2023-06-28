# DelphiLint
DelphiLint is an IDE package for RAD Studio that provides on-the-fly code analysis and linting, powered by
[SonarDelphi](https://github.com/Integrated-Application-Development/sonar-delphi).

**This repository is a work-in-progress - the software, code, and associated history is preliminary and subject to
change without notice.**

## Features

* Integration with [SonarDelphi](https://github.com/Integrated-Application-Development/sonar-delphi), including
  100+ code analysis rules to pick up on code smells, bugs, and vulnerabilities
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