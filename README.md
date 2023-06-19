# DelphiLint
DelphiLint is an IDE package for RAD Studio that provides on-the-fly code analysis and linting, powered by
[SonarDelphi](https://github.com/Integrated-Application-Development/sonar-delphi).

**This repository is a work-in-progress - the software, code, and associated history is preliminary and subject to
change without notice.**

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