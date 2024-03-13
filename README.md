<h1 id="delphilint">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/images/delphilint-title-dark.png">
    <source media="(prefers-color-scheme: light)" srcset="docs/images/delphilint-title-light.png">
    <img alt="DelphiLint" src="docs/images/delphilint-title-dark.png"/>
  </picture>
</h1>

[![Build](https://github.com/integrated-application-development/delphilint/actions/workflows/build.yml/badge.svg)](https://github.com/integrated-application-development/delphilint/actions/workflows/build.yml) [![Format](https://github.com/integrated-application-development/delphilint/actions/workflows/format.yml/badge.svg)](https://github.com/integrated-application-development/delphilint/actions/workflows/format.yml)

DelphiLint is an IDE package for RAD Studio that provides on-the-fly code analysis and linting, powered by
[SonarDelphi](https://github.com/integrated-application-development/sonar-delphi).

## Features

* Brings [SonarDelphi](https://github.com/integrated-application-development/sonar-delphi),
  a static analyzer for Delphi with 100+ code analysis rules, to the Delphi IDE
* Analyze one or more files on-the-fly, shortening the feedback loop so you can pick up and fix problems before they're even checked in
* Detected issues, along with descriptions and rationale, displayed inline in the IDE
* Two analysis modes:
   * Standalone - run analyses entirely locally with a default set of active rules
   * Connected - connect to a SonarQube instance, allowing for
      * Fetching of active rules and configuration from the server's configured quality profiles
      * Suppression of issues that have been resolved in past analyses
      * Usage of the server's version of SonarDelphi
* Support for reading standard `sonar-project.properties` files, providing additional configuration
* A Visual Studio Code companion extension that can be used to run analyses and show results in VS Code itself

## Installation

System requirements:

* Microsoft Edge 79.0.309 or above installed
* PowerShell execution policy set to `Unrestricted` (see [MSDN](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_scripts?view=powershell-7.4#how-to-run-a-script))
* [RAD Studio for Delphi 11 or above](https://www.embarcadero.com/products/delphi) installed
* [Java 11 or above](https://adoptium.net/temurin/releases/?package=jre&version=17) installed

Installation steps:

1. Download the packaged zip for your Delphi version from [the latest release](https://github.com/integrated-application-development/delphilint/releases/latest), or [build from source](#building-from-source).
2. Download or compile the latest SonarDelphi release from the [SonarDelphi repository](https://github.com/integrated-application-development/sonar-delphi).
3. Unzip the DelphiLint package folder from step 1, then run `./setup.ps1 -SonarDelphiJarLocation <path>` inside it.
4. In RAD Studio, install DelphiLint by going to Components > Install Packages and selecting the client .bpl in `%APPDATA%\DelphiLint`.

### Installing the VS Code companion

1. Download the .vsix extension file from [the latest release](https://github.com/integrated-application-development/delphilint/releases/latest),
   or [build from source](#building-the-vs-code-companion).
2. Run `code --install-extension <vsix>` to install the extension.

> [!IMPORTANT]
> For the companion to work, a DelphiLint installation of the same version must be installed.
>
> The VS Code companion is **not** required for the Delphi IDE plugin to function.

## Building from source

Prerequisites:

* RAD Studio 11
* Maven 3.5.0+
* Java 11+
* PowerShell
* npm
* Microsoft Edge 79.0.309 or above

1. Clone the repository at the latest release.
2. Build the project by running `/scripts/build.ps1 -DelphiBin <path/to/bin>`, where `<path/to/bin>` is the
   path to your Delphi IDE installation's `bin` directory (e.g. `C:\Program Files (x86)\Embarcadero\Studio\<version>\bin`).

## Usage

To analyze a file:

1. Open a Delphi project in the IDE.
2. Open the Delphi source file you want to analyze.
3. Click the `DelphiLint > Analyze This File` menu option.

It's as easy as that! The DelphiLint window will then pop up, showing the current state of analysis and any issues
that are raised. Please note that when the file has a lot of imports the analysis could take thirty seconds or so.

To analyze all files that are open in the IDE, use `DelphiLint > Analyze All Open Files` instead.

## Gallery

![A screenshot of the DelphiLint window displaying the description for the "Imports should be moved to the implementation section" rule, and corresponding issues inline in the code](docs/images/gallery-code-view-light.png)

![A screenshot of the DelphiLint window displaying the description for the "Single overloads of the standard math functions should not be used" rule, and corresponding issues inline in the code](docs/images/gallery-code-view-dark.png)

![A screenshot of the DelphiLint window displaying the description for the "IfThen should not be used like a short-circuit operator" rule, and corresponding issues inline in the code](docs/images/gallery-code-view-mountain-mist.png)

## Contributing

DelphiLint is open for contributions - please read the [contributing guide](docs/CONTRIBUTING.md) for more information.

## License

Licensed under the [GNU Lesser General Public License, Version 3.0](http://www.gnu.org/licenses/lgpl.txt).
