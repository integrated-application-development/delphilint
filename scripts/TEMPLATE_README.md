# DelphiLint {{VERSION}}

This package contains [DelphiLint](https://github.com/integrated-application-development/delphilint)
{{VERSION}}, compiled for Delphi {{DELPHI_NAME}}.

DelphiLint is an IDE package for RAD Studio that integrates the static analyzer
[SonarDelphi](https://github.com/integrated-application-development/sonar-delphi) into the Delphi IDE.

## Contents

| File                                  | Description                                             |
|---------------------------------------|---------------------------------------------------------|
| `DelphiLintClient-{{VERSION}}-*.bpl`  | The DelphiLint IDE plugin                               |
| `delphilint-server-{{VERSION}}.jar`   | The DelphiLint analysis server, used by the plugin      |
| `install.bat` / `install.ps1`         | Installation script                                     |
| `LICENSE.txt`                         | License (GNU Lesser General Public License v3)          |

## Installation

System requirements:

* Microsoft Edge 79.0.309 or above
* Delphi {{DELPHI_NAME}}
* [Java 11 or above](https://adoptium.net/temurin/releases/?package=jre&version=17)

To install, run `install.bat`, then restart the Delphi IDE.

> **Note:** Using precompiled BPLs in a different minor Delphi version from the one they were
> compiled with can cause compatibility problems in some circumstances. If you encounter issues,
> please [build from source](https://github.com/integrated-application-development/delphilint#building-from-source).

## Documentation

* [Project readme](https://github.com/integrated-application-development/delphilint#readme)
* [Configuration guide](https://github.com/integrated-application-development/delphilint/blob/master/docs/CONFIGURATION.md)
* [FAQ](https://github.com/integrated-application-development/delphilint/blob/master/docs/FAQ.md)
* [Issue tracker](https://github.com/integrated-application-development/delphilint/issues)
