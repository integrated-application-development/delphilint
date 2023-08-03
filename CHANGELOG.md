# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* New user setting to toggle autosaving files before analysis, defaulting to yes
* Issues now display server metadata such as creation date, status, and assignee when in Connected Mode

### Fixed

* VS Code companion: Improved error handling when the server responds unexpectedly

## [0.3.0] - 2023-08-01

### Added

* Visual Studio Code companion extension to DelphiLint that uses the DelphiLint analysis engine to run analyses,
  show issues, and read configuration files
* Analyze All Files and Analyze All Open Files commands for the VS Code companion
* Ability to choose the active Delphi project for analyses through the VS Code companion

## [0.2.2] - 2023-08-01

### Changed

* Removed "Server Configuration" settings from IDE settings page, and recategorised those settings under a
  "Debug" section in the settings file
* DelphiLint now retrieves the Java executable to use from the `JAVA_HOME` environment variable unless there
  is an override defined (it previously only read `JAVA_HOME` when the setting was first initialized)
* The link colour in rule description view is now consistent with the IDE link colour

### Fixed

* Security hotspots are no longer suppressed when they match server hotspots marked as acknowledged

## [0.2.1] - 2023-07-26

### Added

* Security hotspots are now suppressed when they match reviewed server hotspots
* When an analysis is started, by default the DelphiLint window is shown if it is not already visible
* New user setting to optionally disable the above window auto-show behaviour

### Fixed

* Issues on the same line are now always displayed in the same order
* When autolaunch is off, DelphiLint now attempts to connect to a server on port 14000 instead of hanging
* Issues are no longer suppressed when they match server issues marked as REMOVED
* The IDE plugin is now correctly marked as design-time only

## [0.2.0] - 2023-07-25

### Added

* Show DelphiLint actions in IDE Insight search bar
* Allow DelphiLint actions to be added to editor toolbars
* Keyboard shortcuts for Analyze This File (Shift+Ctrl+L) and Analyze All Open Files (Shift+Ctrl+Alt+L)
* Friendly error messages for common analysis errors

### Fixed

* Scanning a large number of units at once in Connected Mode no longer produces a SonarQube access error
* Files with one issue now display "1 issue" instead of "1 issues"
* Running an analysis with "Read sonar-project.properties if present" enabled no longer produces an error
  if there is no `sonar-project.properties` file present

## [0.1.0] - 2023-07-20

### Added

* Analysis server that uses [SonarDelphi](https://github.com/Integrated-Application-Development/sonar-delphi)
  to analyze Delphi code
* IDE plugin for RAD Studio 11 that interfaces with the analysis server
* Support for connection to SonarQube, including quality profile, rule, plugin, and
  resolved issue retrieval
* Support for reading `sonar-project.properties`

[unreleased]: https://github.com/Integrated-Application-Development/delphilint/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/Integrated-Application-Development/delphilint/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/Integrated-Application-Development/delphilint/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/Integrated-Application-Development/delphilint/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/Integrated-Application-Development/delphilint/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/Integrated-Application-Development/delphilint/releases/tag/v0.1.0