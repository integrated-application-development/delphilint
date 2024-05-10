# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* Clear Active File menu item, which clears all issues from the active file.
* VS Code companion: Clear Issues For This File command, which removes any outstanding issues on the current file.
* Support for "quick fixes" in the IDE - suggested fixes that can be applied automatically.

### Changed

* Changes in issue validity are now immediately reflected across the UI.
* The issue view has been redesigned for improved performance and utility.
* The default SonarDelphi version is now [1.5.0](https://github.com/integrated-application-development/sonar-delphi/releases/tag/v1.5.0).

### Fixed

* Issues with low impact severities are no longer incorrectly displayed with the medium severity icon.
* VS Code companion: Re-analyzing a file that previously had issues will now clear the old issues when there are no new
  issues reported.
* Multiline issues now display their underlines correctly.
* Right clicking the separator between the issue view and rule view no longer prevents the separator from being moved.
* Issue line trackers are now invalidated when the backing IDE buffer is freed.

## [1.0.2] - 2024-04-02

### Changed

* Double clicking an issue in the issue view now centers the relevant line in the editor.
* The default SonarDelphi version is now [1.4.0](https://github.com/integrated-application-development/sonar-delphi/releases/tag/v1.4.0).

### Fixed

* Idling before the DelphiLint server has been started no longer causes noticeable CPU usage.

## [1.0.1] - 2024-03-20

### Added

* The arguments passed to the server JVM can now be customized via the `Server.JvmOptions` setting.

### Changed

* The server JVM is now initialized in `-server` mode.

### Fixed

* DelphiLint now passes SonarDelphi the correct Delphi installation path and compiler version when running in Delphi 12.

## [1.0.0] - 2024-03-19

### Added

* Support for the [`requiredForLanguages` plugin metadata property](https://community.sonarsource.com/t/the-sonarscanners-download-only-required-3rd-party-plugins/108156)
  when connected to a SonarQube 10.4+ server.
* [SonarDelphi](https://github.com/integrated-application-development/sonar-delphi) is now automatically downloaded
  when run in Standalone Mode.
* "SonarDelphi Version Override" settings option, which overrides the default SonarDelphi version with a particular
  GitHub release.

### Changed

* SonarQube tokens have been moved from project-level configuration to user-level settings, allowing project
  configuration files to be safely checked into source control.
* SonarQube API fields that are deprecated in 10.2 and above are no longer used for those versions.
* Logs are now periodically cleaned up.
* The server is now initialized asynchronously, instead of synchronously on the main IDE thread.

### Removed

* "SonarDelphi Jar Override" settings option.

### Fixed

* The rule description HTML embed no longer has an arbitrary maximum width.
* Issues are now correctly matched on lines with non-ASCII characters.
* Issues are no longer incorrectly discarded on ANSI and UTF-16 files.

## [0.7.0] - 2024-01-24

### Added

* Syntax highlighting for code examples in rule descriptions.
* Diff views for code examples in rule descriptions.
* Enhanced security for temporary HTML files.

### Changed

* Inline issue display and underline have been redesigned to be more prominent.
* The server is no longer immediately restarted if it is killed - instead it is restarted when it is next needed.
* The embedded HTML renderer is now Microsoft Edge instead of Internet Explorer.
* Zoom functionality, history navigation, and the right click context menu is now disabled in rule descriptions.
* The rule description scrollbar has been redesigned to be more subtle.

### Fixed

* Scanning both source files (in `sonar.sources`) and test files (in `sonar.tests`) at the same time no longer
  fails with an error in Connected Mode.
* A plugin-destabilising access violation no longer occurs if the server connection times out.
* Analyzing a file that has a relative path including spaces no longer fails with an error in Connected Mode.
* Analyzing a file with a file-level issue no longer raises an exception.
* Connecting to the DelphiLint server no longer occasionally fails to read a temp file.
* Issue matching now correctly handles lines with special characters in ANSI-encoded files.

## [0.6.0] - 2023-10-31

### Added

* Support for the new Clean Code taxonomy for rules and issues.
* Support for the new Sonar standard for rule descriptions.

### Changed

* Security hotspots no longer have an accompanying severity.

### Fixed

* VS Code companion: Non-Delphi source files are now correctly excluded from "Analyze All Open Files" analyses.
* VS Code companion: Development versions of the DelphiLint server are now correctly retrieved.

## [0.5.0] - 2023-08-15

### Added

* Support for custom rules plugins - all server plugins whose keys contain the phrase "delphi" are now retrieved and used for analysis.
* Icons for rule type and severity in rule view.

### Fixed

* "Unspecified error" exception when docking the DelphiLint panel or changing layout while the rule view is open.
* "Unspecified error" exception when debugging a program and changing between analyzed files.
* Unreviewed security hotspots are no longer suppressed.

## [0.4.1] - 2023-08-08

### Changed

* Connected Mode issue metadata now shows "Unassigned" instead of nothing when an issue is unassigned.

### Fixed

* Security hotspots are no longer suppressed when they match server hotspots marked as acknowledged.
* VS Code companion: Error messages raised by SonarDelphi when parsing a file are now displayed correctly when they
  contain brackets.
* VS Code companion: Long messages from the DelphiLint server are no longer occasionally cut off, causing "End of JSON
  input" errors.

## [0.4.0] - 2023-08-04

### Added

* New user setting to toggle autosaving files before analysis, defaulting to yes.
* Issues now display server metadata such as creation date, status, and assignee when in Connected Mode.
* Display hints for DelphiLint actions on hover when they are placed on an IDE toolbar.
* Icons in issue view representing issue severity and type.
* VS Code companion: Standalone mode for analysis.

### Changed

* Updated lint status icons to match smooth issue icon design.
* Removed issues now display nothing before the issue message (previously displayed "(removed)").
* VS Code companion: The "DelphiLint Server" output log now disregards the Debug.ShowConsole ini setting and is
  always created.

### Fixed

* VS Code companion: Improved error handling when the server responds unexpectedly.
* VS Code companion: Boolean values in project options or settings are now interpreted correctly.

## [0.3.0] - 2023-08-01

### Added

* Visual Studio Code companion extension to DelphiLint that uses the DelphiLint analysis engine to run analyses,
  show issues, and read configuration files.
* Analyze All Files and Analyze All Open Files commands for the VS Code companion.
* Ability to choose the active Delphi project for analyses through the VS Code companion.

## [0.2.2] - 2023-08-01

### Changed

* Removed "Server Configuration" settings from IDE settings page, and recategorised those settings under a
  "Debug" section in the settings file.
* DelphiLint now retrieves the Java executable to use from the `JAVA_HOME` environment variable unless there
  is an override defined (it previously only read `JAVA_HOME` when the setting was first initialized).
* The link colour in rule description view is now consistent with the IDE link colour.

### Fixed

* Security hotspots are no longer suppressed when they match server hotspots marked as acknowledged.

## [0.2.1] - 2023-07-26

### Added

* Security hotspots are now suppressed when they match reviewed server hotspots.
* When an analysis is started, by default the DelphiLint window is shown if it is not already visible.
* New user setting to optionally disable the above window auto-show behaviour.

### Fixed

* Issues on the same line are now always displayed in the same order.
* When autolaunch is off, DelphiLint now attempts to connect to a server on port 14000 instead of hanging.
* Issues are no longer suppressed when they match server issues marked as REMOVED.
* The IDE plugin is now correctly marked as design-time only.

## [0.2.0] - 2023-07-25

### Added

* Show DelphiLint actions in IDE Insight search bar.
* Allow DelphiLint actions to be added to editor toolbars.
* Keyboard shortcuts for Analyze This File (Shift+Ctrl+L) and Analyze All Open Files (Shift+Ctrl+Alt+L).
* Friendly error messages for common analysis errors.

### Fixed

* Scanning a large number of units at once in Connected Mode no longer produces a SonarQube access error.
* Files with one issue now display "1 issue" instead of "1 issues".
* Running an analysis with "Read sonar-project.properties if present" enabled no longer produces an error
  if there is no `sonar-project.properties` file present.

## [0.1.0] - 2023-07-20

### Added

* Analysis server that uses [SonarDelphi](https://github.com/integrated-application-development/sonar-delphi)
  to analyze Delphi code.
* IDE plugin for RAD Studio 11 that interfaces with the analysis server.
* Support for connection to SonarQube, including quality profile, rule, plugin, and
  resolved issue retrieval.
* Support for reading `sonar-project.properties`.

[unreleased]: https://github.com/integrated-application-development/delphilint/compare/v1.0.2...HEAD
[1.0.2]: https://github.com/integrated-application-development/delphilint/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/integrated-application-development/delphilint/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/integrated-application-development/delphilint/compare/v0.7.0...v1.0.0
[0.7.0]: https://github.com/integrated-application-development/delphilint/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/integrated-application-development/delphilint/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/integrated-application-development/delphilint/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/integrated-application-development/delphilint/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/integrated-application-development/delphilint/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/integrated-application-development/delphilint/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/integrated-application-development/delphilint/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/integrated-application-development/delphilint/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/integrated-application-development/delphilint/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/integrated-application-development/delphilint/releases/tag/v0.1.0