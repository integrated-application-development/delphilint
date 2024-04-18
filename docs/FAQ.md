# FAQ

- [General](#general)
  - [Why is Delphi Community Edition not supported?](#why-is-delphi-community-edition-not-supported)
  - [How do I supply custom options to the Java Virtual Machine?](#how-do-i-supply-custom-options-to-the-java-virtual-machine)
  - [How do I uninstall DelphiLint?](#how-do-i-uninstall-delphilint)
- [Troubleshooting](#troubleshooting)
  - [When I go to analyze a file, it says "File not analyzable" and analysis is greyed out.](#when-i-go-to-analyze-a-file-it-says-file-not-analyzable-and-analysis-is-greyed-out)
  - ["Analyze All Open Files" does not analyze my `.dpr` or `.dpk` file, even though it is open.](#analyze-all-open-files-does-not-analyze-my-dpr-or-dpk-file-even-though-it-is-open)
  - [DelphiLint has been stuck in analysis for a long time.](#delphilint-has-been-stuck-in-analysis-for-a-long-time)
  - [Error: "Could not connect to the configured SonarQube instance."](#error-could-not-connect-to-the-configured-sonarqube-instance)
  - [Error: "SonarDelphi could not be retrieved from GitHub."](#error-sonardelphi-could-not-be-retrieved-from-github)
  - [Fixing invalid SSL certificates](#fixing-invalid-ssl-certificates)

## General

### Why is Delphi Community Edition not supported?

[SonarDelphi](https://github.com/integrated-application-development/sonar-delphi), which DelphiLint uses behind the
scenes, requires source code for all dependencies. Delphi Community Edition does not provide source code for the
standard library.

### How do I supply custom options to the Java Virtual Machine?

> [!WARNING]
> This should not be necessary for most DelphiLint installations.

DelphiLint reads the JVM's startup arguments from the `JvmOptions` setting in `%APPDATA\DelphiLint\delphilint.ini` -
this can be freely edited at your own risk.

### How do I uninstall DelphiLint?

Like any other IDE package, DelphiLint can be removed from an IDE installation by going to
`Components > Install Packages`, selecting DelphiLint, and clicking "Remove" in the bottom-right corner.

To totally remove all traces of DelphiLint from your system, delete the folder `%APPDATA%\DelphiLint`.

> [!NOTE]
> When upgrading to a new DelphiLint version, the install script automatically uninstalls any other versions -
> there is no need to perform a manual uninstall beforehand.

## Troubleshooting

### When I go to analyze a file, it says "File not analyzable" and analysis is greyed out.

Make sure that your project base directory is correctly configured in the options of your current project, and that
the file is a Delphi source file (`.pas`, `.dpr`, `.dpk`).
Only Delphi source files under the base directory (including in subdirectories) are able to be analyzed.

### "Analyze All Open Files" does not analyze my `.dpr` or `.dpk` file, even though it is open.

This is intentional, as analyzing `.dpr` and `.dpk` files may raise erroneous issues due to
dependency analysis limitations. `.dpr` and `.dpk` files can be explicitly analyzed using "Analyze This File".

### DelphiLint has been stuck in analysis for a long time.

Generally speaking, DelphiLint analyses can take upwards of 30 seconds when dealing with files with many imports. If it
has been a longer time, check the progress of the scan in the logs at
`%APPDATA%\DelphiLint\logs\delphilint-server.log`. If a problem seems to have occurred, the server can be restarted
with `DelphiLint > Restart Server`.

### Error: "Could not connect to the configured SonarQube instance."

This means that the configured "Server URL" was totally inaccessible from DelphiLint. This generally means one of
two things:

1. The server URL is incorrect (this can be changed at `DelphiLint > Project Options`).
2. The server uses a HTTPS connection signed with a certificate that has not been
   added to the Java Virtual Machine's keystore - see [Fixing invalid SSL certificates](#fixing-invalid-ssl-certificates).

### Error: "SonarDelphi could not be retrieved from GitHub."

DelphiLint tries to download the SonarDelphi plugin jar from [SonarDelphi's GitHub releases](https://github.com/integrated-application-development/sonar-delphi/releases).
This dialog appears if there's a problem doing so - because GitHub's servers are almost never offline, this is usually a problem with your internet connection.

If you're confident your internet connection is working, checking the logs at `%APPDATA\DelphiLint\delphilint-server.log` might reveal the issue. Look for a line saying
`Fallback provider could not provide plugin` and the corresponding error.

If the error is an `SSLHandshakeException`, see [Fixing invalid SSL certificates](#fixing-invalid-ssl-certificates).
Otherwise, please [raise an issue](https://github.com/integrated-application-development/delphilint/issues), including
your log file, and we'll see how we can help.

### Fixing invalid SSL certificates

This occurs when DelphiLint tries to access a site via HTTPS, but the site's certificate is not issued by a
trusted certificate authority. This indicates that the HTTPS connection could have been compromised by a
[man-in-the-middle (MITM) attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack).

> [!TIP]
> In corporate settings, proxies like [WatchGuard](https://www.watchguard.com/help/docs/help-center/en-US/Content/en-US/Fireware/proxies/https/https_proxy_contentinspection_c.html)
> may act as a man-in-the-middle, decrypting HTTPS traffic then re-signing it using another certificate.
> If this is the case, this issue will appear for any attempt to access a public site.

To solve the problem, the certificate must be provided to the Java Virtual Machine (which maintains a list of
trusted certificates). This can be done in two ways:

* **Solution 1:** Add the certificate to the JVM keystore:
  ```powershell
  keytool -import -file <path/to/cert> -alias <unique name> -keystore <JAVA_HOME>/lib/security/cacerts
  ```
* **Solution 2:** Force the JVM to switch to the Windows key store instead, by adding the following to
  [DelphiLint's JVM options](#how-do-i-supply-custom-options-to-the-java-virtual-machine):
  ```
  -Djavax.net.ssl.trustStore=NUL -Djavax.net.ssl.trustStoreType=Windows-ROOT
  ```

For more information, see this [blog post](https://chancharles.medium.com/java-consultant-tip-ssl-certificates-and-man-in-the-middle-ssl-proxy-3867b81ee5f0).