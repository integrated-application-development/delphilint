# FAQ

- [General](#general)
  - [How do I supply custom options to the Java Virtual Machine?](#how-do-i-supply-custom-options-to-the-java-virtual-machine)
  - [How do I uninstall DelphiLint?](#how-do-i-uninstall-delphilint)
  - [How do I authenticate with a SonarQube instance in Connected Mode?](#how-do-i-authenticate-with-a-sonarqube-instance-in-connected-mode)
- [Troubleshooting](#troubleshooting)
  - [When I go to analyze a file, it says "File not analyzable" and analysis is greyed out.](#when-i-go-to-analyze-a-file-it-says-file-not-analyzable-and-analysis-is-greyed-out)
  - ["Analyze All Open Files" does not analyze my `.dpr` or `.dpk` file, even though it is open.](#analyze-all-open-files-does-not-analyze-my-dpr-or-dpk-file-even-though-it-is-open)
  - [DelphiLint has been stuck in analysis for a long time.](#delphilint-has-been-stuck-in-analysis-for-a-long-time)
  - [I don't see any quick fixes when I right click on my issues.](#i-dont-see-any-quick-fixes-when-i-right-click-on-my-issues)
  - [I applied a quick fix and it broke my code.](#i-applied-a-quick-fix-and-it-broke-my-code)
  - [Error: "Could not connect to the configured SonarQube instance."](#error-could-not-connect-to-the-configured-sonarqube-instance)
  - [Error: "SonarDelphi could not be retrieved from GitHub."](#error-sonardelphi-could-not-be-retrieved-from-github)
  - [Fixing invalid SSL certificates](#fixing-invalid-ssl-certificates)
  - [DelphiLint fails, saying that the server failed to communicate the port.](#delphilint-fails-saying-that-the-server-failed-to-communicate-the-port)
  - [DelphiLint can't find my Java executable / is using the wrong Java executable.](#delphilint-cant-find-my-java-executable--is-using-the-wrong-java-executable)

## General

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

### How do I authenticate with a SonarQube instance in Connected Mode?

Unless "Force user authentication" is disabled on the SonarQube instance, you will require a SonarQube token.
SonarQube allows you to generate three types of tokens:

* User tokens, which authenticate as a certain user
* Project analysis tokens, which can be used to run analyses on the project it was generated for
* Global analysis tokens, which can be used to run analyses on every project

While DelphiLint supports all three token types, it is highly recommended that you provide **user** tokens if possible.
Due to an undocumented limitation in SonarQube's API, project analysis tokens and global analysis tokens are unable to
access information about project security hotspots.

For more information about generating and using tokens, see the
[SonarQube documentation](https://docs.sonarsource.com/sonarqube/latest/user-guide/user-account/generating-and-using-tokens/).

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

### I don't see any quick fixes when I right click on my issues.

The available quick fixes are dependent on the SonarDelphi version being used. The first quick fixes were implemented
in SonarDelphi 1.5.0. If you are using a version older than 1.5.0, please upgrade to take advantage of quick fixes.

In addition, please note that not all rules have quick fixes available.

### I applied a quick fix and it broke my code.

If the file has changed since the last analysis, quick fixes can easily become invalidated. Because they are based
on replacing ranges of text within the file, if their offset from the originating issue becomes different than the
"correct" offset, the quick fix may apply in the wrong area and provide unexpected results.

Please note that this is a limitation of quick fixes in any tool and is not considered a "bug" per se. That being
said, improving the experience around preserving and invalidating quick fixes as the file changes is an area of
ongoing work - it is likely to improve in the future.

If the quick fix is broken even if there have been no file changes since the last analysis, this is probably a
legitimate bug in the implementation of the quick fix -
in this case, [raise an issue](https://github.com/integrated-application-development/delphilint/issues).

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

### DelphiLint fails, saying that the server failed to communicate the port.

This is usually due to the server failing to start on the provided Java version. Please ensure that the version
of Java that is being used is version 11 or higher, since DelphiLint uses Java 11 features and is compiled with
Java 11.

If multiple Java installations are installed and DelphiLint is choosing the wrong one, it is recommended to manually
specify the path to the desired Java 11+ executable
(see [below](#delphilint-cant-find-my-java-executable--is-using-the-wrong-java-executable)).

### DelphiLint can't find my Java executable / is using the wrong Java executable.

Most Java installations set the `JAVA_HOME` environment variable, which points to the root directory of the Java
installation. If this variable is present, DelphiLint defaults to `%JAVA_HOME%\bin\java.exe`.

It's also possible to manually specify the path to the Java executable if `JAVA_HOME` isn't present, or points to the
wrong Java version. This can be done in the DelphiLint External Resources Setup window at
`DelphiLint > Settings > Set up external resources`.
