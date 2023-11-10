# FAQ

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