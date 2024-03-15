#! powershell -File

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

##{STARTREPLACE}##
Write-Host -ForegroundColor Red "This script is a template. Do not run it directly."
Exit 1
##{ENDREPLACE}##
$DelphiLintFolder = Join-Path $env:APPDATA "DelphiLint"
$BinFolder = (Join-Path $DelphiLintFolder "bin")
$TempFolder = (Join-Path $DelphiLintFolder "tmp")

Write-Host "Setting up DelphiLint $Version."
New-Item -Path $DelphiLintFolder -ItemType Directory -ErrorAction Ignore | Out-Null
New-Item -ItemType Directory $BinFolder -Force -ErrorAction Ignore | Out-Null
New-Item -ItemType Directory $TempFolder -Force -ErrorAction Ignore | Out-Null
Write-Host "Created DelphiLint folder."

Remove-Item (Join-Path $DelphiLintFolder "DelphiLintClient*.bpl") -ErrorAction Continue
Remove-Item (Join-Path $DelphiLintFolder "delphilint-server*.jar") -ErrorAction Continue
Write-Host "Deleted any existing build artifacts."

@("DelphiLintClient-$Version-$PackageVersion.bpl", "delphilint-server-$Version.jar") | ForEach-Object {
  Copy-Item -Path (Join-Path $PSScriptRoot $_) -Destination (Join-Path $DelphiLintFolder $_) -Force
}
Write-Host "Copied resources."

$WebViewZip = (Join-Path $TempFolder "webview.zip")

Invoke-WebRequest -Uri "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.2210.55" -OutFile $WebViewZip

Add-Type -Assembly System.IO.Compression.FileSystem
$Archive = [System.IO.Compression.ZipFile]::OpenRead($WebViewZip)
try {
  $Archive.Entries |
    Where-Object { $_.FullName -eq "build/native/x86/WebView2Loader.dll" } |
    ForEach-Object {
      [System.IO.Compression.ZipFileExtensions]::ExtractToFile($_, (Join-Path $BinFolder "WebView2Loader.dll"), $true)
      Write-Host "Downloaded $($_.Name) from NuGet."
    }
} finally {
  $Archive.Dispose()
}

$DirsOnPath = $env:Path -split ";"
if($DirsOnPath -icontains $BinFolder) {
  Write-Host "Bin directory is already on user path."
} else {
  [Environment]::SetEnvironmentVariable("Path", $env:Path + ";$BinFolder", [System.EnvironmentVariableTarget]::User)
  Write-Host "Added bin directory to user path."
}

Remove-Item $TempFolder -Recurse -Force -ErrorAction Continue
Write-Host "Install completed for DelphiLint $Version."

@(
  "",
  "Almost there! DelphiLint's files are now installed at:"
  "  $env:APPDATA\DelphiLint"
  "",
  "Add the plugin bpl (below) to your IDE via Components > Install Packages:"
  "  $env:APPDATA\DelphiLint\DelphiLintClient-$Version-$PackageVersion.bpl"
) | Write-Host