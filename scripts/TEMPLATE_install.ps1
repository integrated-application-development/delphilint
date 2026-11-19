#!/usr/bin/env -S powershell -File
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

##{STARTREPLACE}##
Write-Host -ForegroundColor Red "This script is a template. Do not run it directly."
Exit 1
##{ENDREPLACE}##
$DelphiLintFolder = Join-Path $env:APPDATA "DelphiLint"
$BinFolder = (Join-Path $DelphiLintFolder "bin")

function New-AppDataPath {
  New-Item -Path $DelphiLintFolder -ItemType Directory -ErrorAction Ignore | Out-Null
  New-Item -ItemType Directory $BinFolder -Force -ErrorAction Ignore | Out-Null
  Write-Host "Created DelphiLint folder."
}

function Clear-RegistryEntry([string]$PackagesKey) {
  $RegistryPath = "HKCU:\SOFTWARE\Embarcadero\BDS\$RegistryVersion\$PackagesKey"

  if (-not (Test-Path $RegistryPath)) {
    return
  }

  (Get-ItemProperty -Path $RegistryPath).PSObject.Properties `
    | Where-Object { $_.Value -eq "DelphiLint" } `
    | ForEach-Object {
      Remove-Item $_.Name -ErrorAction Continue
      Remove-ItemProperty -Path $RegistryPath -Name $_.Name -ErrorAction Continue
      if($?) {
        Write-Host "Removed existing DelphiLint install at $($_.Name)."
      }
    }
}

function Copy-BuildArtifacts {
  $Artifacts = @("DelphiLintClient-$Version-$VersionName.bpl", "delphilint-server-$Version.jar")
  if ($IncludeWin64) {
    $Artifacts += "DelphiLintClient-$Version-$VersionName-Win64.bpl"
  }

  $Artifacts | ForEach-Object {
    Copy-Item -Path (Join-Path $PSScriptRoot $_) -Destination (Join-Path $DelphiLintFolder $_) -Force
  }
  Write-Host "Copied resources."
}

function Get-WebView2 {
  $TempFolder = (Join-Path $DelphiLintFolder "tmp")
  New-Item -ItemType Directory $TempFolder -Force -ErrorAction Ignore | Out-Null

  $WebViewZip = (Join-Path $TempFolder "webview.zip")

  Invoke-WebRequest -Uri "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.2210.55" -OutFile $WebViewZip

  $Extractions = @{ "build/native/x86/WebView2Loader.dll" = (Join-Path $BinFolder "WebView2Loader.dll") }
  if ($IncludeWin64) {
    $Win64BinFolder = Join-Path $BinFolder "win64"
    New-Item -ItemType Directory $Win64BinFolder -Force -ErrorAction Ignore | Out-Null
    $Extractions["build/native/x64/WebView2Loader.dll"] = (Join-Path $Win64BinFolder "WebView2Loader.dll")
  }

  Add-Type -Assembly System.IO.Compression.FileSystem
  $Archive = [System.IO.Compression.ZipFile]::OpenRead($WebViewZip)
  try {
    $Archive.Entries |
      Where-Object { $Extractions.ContainsKey($_.FullName) } |
      ForEach-Object {
        [System.IO.Compression.ZipFileExtensions]::ExtractToFile($_, $Extractions[$_.FullName], $true)
        Write-Host "Downloaded $($_.FullName) from NuGet."
      }
  } finally {
    $Archive.Dispose()
  }

  Remove-Item $TempFolder -Recurse -Force -ErrorAction Continue
}

function Add-RegistryEntry([string]$BplName, [string]$PackagesKey) {
  $BplPath = Join-Path $DelphiLintFolder $BplName
  $RegistryPath = "HKCU:\SOFTWARE\Embarcadero\BDS\$RegistryVersion\$PackagesKey"

  if (-not (Test-Path $RegistryPath)) {
    New-Item -Path $RegistryPath -Force | Out-Null
  }

  New-ItemProperty -Path $RegistryPath `
    -Name $BplPath `
    -Value 'DelphiLint' `
    -PropertyType String | Out-Null

  Write-Host "Added Delphi IDE registry entry for $BplName."
}

Write-Host "Setting up DelphiLint $Version."
New-AppDataPath
Clear-RegistryEntry "Known Packages"
Clear-RegistryEntry "Known Packages x64"
Copy-BuildArtifacts
Get-WebView2
Add-RegistryEntry "DelphiLintClient-$Version-$VersionName.bpl" "Known Packages"
if ($IncludeWin64) {
  Add-RegistryEntry "DelphiLintClient-$Version-$VersionName-Win64.bpl" "Known Packages x64"
}
Write-Host -ForegroundColor Green "Install completed for DelphiLint $Version."