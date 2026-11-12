#!/usr/bin/env -S powershell -File
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

##{STARTREPLACE}##
Write-Host -ForegroundColor Red "This script is a template. Do not run it directly."
Exit 1
$Version = "0.0.0" # Placeholder
$VersionName = "None" # Placeholder
$RegistryVersion = "00.0" # Placeholder
$IncludeWin64 = $false # Placeholder
##{ENDREPLACE}##
$DelphiLintFolder = Join-Path $env:APPDATA "DelphiLint"
$BinFolder = (Join-Path $DelphiLintFolder "bin")
$Bin64Folder = (Join-Path $DelphiLintFolder "bin64")

function New-AppDataPath([switch]$IncludeWin64) {
  New-Item -Path $DelphiLintFolder -ItemType Directory -ErrorAction Ignore | Out-Null
  New-Item -ItemType Directory $BinFolder -Force -ErrorAction Ignore | Out-Null
  if ($IncludeWin64) {
    New-Item -ItemType Directory $Bin64Folder -Force -ErrorAction Ignore | Out-Null
  }
  Write-Host "Created DelphiLint folder."
}

function Clear-RegistryEntry([string]$Key) {
  $RegistryPath = "HKCU:\SOFTWARE\Embarcadero\BDS\$RegistryVersion\$Key"
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

function Copy-BuildArtifacts([switch]$IncludeWin64) {
  $Artifacts = @("DelphiLintClient-$Version-$VersionName.bpl", "delphilint-server-$Version.jar")
  if ($IncludeWin64) {
    $Artifacts += "DelphiLintClient-$Version-$VersionName-Win64.bpl"
  }

  $Artifacts | ForEach-Object {
    Copy-Item -Path (Join-Path $PSScriptRoot $_) -Destination (Join-Path $DelphiLintFolder $_) -Force
  }
  Write-Host "Copied resources."
}

function Copy-WebView2([System.IO.Compression.ZipArchive]$Archive, [string]$Path, [string]$TargetDirectory) {
  $Archive.Entries |
    Where-Object { $_.FullName -eq "$Path/WebView2Loader.dll" } |
    ForEach-Object {
      [System.IO.Compression.ZipFileExtensions]::ExtractToFile($_, (Join-Path $TargetDirectory "WebView2Loader.dll"), $true)
      Write-Host "Downloaded $($_.FullName) from NuGet."
    }
}

function Get-WebView2([switch]$IncludeWin64) {
  $TempFolder = (Join-Path $DelphiLintFolder "tmp")
  New-Item -ItemType Directory $TempFolder -Force -ErrorAction Ignore | Out-Null

  $WebViewZip = (Join-Path $TempFolder "webview.zip")

  Invoke-WebRequest -Uri "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.4314.35" -OutFile $WebViewZip

  Add-Type -Assembly System.IO.Compression.FileSystem
  $Archive = [System.IO.Compression.ZipFile]::OpenRead($WebViewZip)
  try {
    Copy-WebView2 $Archive "build/native/x86" $BinFolder
    if ($IncludeWin64) {
      Copy-WebView2 $Archive "build/native/x64" $Bin64Folder
    }
  } finally {
    $Archive.Dispose()
  }

  Remove-Item $TempFolder -Recurse -Force -ErrorAction Continue
}

function Add-RegistryEntry([string]$Key, [string]$Suffix = '') {
  $BplName = "DelphiLintClient-$Version-${VersionName}${Suffix}.bpl"
  $BplPath = Join-Path $DelphiLintFolder $BplName
  $RegistryPath = "HKCU:\SOFTWARE\Embarcadero\BDS\$RegistryVersion\$Key"
  if (-not (Test-Path $RegistryPath)) {
    New-Item -Path $RegistryPath | Out-Null
  }

  New-ItemProperty -Path $RegistryPath `
    -Name $BplPath `
    -Value 'DelphiLint' `
    -PropertyType String | Out-Null

  Write-Host "Added Delphi IDE registry entry for $BplName."
}

Write-Host "Setting up DelphiLint $Version."
New-AppDataPath -IncludeWin64:$IncludeWin64
Clear-RegistryEntry "Known Packages"
if ($IncludeWin64) {
  Clear-RegistryEntry "Known Packages x64"
}
Copy-BuildArtifacts -IncludeWin64:$IncludeWin64
Get-WebView2 -IncludeWin64:$IncludeWin64
Add-RegistryEntry "Known Packages"
if ($IncludeWin64) {
  Add-RegistryEntry "Known Packages x64" "-Win64"
}
Write-Host -ForegroundColor Green "Install completed for DelphiLint $Version."