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

function Clear-RegistryEntry {
  $RegistryPath = "HKCU:\SOFTWARE\Embarcadero\BDS\$RegistryVersion\Known Packages"

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
  @("DelphiLintClient-$Version-$PackageVersion.bpl", "delphilint-server-$Version.jar") | ForEach-Object {
    Copy-Item -Path (Join-Path $PSScriptRoot $_) -Destination (Join-Path $DelphiLintFolder $_) -Force
  }
  Write-Host "Copied resources."
}

function Get-WebView2 {
  $TempFolder = (Join-Path $DelphiLintFolder "tmp")
  New-Item -ItemType Directory $TempFolder -Force -ErrorAction Ignore | Out-Null

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
  if ($DirsOnPath -icontains $BinFolder) {
    Write-Host "Bin directory is already on user path."
  } else {
    [Environment]::SetEnvironmentVariable("Path", $env:Path + ";$BinFolder", [System.EnvironmentVariableTarget]::User)
    Write-Host "Added bin directory to user path."
  }

  Remove-Item $TempFolder -Recurse -Force -ErrorAction Continue
}

function Add-RegistryEntry {
  $BplName = "DelphiLintClient-$Version-$PackageVersion.bpl"
  $BplPath = Join-Path $DelphiLintFolder $BplName
  New-ItemProperty -Path "HKCU:\SOFTWARE\Embarcadero\BDS\$RegistryVersion\Known Packages" `
    -Name $BplPath `
    -Value 'DelphiLint' `
    -PropertyType String | Out-Null

  Write-Host "Added Delphi IDE registry entry for $BplName."
}

Write-Host "Setting up DelphiLint $Version."
New-AppDataPath
Clear-RegistryEntry
Copy-BuildArtifacts
Get-WebView2
Add-RegistryEntry
Write-Host -ForegroundColor Green "Install completed for DelphiLint $Version."