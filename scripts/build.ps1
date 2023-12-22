#! powershell -File
param(
  [Parameter(Mandatory)]
  [string]$DelphiBin,
  [switch]$ShowOutput,
  [switch]$SkipCompanion
)

$ErrorActionPreference = "Stop"
Import-Module "$PSScriptRoot/common" -Force

$Version = Get-Version
$StaticVersion = $Version -replace "\+dev.*$","+dev"
$BuildConfig = "Release"

$ClientBpl = Join-Path $PSScriptRoot "../client/source/target/Win32/$BuildConfig/DelphiLintClient.bpl"
$ServerJar = Join-Path $PSScriptRoot "../server/delphilint-server/target/delphilint-server-$Version.jar"
$CompanionVsix = Join-Path $PSScriptRoot "../companion/delphilint-vscode/delphilint-vscode-$StaticVersion.vsix"

$TargetDir = Join-Path $PSScriptRoot "../target"
$PackageDir = Join-Path $TargetDir "DelphiLint-$Version"

function Assert-BuildArtifact([string]$Path, [string]$Message) {
  if(Test-Path $Path) {
    Write-Status -Status Success "$(Resolve-PathToRoot $Path) exists."
  } else {
    Write-Status -Status Problem "$Path does not exist."
    Exit
  }
}

function Test-ClientVersion([string]$Path, [string]$Version) {
  $Split = Split-Version $Version

  $DevVersionStr = if($Split.Dev) { "True" } else { "False" }

  $DlVersionContent = Get-Content $Path -Raw
  $MatchMajor = $DlVersionContent -imatch ".*{MAJOR}$($Split.Major){\/MAJOR}.*"
  $MatchMinor = $DlVersionContent -imatch ".*{MINOR}$($Split.Minor){\/MINOR}.*"
  $MatchPatch = $DlVersionContent -imatch ".*{PATCH}$($Split.Patch){\/PATCH}.*"
  $MatchDevVersion = $DlVersionContent -imatch ".*{DEV}$DevVersionStr{\/DEV}.*"

  return $MatchMajor -and $MatchMinor -and $MatchPatch -and $MatchDevVersion
}

function Assert-ClientVersion([string]$Version, [string]$Message) {
  $Path = (Join-Path $PSScriptRoot "../client/source/dlversion.inc")

  if(Test-ClientVersion -Path $Path -Version $Version) {
    Write-Status -Status Success "Version is set correctly as $Version in dlversion.inc."
  } else {
    Write-Status -Status Problem "Version is not set correctly as $Version in dlversion.inc."
    Exit
  }
}

function New-SetupScript([string]$Path, [string]$Version) {
  $SetupScript = @(
    '#! powershell -File',
    'param(',
    '  [Parameter(Mandatory)]',
    '  [string]$SonarDelphiJarLocation',
    ')',
    '',
    '$ErrorActionPreference = "Stop"',
    '',
    "`$Version = '$Version'",
    '',
    '$DelphiLintFolder = Join-Path $env:APPDATA "DelphiLint"',
    '',
    'Write-Host "Setting up DelphiLint $Version."',
    'New-Item -Path $DelphiLintFolder -ItemType Directory -ErrorAction Ignore | Out-Null',
    'Write-Host "Created DelphiLint folder."',
    '',
    'Remove-Item (Join-Path $DelphiLintFolder "DelphiLintClient*.bpl") -ErrorAction Continue',
    'Remove-Item (Join-Path $DelphiLintFolder "delphilint-server*.jar") -ErrorAction Continue',
    'Write-Host "Deleted any existing build artifacts."',
    '',
    '@("DelphiLintClient-$Version.bpl", "delphilint-server-$Version.jar") | ForEach-Object {',
    '  Copy-Item -Path (Join-Path $PSScriptRoot $_) -Destination (Join-Path $DelphiLintFolder $_) -Force',
    '}',
    'Write-Host "Copied resources."',
    '',
    '$SonarDelphiJar = Resolve-Path $SonarDelphiJarLocation',
    'Copy-Item -Path $SonarDelphiJar -Destination (Join-Path $DelphiLintFolder "sonar-delphi-plugin.jar") -Force',
    'Write-Host "Copied SonarDelphi."',
    '',
    'Write-Host "Setup completed for DelphiLint $Version."'
  )

  Set-Content -Path $Path -Value $SetupScript
}

function Invoke-ClientCompile([string]$DelphiBin, [string]$BuildConfig) {
  Push-Location (Join-Path $PSScriptRoot ..\client\source)
  try {
    & cmd /c "`"$DelphiBin\\rsvars.bat`" && msbuild /p:config=`"$BuildConfig`""
  }
  finally {
    Pop-Location
  }
}

function Invoke-ServerCompile() {
  Push-Location (Join-Path $PSScriptRoot ..\server)
  try {
    & .\buildversioned.ps1
  }
  finally {
    Pop-Location
  }
}

function Invoke-VscCompanionCompile {
  Push-Location (Join-Path $PSScriptRoot ..\companion\delphilint-vscode)
  try {
    & npm install
    & vsce package
  }
  finally {
    Pop-Location
  }
}

function Clear-TargetFolder {
  New-Item -ItemType Directory $TargetDir -Force | Out-Null
  Get-ChildItem -Path $TargetDir -Recurse | Remove-Item -Force -Recurse
}

function New-PackageFolder([hashtable]$Artifacts) {
  New-Item -ItemType Directory $PackageDir -Force

  $Artifacts.GetEnumerator() | ForEach-Object {
    Copy-Item -Path $_.Key -Destination (Join-Path $PackageDir $_.Value)
  }
  New-SetupScript -Path (Join-Path $PackageDir "setup.ps1") -Version $Version
}

function Invoke-Project([hashtable]$Project) {
  if($Project.Prerequisite) {
    Write-Host -ForegroundColor Yellow "Preconditions:"
    & $Project.Prerequisite
    Write-Host
  }

  $Time = Measure-Command {
    $Output = ""

    if($ShowOutput) {
      & $Project.Build | ForEach-Object { Write-Host $_ }
    } else {
      $Output = (& $Project.Build)
    }

    if($LASTEXITCODE -eq 0) {
      Write-Host -ForegroundColor Green "Succeeded" -NoNewline
    } else {
      $Output | ForEach-Object { Write-Host $_ }
      Write-Problem "Failed."
      Exit
    }
  }
  Write-Host -ForegroundColor Green " in $($Time.TotalSeconds) seconds."

  if($Project.Postrequisite) {
    Write-Host
    Write-Host -ForegroundColor Yellow "Postconditions:"
    & $Project.Postrequisite
  }
}

#-----------------------------------------------------------------------------------------------------------------------

$StandaloneArtifacts = @{}
$PackagedArtifacts = @{}

$Projects = @(
  @{
    "Name" = "Build client"
    "Prerequisite" = {
      Assert-ClientVersion -Version $Version
    }
    "Build" = {
      Invoke-ClientCompile -DelphiBin $DelphiBin -BuildConfig $BuildConfig
      $PackagedArtifacts.Add($ClientBpl, "DelphiLintClient-$Version.bpl");
    }
    "Postrequisite" = {
      Assert-BuildArtifact $ClientBpl
    }
  },
  @{
    "Name" = "Build server"
    "Build" = {
      Invoke-ServerCompile
      $StandaloneArtifacts.Add($ServerJar, "delphilint-server-$Version.jar");
      $PackagedArtifacts.Add($ServerJar, "delphilint-server-$Version.jar");
    }
    "Postrequisite" = {
      Assert-BuildArtifact $ServerJar
    }
  },
  @{
    "Name" = "Build VS Code companion"
    "Build" = {
      if($SkipCompanion) {
        Write-Host -ForegroundColor Yellow "-SkipCompanion flag passed - skipping build."
      } else {
        Invoke-VscCompanionCompile
        $StandaloneArtifacts.Add($CompanionVsix, "delphilint-vscode-$StaticVersion.vsix");
      }
    }
    "Postrequisite" = {
      if(-not $SkipCompanion) {
        Assert-BuildArtifact $CompanionVsix
      }
    }
  },
  @{
    "Name" = "Collate build artifacts"
    "Build" = {
      Clear-TargetFolder
      $StandaloneArtifacts.GetEnumerator() | ForEach-Object {
        Copy-Item -Path $_.Key -Destination (Join-Path $TargetDir $_.Value)
      }
      New-PackageFolder $PackagedArtifacts
    }
    "Postrequisite" = {
      $StandaloneArtifacts.Values | ForEach-Object {
        Assert-BuildArtifact (Join-Path $TargetDir $_)
      }
      $PackagedArtifacts.Values | ForEach-Object {
        Assert-BuildArtifact (Join-Path $PackageDir $_)
      }
    }
  },
  @{
    "Name" = "Zip build artifacts"
    "Build" = {
      $ZippedPackage = Join-Path $TargetDir "DelphiLint-$Version.zip"
      Compress-Archive $PackageDir -DestinationPath $ZippedPackage -Force
    }
    "Postrequisite" = {
      $ZippedPackage = Join-Path $TargetDir "DelphiLint-$Version.zip"
      Assert-BuildArtifact $ZippedPackage
    }
  }
);

#-----------------------------------------------------------------------------------------------------------------------

Write-Title "Packaging DelphiLint ${Version}"

$Time = Measure-Command {
  $Projects | ForEach-Object {
    Write-Header $_.Name
    Invoke-Project $_
  }
}

Write-Title "DelphiLint $Version packaged"
Write-Host "Succeeded in $($Time.TotalSeconds) seconds."