#! powershell -File
$ErrorActionPreference = "Stop"

Import-Module "$PSScriptRoot/common" -Force

function Wait-BuildArtifact([string]$Path, [string]$Message) {
  while(-not (Test-Path $Path)) {
    Write-Host -ForegroundColor Red $Message
    $TryAgain = Read-Host -Prompt "Try again? (y/n)"
    if(-not (Test-PromptConfirmed $TryAgain)) {
      Exit
    }
  }
  Write-Host "Build artifact at $Path exists."
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

function Wait-ClientVersion([string]$Version, [string]$Message) {
  $Path = (Join-Path $PSScriptRoot "../client/source/dlversion.inc")

  while(-not (Test-ClientVersion -Path $Path -Version $Version)) {
    Write-Host -ForegroundColor Red $Message
    $TryAgain = Read-Host -Prompt "Try again? (y/n)"
    if(-not (Test-PromptConfirmed $TryAgain)) {
      Exit
    }
  }
  Write-Host "Version is set correctly as $Version in dlversion.inc."
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

function Get-ClientBpl([string]$BuildConfig) {
  return Resolve-Path (Join-Path $PSScriptRoot "../client/source/target/Win32/$BuildConfig/DelphiLintClient.bpl")
}

function Get-ServerJar([string]$Version) {
  return Resolve-Path (Join-Path $PSScriptRoot "../server/delphilint-server/target/delphilint-server-$Version.jar")
}

#-----------------------------------------------------------------------------------------------------------------------

$Version = Get-Version
$BuildConfig = "Release"

Write-Host "Packaging DelphiLint $Version."

Write-Host "`nDue diligence:"
Wait-Prompt "  1. Is the client .bpl compiled for $Version in ${BuildConfig}?" -ExitOnNo
Wait-Prompt "  2. Is the server .jar compiled for ${Version}?" -ExitOnNo

Write-Host "`nValidating build artifacts..."

$ClientBpl = Get-ClientBpl $BuildConfig
$ServerJar = Get-ServerJar $Version

Wait-ClientVersion -Version $Version -Message "Update the constants in client/source/dlversion.inc to $Version."
Wait-BuildArtifact -Path $ClientBpl -Message "Build the client .bpl for $BuildConfig."
Wait-BuildArtifact -Path $ServerJar -Message "Build the server .jar for $Version."

Write-Host "Build artifacts validated.`n"

$TargetDir = Resolve-Path (Join-Path $PSScriptRoot "../target")
New-Item -ItemType Directory $TargetDir -Force | Out-Null
Get-ChildItem -Path $TargetDir -Recurse | Remove-Item -Force -Recurse

$PackageDir = Join-Path $TargetDir "DelphiLint-$Version"
New-Item -ItemType Directory $PackageDir -Force | Out-Null

Write-Host "Package directory created."

Copy-Item $ClientBpl (Join-Path $PackageDir "DelphiLintClient-$Version.bpl") | Out-Null
Copy-Item $ServerJar (Join-Path $PackageDir "delphilint-server-$Version.jar") | Out-Null
New-SetupScript -Path (Join-Path $PackageDir "setup.ps1") -Version $Version

Write-Host "Build artifacts copied."

$ZippedPackage = Join-Path $TargetDir "DelphiLint-$Version.zip"
Compress-Archive $PackageDir -DestinationPath $ZippedPackage -Force

Write-Host "Package compressed at $ZippedPackage."

Write-Host -ForegroundColor Green "DelphiLint $Version packaged."