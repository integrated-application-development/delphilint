#! powershell -File
param(
  [Parameter(Mandatory)]
  [ValidatePattern("^\d\.\d\.\d(\.[A-Fa-f0-9]{7})?$")]
  [string]$Version,
  [Parameter()]
  [string]$BuildConfig = "Release"
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

function Test-PromptConfirmed([string]$Response) {
  return $Response -imatch "^y"
}

function Wait-Prompt([string]$Message, [switch]$ExitOnNo) {
  $Done = $false
  $Response = "n"
  while(-not $Done) {
    $Response = Read-Host -Prompt "$Message (y/n)"
    $Done = Test-PromptConfirmed $Response
    if($ExitOnNo -and -not ($Done)) {
      Exit
    }
  }
}

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
  if(-not ($Version -match "^(\d)\.(\d)\.(\d)(\.([A-Fa-f0-9]{7}))?$")) {
    Write-Error "Invalid version format for $Version."
    Exit
  }

  $Major = $Matches[1]
  $Minor = $Matches[2]
  $Patch = $Matches[3]
  $Commit = $Matches[5]
  $DevVersion = ($null -ne $Commit)
  $DevVersionStr = if($DevVersion) { "True" } else { "False" }

  $DlVersionContent = Get-Content $Path -Raw

  $MatchMajor = $DlVersionContent -imatch ".*{MAJOR}$Major{\/MAJOR}.*"
  $MatchMinor = $DlVersionContent -imatch ".*{MINOR}$Minor{\/MINOR}.*"
  $MatchPatch = $DlVersionContent -imatch ".*{PATCH}$Patch{\/PATCH}.*"
  $MatchDevVersion = $DlVersionContent -imatch ".*{DEV}$DevVersionStr{\/DEV}.*"

  if($DevVersion) {
    $CurrentCommit = (& git rev-parse --short HEAD)
    $MatchCommit = $CurrentCommit -imatch $Commit
  } else {
    $MatchCommit = $true
  }


  return $MatchMajor -and $MatchMinor -and $MatchPatch -and $MatchDevVersion -and $MatchCommit
}

function Wait-ClientVersion([string]$Version, [string]$Message) {
  $Path = (Join-Path $PSScriptRoot "client/source/dlversion.inc")

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
  return Join-Path $PSScriptRoot "client/target/Win32/$BuildConfig/DelphiLintClient.bpl"
}

function Get-ServerJar([string]$Version) {
  return Join-Path $PSScriptRoot "server/delphilint-server/target/delphilint-server-$Version.jar"
}

Write-Host "Packaging DelphiLint $Version (build configuration $BuildConfig)."

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

$TargetDir = Join-Path $PSScriptRoot "target"
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

Write-Host -ForegroundColor Green "DelphiLint $Version built."