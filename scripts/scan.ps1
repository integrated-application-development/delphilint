#! powershell -File
param(
  [Parameter(Mandatory)]
  [string]$ScannerLocation,
  [Parameter(Mandatory)]
  [string]$HostUrl,
  [Parameter(ValueFromRemainingArguments)]
  [string[]]$RemainingArgs,
  [Parameter()]
  [string]$Token,
  [Parameter()]
  [string]$Login,
  [Parameter()]
  [string]$Password
)
$ErrorActionPreference = "Continue"

function Get-SonarArgs {
  $SonarArgs = $RemainingArgs

  if($HostUrl) { $SonarArgs += @("-Dsonar.host.url=" + $HostUrl) }
  if($Token) { $SonarArgs += @("-Dsonar.token=" + $Token) }
  if($Login) { $SonarArgs += @("-Dsonar.login=" + $Login) }
  if($Password) { $SonarArgs += @("-Dsonar.password=" + $Password) }

  return $SonarArgs
}

function Invoke-ClientScan {
  Push-Location (Join-Path $PSScriptRoot "../client")
  try {
    & $ScannerLocation @(Get-SonarArgs)
  } finally {
    Pop-Location
  }
}

function Invoke-ServerScan {
  Push-Location (Join-Path $PSScriptRoot "../server")
  try {
    & mvn clean install sonar:sonar @(Get-SonarArgs)
  } finally {
    Pop-Location
  }
}

function Invoke-CompanionScan {
  Push-Location (Join-Path $PSScriptRoot "../companion/delphilint-vscode")
  try {
    & $ScannerLocation @(Get-SonarArgs)
  } finally {
    Pop-Location
  }
}

Write-Host -ForegroundColor Cyan "Scanning DelphiLint Client..."
Invoke-ClientScan
Write-Host -ForegroundColor Cyan "Scanning DelphiLint Server..."
Invoke-ServerScan
Write-Host -ForegroundColor Cyan "Scanning DelphiLint Companion..."
Invoke-CompanionScan
Write-Host -ForegroundColor Green "Scans complete."