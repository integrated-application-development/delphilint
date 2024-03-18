#! powershell -File
param(
  [Parameter(Mandatory)]
  [string]$ScannerLocation,
  [Parameter(Mandatory)]
  [string]$HostUrl,
  [Parameter(ValueFromRemainingArguments)]
  [string[]]$RemainingArgs
)
$ErrorActionPreference = "Continue"
Import-Module "$PSScriptRoot/common" -Force

function Get-SonarArgs {
  $SonarArgs = $RemainingArgs
  if ($HostUrl) { $SonarArgs += @("-Dsonar.host.url=" + $HostUrl) }
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

#-----------------------------------------------------------------------------------------------------------------------

Write-Title "Scanning DelphiLint projects"
$Time = Measure-Command {
  Write-Header "Scan client"
  Invoke-ClientScan | Write-Host
  Write-Header "Scan server"
  Invoke-ServerScan | Write-Host
  Write-Header "Scan companion"
  Invoke-CompanionScan | Write-Host
}
Write-Title "DelphiLint projects scanned"
Write-Host "Succeeded in $($Time.TotalSeconds) seconds."