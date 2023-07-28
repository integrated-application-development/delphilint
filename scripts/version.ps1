#! powershell -File
$ErrorActionPreference = "Stop"

Import-Module "$PSScriptRoot/common" -Force

function Expand-DlVersionMacro([string]$Content, [string]$Macro, [string]$Value) {
  return $Content -replace "({$Macro)}.*?{(\/$Macro})", "`$1}$Value{`$2"
}

function Set-ClientVersion([string]$Path, [string]$Version) {
  $Split = Split-Version $Version

  $DlVersionContent = Get-Content -Path $Path -Raw

  $DevVersionStr = if ($Split.Dev) { "True" } else { "False" }

  $DlVersionContent = Expand-DlVersionMacro $DlVersionContent -Macro "MAJOR" -Value $Split.Major
  $DlVersionContent = Expand-DlVersionMacro $DlVersionContent -Macro "MINOR" -Value $Split.Minor
  $DlVersionContent = Expand-DlVersionMacro $DlVersionContent -Macro "PATCH" -Value $Split.Patch
  $DlVersionContent = Expand-DlVersionMacro $DlVersionContent -Macro "DEV" -Value $DevVersionStr

  Set-Content $Path -Value $DlVersionContent -NoNewline
}

function Set-ServerVersion([string[]]$Paths, [string]$Version) {
  $Split = Split-Version $Version

  $DevStr = if ($Split.Dev) { "+dev.`${revision}" } else { "" }
  $VersionStr = "$($Split.Major).$($Split.Minor).$($Split.Patch)$DevStr"

  [regex]$VersionTag = "<version>(.*?)</version>"

  $Paths | ForEach-Object {
    $Content = Get-Content $_ -Raw
    $Content = ($VersionTag.replace($Content, "`<version>$VersionStr</version>", 1))
    Set-Content $_ -Value $Content -NoNewline
  }
}

function Set-VscClientVersion([string]$Path, [string]$Version) {
  $Split = Split-Version $Version

  $DevStr = if ($Split.Dev) { "+dev" } else { "" }
  $VersionStr = "$($Split.Major).$($Split.Minor).$($Split.Patch)$DevStr"

  [regex]$VersionProp = "`"version`":\s*`".*?`""

  $PackageContent = Get-Content $Path -Raw
  $PackageContent = $VersionProp.replace($PackageContent, "`"version`": `"$VersionStr`"", 1)
  Set-Content $Path -Value $PackageContent -NoNewline
}

#-----------------------------------------------------------------------------------------------------------------------

$Version = Get-Version

Set-ClientVersion (Join-Path $PSScriptRoot "../client/source/dlversion.inc") $Version

$PomFiles = @("../server", "../server/delphilint-server", "../server/sonarlint-core-overrides") |
  ForEach-Object { "$PSScriptRoot/$_/pom.xml" }

Set-ServerVersion $PomFiles $Version

Set-VscClientVersion (Join-Path $PSScriptRoot "../companion/delphilint-vscode/package.json") $Version