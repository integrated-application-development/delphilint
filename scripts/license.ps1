param(
  [switch]$Check,
  [switch]$Force
)

function Get-LicenseHeaderLines([string]$Software) {
  return @(
    $Software,
    "Copyright (C) $(Get-Date -Format "yyyy") Integrated Application Development",
    "",
    "This program is free software; you can redistribute it and/or",
    "modify it under the terms of the GNU Lesser General Public",
    "License as published by the Free Software Foundation; either",
    "version 3 of the License, or (at your option) any later version.",
    "",
    "This program is distributed in the hope that it will be useful,",
    "but WITHOUT ANY WARRANTY; without even the implied warranty of",
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU",
    "Lesser General Public License for more details.",
    "",
    "You should have received a copy of the GNU General Public License",
    "along with this program. If not, see <https://www.gnu.org/licenses/>."
  )
}

function Get-JavaLicenseHeader {
  $Lines = @("/*") + (
    Get-LicenseHeaderLines "DelphiLint Server" | ForEach-Object { " * $_".TrimEnd() }
  ) + @(" */")

  return ($Lines -join "`r`n") + "`r`n"
}

function Get-JSLicenseHeader {
  $Lines = @("/*") + (
    Get-LicenseHeaderLines "DelphiLint VSCode" | ForEach-Object { " * $_".TrimEnd() }
  ) + @(" */")

  return ($Lines -join "`r`n") + "`r`n"
}

function Get-DelphiLicenseHeader {
  $Lines = @("{") + (Get-LicenseHeaderLines "DelphiLint Client" | ForEach-Object { $_.TrimEnd() }) + @("}")
  return ($Lines -join "`r`n") + "`r`n"
}

$CheckFailures = [System.Collections.Generic.List[string]]::new()

function Add-LicenseHeader([string]$Path, [string]$License) {
  if ($Check) {
    $CheckFailures.Add((Resolve-Path -Relative $Path))
  } else {
    $Content = Get-Content -Raw -Path $Path
    Set-Content -Path $Path ($License + $Content) -NoNewline
  }
}

function Remove-LicenseHeader([string]$Path, [string]$Pattern) {
  if (-not $Check) {
    $Content = Get-Content -Raw -Path $Path
    $Content = $Content -replace $Pattern,""
    Set-Content -Path $Path $Content -NoNewline
  }
}

function Select-StringRaw([string]$Path, [string]$Pattern) {
  $Content = Get-Content -Raw -Path $Path
  return $Content -match $Pattern
}

try {
  Push-Location $PSScriptRoot/..

  Get-ChildItem server/delphilint-server -Filter *.java -Recurse `
    | Where-Object { $Force -or (-not (Select-StringRaw -Path $_.FullName -Pattern "(?s)^\/\*")) } `
    | ForEach-Object {
      Remove-LicenseHeader -Path $_.FullName -Pattern "(?s)^\/\*.*?\*\/\r?\n?"
      Add-LicenseHeader -Path $_.FullName -License (Get-JavaLicenseHeader)
    }

  Get-ChildItem client -Filter *.pas -Recurse `
  | Where-Object { $Force -or (-not (Select-StringRaw -Path $_.FullName -Pattern "(?s)^\{")) } `
  | ForEach-Object {
    Remove-LicenseHeader -Path $_.FullName -Pattern "(?s)^\{.*?\}\r?\n?"
    Add-LicenseHeader -Path $_.FullName -License (Get-DelphiLicenseHeader)
  }

  Get-ChildItem companion/delphilint-vscode/src -Filter *.ts `
  | Where-Object { $Force -or (-not (Select-StringRaw -Path $_.FullName -Pattern "(?s)^\/\*")) } `
  | ForEach-Object {
    Remove-LicenseHeader -Path $_.FullName -Pattern "(?s)^\/\*.*?\*\/\r?\n?"
    Add-LicenseHeader -Path $_.FullName -License (Get-JSLicenseHeader)
  }
} finally {
  Pop-Location
}

if ($Check) {
  if ($CheckFailures) {
    Write-Host -ForegroundColor Red "No header detected in $($CheckFailures.Count) files:"
    $CheckFailures | ForEach-Object { Write-Host -ForegroundColor Red "  $_" }
    exit 1
  } else {
    Write-Host "Headers detected in all files."
  }
} else {
  Write-Host "Headers updated."
}