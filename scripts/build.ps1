#! powershell -File
param(
  [switch]$ShowOutput,
  [switch]$SkipCompanion,
  [Parameter(ValueFromRemainingArguments)]
  [string[]]$DelphiVersions
)

$ErrorActionPreference = "Stop"
Import-Module "$PSScriptRoot/common" -Force

$Global:DelphiVersionMap = @{
  "280" = [DelphiVersion]::new("11 Alexandria", "280", "22.0")
  "290" = [DelphiVersion]::new("12 Athens", "290", "23.0")
}

class DelphiVersion {
  [string]$Name
  [string]$PackageVersion
  [string]$RegistryVersion

  DelphiVersion([string]$Name, [string]$PackageVersion, [string]$RegistryVersion) {
    $this.Name = $Name
    $this.PackageVersion = $PackageVersion
    $this.RegistryVersion = $RegistryVersion
  }
}

class DelphiInstall {
  [DelphiVersion]$Version
  [string]$BinPath

  DelphiInstall([string]$PackageVersion) {
    $this.Version = $Global:DelphiVersionMap[$PackageVersion]
    $this.BinPath = "C:\Program Files (x86)\Embarcadero\Studio\$($this.Version.RegistryVersion)\bin"
  }

  DelphiInstall([string]$PackageVersion, [string]$BinPath) {
    $this.PackageVersion = $PackageVersion
    $this.BinPath = $BinPath
  }
}

class PackagingConfig {
  [DelphiInstall]$Delphi
  [Hashtable]$Artifacts
  [string]$Version

  PackagingConfig([DelphiInstall]$Delphi) {
    $this.Delphi = $Delphi
    $this.Artifacts = @{}
    $this.Version = Get-Version
  }

  [string] GetOutputBplName() {
    return "DelphiLintClient-$($this.Version)-$($this.Delphi.Version.PackageVersion).bpl"
  }

  [string] GetInputBplPath() {
    $Ver = $this.Delphi.Version.PackageVersion
    return Join-Path $PSScriptRoot "../client/source/target/$Ver/Release/DelphiLintClient$Ver.bpl"
  }

  [string] GetPackageFolderName() {
    return "DelphiLint-$($this.Version)-$($this.Delphi.Version.PackageVersion)"
  }
}

$DelphiInstalls = $DelphiVersions `
  | ForEach-Object { ,($_ -split '=') } `
  | Where-Object {
      $SupportedVersion = $DelphiVersionMap.ContainsKey($_[0])

      if(-not $SupportedVersion) {
        Write-Host "Delphi version '$($_[0])' is not compatible with DelphiLint, ignoring."
      }

      return $SupportedVersion
    } `
  | ForEach-Object {
      if($_.Length -gt 1) {
        return [DelphiInstall]::new($_[0], $_[1])
      } else {
        return [DelphiInstall]::new($_[0])
      }
    }

if ($DelphiInstalls.Length -eq 0) {
  Write-Problem "Please supply at least one version to build for."
  Exit
}

$Version = Get-Version
$StaticVersion = $Version -replace "\+dev.*$","+dev"

$ServerJar = Join-Path $PSScriptRoot "../server/delphilint-server/target/delphilint-server-$Version.jar"
$CompanionVsix = Join-Path $PSScriptRoot "../companion/delphilint-vscode/delphilint-vscode-$StaticVersion.vsix"

$TargetDir = Join-Path $PSScriptRoot "../target"

function Assert-Exists([string]$Path) {
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

function Invoke-ClientCompile([PackagingConfig]$Config) {
  Push-Location (Join-Path $PSScriptRoot ..\client\source)
  try {
    & cmd /c "`"$($Config.Delphi.BinPath)\\rsvars.bat`" && msbuild DelphiLintClient$($Config.Delphi.Version.PackageVersion).dproj /p:config=`"Release`""
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
    & npx @vscode/vsce package --skip-license
  }
  finally {
    Pop-Location
  }
}


function Clear-TargetFolder {
  New-Item -ItemType Directory $TargetDir -Force | Out-Null
  Get-ChildItem -Path $TargetDir -Recurse | Remove-Item -Force -Recurse
}

function New-BatchScript([string]$Path, [string]$PSScriptPath) {
  $BatchScript = @(
    '@echo off',
    "powershell -ExecutionPolicy Bypass -File $PSScriptPath",
    'pause'
  )

  Set-Content -Path $Path -Value $BatchScript
}

function New-SetupScript([string]$Path, [string]$Version, [string]$PackageVersion) {
  $MacroContents = "`$Version = '$Version'`n`$PackageVersion = '$PackageVersion'`n"

  Copy-Item (Join-Path $PSScriptRoot TEMPLATE_install.ps1) $Path
  $Content = Get-Content -Raw $Path
  $Content = $Content -replace "##\{STARTREPLACE\}##(.|\n)*##\{ENDREPLACE\}##",$MacroContents
  Set-Content -Path $Path -Value $Content
}

function New-PackageFolder([PackagingConfig]$Config, [hashtable]$Artifacts) {
  $Path = (Join-Path $TargetDir $Config.GetPackageFolderName())
  New-Item -ItemType Directory $Path -Force

  $Artifacts.GetEnumerator() | ForEach-Object {
    Copy-Item -Path $_.Key -Destination (Join-Path $Path $_.Value)
  }

  $InstallScriptPath = (Join-Path $Path "install.ps1")
  New-SetupScript -Path $InstallScriptPath -Version $Version -PackageVersion $Config.Delphi.Version.PackageVersion
  New-BatchScript -Path (Join-Path $Path "install.bat") -PSScriptPath "install.ps1"
}

function Get-PackageFolder([string]$DelphiVersion) {
  return Join-Path $TargetDir "DelphiLint-$Version-$($_.Install.DelphiVersion)"
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
$CommonArtifacts = @{}

$PackagingConfigs = $DelphiInstalls | ForEach-Object { [PackagingConfig]::new($_) }

$Projects = @(
  @{
    "Name" = "Build client"
    "Prerequisite" = {
      Assert-ClientVersion -Version $Version
      $PackagingConfigs | ForEach-Object { Assert-Exists $_.Delphi.BinPath }
    }
    "Build" = {
      $PackagingConfigs | ForEach-Object {
        Invoke-ClientCompile -Config $_
        $_.Artifacts.Add($_.GetInputBplPath(), $_.GetOutputBplName())
        Write-Host "Built for Delphi $($_.Delphi.Version.Name) ($($_.Delphi.Version.PackageVersion))."
      }
    }
    "Postrequisite" = {
      $PackagingConfigs | ForEach-Object {
        Assert-Exists $_.GetInputBplPath()
      }
    }
  },
  @{
    "Name" = "Build server"
    "Build" = {
      Invoke-ServerCompile
      $StandaloneArtifacts.Add($ServerJar, "delphilint-server-$Version.jar");
      $CommonArtifacts.Add($ServerJar, "delphilint-server-$Version.jar");
    }
    "Postrequisite" = {
      Assert-Exists $ServerJar
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
        Assert-Exists $CompanionVsix
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
      $PackagingConfigs | ForEach-Object {
        New-PackageFolder -Config $_ -Artifacts ($CommonArtifacts + $_.Artifacts)
      }
    }
    "Postrequisite" = {
      $StandaloneArtifacts.Values | ForEach-Object {
        Assert-Exists (Join-Path $TargetDir $_)
      }

      $PackagingConfigs | ForEach-Object {
        $PackageFolder = (Join-Path $TargetDir $_.GetPackageFolderName())

        $CommonArtifacts.Values | ForEach-Object {
          Assert-Exists (Join-Path $PackageFolder $_)
        }

        $_.Artifacts.Values | ForEach-Object {
          Assert-Exists (Join-Path $PackageFolder $_)
        }
      }
    }
  },
  @{
    "Name" = "Zip build artifacts"
    "Build" = {
      $PackagingConfigs | ForEach-Object {
        $PackageFolder = Join-Path $TargetDir $_.GetPackageFolderName()
        $ZippedPackage = "${PackageFolder}.zip"
        Compress-Archive $PackageFolder -DestinationPath $ZippedPackage -Force
      }
    }
    "Postrequisite" = {
      $PackagingConfigs | ForEach-Object {
        $ZippedPackage = "$($_.GetPackageFolderName()).zip"
        Assert-Exists (Join-Path $TargetDir $ZippedPackage)
      }
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