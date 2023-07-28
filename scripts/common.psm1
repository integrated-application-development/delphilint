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

function Get-Version {
  $Ver = Get-Content (Join-Path $PSScriptRoot "../VERSION.txt")
  $Commit = (& git rev-parse --short HEAD)
  return $Ver -replace "#", $Commit
}

function Split-Version([string]$Version) {
  if(-not ($Version -match "^(\d)\.(\d)\.(\d)(\+dev)?")) {
    Write-Error "Invalid version format: $Version"
    Exit
  }

  $Major = $Matches[1]
  $Minor = $Matches[2]
  $Patch = $Matches[3]
  $DevVersion = ($null -ne $Matches[4])

  return [PSCustomObject]@{
    Major = $Major
    Minor = $Minor
    Patch = $Patch
    Dev = $DevVersion
    Full = $Version
  }
}