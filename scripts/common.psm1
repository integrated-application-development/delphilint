function Test-PromptConfirmed([string]$Response) {
  return $Response -imatch "^y"
}

function Wait-Prompt([string]$Message, [switch]$ExitOnNo) {
  $Done = $false
  $Response = "n"
  while(-not $Done) {
    $Response = Read-Host -Prompt "$Message (y/n)"
    $Done = Test-PromptConfirmed $Response
    if ($ExitOnNo -and -not ($Done)) {
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
  if (-not ($Version -match "^(\d)\.(\d)\.(\d)(\+dev)?")) {
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

function Write-Title([string]$Title) {
  $NumSpaces = 80 - $Title.Length
  $LeftPad = $NumSpaces / 2 + $Title.Length
  Write-Host -ForegroundColor Cyan $Title.PadLeft($LeftPad, "=").PadRight(80, "=");
}

function Write-Header([string]$Title) {
  $NumSpaces = 80 - $Title.Length
  $LeftPad = $NumSpaces / 2 + $Title.Length
  Write-Host -ForegroundColor Cyan $Title.PadLeft($LeftPad, "-").PadRight(80, "-");
}

function Write-Problem([string]$Message) {
  Write-Host -ForegroundColor Red $Message
}

function Write-Success([string]$Message) {
  Write-Host -ForegroundColor Green $Message
}

function Write-Status(
  [ValidateSet("Success", "Question", "Problem")]
  [string]$Status
) {
  Write-Host "[" -NoNewline
  if ($Status -eq "Success") {
    Write-Host -ForegroundColor Green "/" -NoNewline
  } elseif ($Status -eq "Problem") {
    Write-Host -ForegroundColor Red "X" -NoNewline
  } else {
    Write-Host -ForegroundColor Yellow "?" -NoNewline
  }

  Write-Host "] " -NoNewline
  Write-Host @Args
}

function Resolve-PathToRoot([string]$Path) {
  Push-Location $PSScriptRoot\..
  try {
    return (Resolve-Path -Relative $Path) -replace "^\.[\/\\]",""
  } finally {
    Pop-Location
  }
}

Export-ModuleMember -Function *