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

$global:Indent = ""

function Push-Indent {
  $global:Indent = "  $Indent"
}

function Pop-Indent {
  $global:Indent = $Indent -replace "^  ",""
}

function Write-Indent {
  Write-Host "$Indent" -NoNewline
  Write-Host @Args
}

function Write-Problem([string]$Message) {
  Write-Indent -ForegroundColor Red $Message
}

function Write-Success([string]$Message) {
  Write-Indent -ForegroundColor Green $Message
}

function Write-Status(
  [ValidateSet("Success", "Question", "Problem")]
  [string]$Status
) {
  Write-Indent "[" -NoNewline
  if($Status -eq "Success") {
    Write-Host -ForegroundColor Green "/" -NoNewline
  } elseif ($Status -eq "Problem") {
    Write-Host -ForegroundColor Red "X" -NoNewline
  } else {
    Write-Host -ForegroundColor Yellow "?" -NoNewline
  }

  Write-Host "] " -NoNewline
  Write-Host @Args
}

Export-ModuleMember -Variable $Indent
Export-ModuleMember -Function *