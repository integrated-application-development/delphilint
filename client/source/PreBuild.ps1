function Expand-CommitMacro {
  Write-Host "Expanding commit macro..."
  $IncFile = Join-Path $PSScriptRoot "dlversion.inc"
  $IncFileCopy = Join-Path $PSScriptRoot "dlversion.inc.orig"
  Copy-Item $IncFile $IncFileCopy -Force

  & git rev-parse --is-inside-work-tree | Out-Null
  if($?) {
    $Commit = (& git rev-parse --short HEAD)

    $IncContents = Get-Content -Raw $IncFile
    Set-Content -Path $IncFile -Value ($IncContents -replace "{COMMIT}[^{]*{\/COMMIT}","'$Commit'") -NoNewline
    Write-Output "Commit macro expanded."
  } else {
    Write-Output "Not in a Git repository - macro not expanded."
  }
}

& ..\BuildAdditionalResources.ps1 "DelphiLintClient"
Expand-CommitMacro