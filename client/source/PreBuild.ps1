$IncFile = Join-Path $PSScriptRoot "dlversion.inc"
$IncFileCopy = Join-Path $PSScriptRoot "dlversion.inc.orig"
Copy-Item $IncFile $IncFileCopy -Force

& git rev-parse --is-inside-work-tree | Out-Null
if($?) {
  $Commit = (& git rev-parse --short HEAD)

  $IncContents = Get-Content -Raw $IncFile
  Set-Content -Path $IncFile -Value ($IncContents -replace "{START_COMMIT}[^{]*{END_COMMIT}","'$Commit'")
  Write-Output "Commit macro expanded."
} else {
  Write-Output "Not in a Git repository - macro not expanded."
}
