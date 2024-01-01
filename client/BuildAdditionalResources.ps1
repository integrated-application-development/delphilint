param(
  [string]$ProjectName
)

$ResourceFile = "${ProjectName}Additional.rc"

function Invoke-Webpack {
  Write-Host "Building JS libraries..."
  Push-Location (Join-Path $PSScriptRoot jslib)
  try {
    & npm install
    & npx webpack
  }
  finally {
    Pop-Location
  }
}

function Add-Resources {
  Write-Host "Generating additional .rc file..."
  @{
    "DL_HTML_SCRIPT" = "..\jslib\dist\compiled.js"
  }.GetEnumerator() |
    ForEach-Object { "$($_.Key) RCDATA `"$($_.Value)`"" } |
    Out-File $ResourceFile -Encoding ascii
}

function Invoke-Brcc32 {
  Write-Host "Compiling additional resources..."
  & brcc32.exe $ResourceFile
}

Invoke-Webpack
Add-Resources
Invoke-Brcc32