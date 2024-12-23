param(
  [string]$ProjectName
)

$AssetsDir = Join-Path $PSScriptRoot "assets\resources"
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

function Get-Hash([string]$Text) {
  $Stream = [System.IO.MemoryStream]::new([byte[]][char[]]$Text)
  Get-FileHash -InputStream $Stream -Algorithm SHA256
}

function Get-AssetResources {
  $RcData = @{}

  Push-Location $AssetsDir
  try {
    Get-ChildItem . -Recurse -File | ForEach-Object {
      $RelativePath = ((Resolve-Path -Relative $_.FullName) -replace "^\.\\").ToUpper()
      $Key = "DL_ASSET_" + (Get-Hash $RelativePath).Hash
      $RcData[$Key] = $_.FullName
    }
  }
  finally {
    Pop-Location
  }

  $RcData
}

function New-Rc([hashtable]$RcData) {
  Write-Host "Generating additional .rc file..."
  $RcData.GetEnumerator() | ForEach-Object {
    "$($_.Key) RCDATA `"$($_.Value)`""
  } | Out-File $ResourceFile -Encoding ascii
}

function Invoke-Brcc32 {
  Write-Host "Compiling additional resources..."
  & brcc32.exe $ResourceFile
}

Invoke-Webpack
New-Rc (@{
  "DL_GENERATED_JS" = Join-Path $PSScriptRoot "jslib\dist\compiled.js"
} + (Get-AssetResources))
Invoke-Brcc32