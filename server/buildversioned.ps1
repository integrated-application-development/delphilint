#! powershell -File

Push-Location $PSScriptRoot
try {
  mvn clean package "-Drevision=$(& git rev-parse --short HEAD)"
  if($LASTEXITCODE) {
    throw "Maven failed with code $LASTEXITCODE"
  }
} finally {
  Pop-Location
}