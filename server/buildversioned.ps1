#! powershell -File

Push-Location $PSScriptRoot
try {
  mvn clean package "-Drevision=$(& git rev-parse --short HEAD)"
} finally {
  Pop-Location
}