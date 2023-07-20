#! powershell -File

Push-Location $PSScriptRoot
try {
  mvn clean package "-Dchangelist=$(& git rev-parse --short HEAD)"
} finally {
  Pop-Location
}