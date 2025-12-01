#! powershell -File

Push-Location $PSScriptRoot
try {
  # Set JAVA_HOME to JDK 17 if not already set or if pointing to a JRE
  if (-not $env:JAVA_HOME -or $env:JAVA_HOME -match '\\jre-') {
    $env:JAVA_HOME = 'C:\Program Files\Eclipse Adoptium\jdk-17.0.9.9-hotspot'
  }

  mvn clean package "-Drevision=$(& git rev-parse --short HEAD)"
  if($LASTEXITCODE) {
    throw "Maven failed with code $LASTEXITCODE"
  }
} finally {
  Pop-Location
}