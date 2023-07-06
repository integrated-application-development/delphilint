#! powershell -File
mvn clean package "-Dchangelist=$(& git rev-parse --short HEAD)"