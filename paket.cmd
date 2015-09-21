@echo off

.paket\paket.bootstrapper.exe -s
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe %*