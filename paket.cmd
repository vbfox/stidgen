@echo off

.paket\paket.bootstrapper.exe 3.20.2 -s
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe %*