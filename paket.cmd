@echo off

.paket\paket.bootstrapper.exe 3.19.4 -s
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe %*