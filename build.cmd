@echo off

paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

"packages/FAKE/tools/FAKE.exe" --removeLegacyFakeWarning "build/build.fsx" %*
