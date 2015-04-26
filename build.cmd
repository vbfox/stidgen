@echo off

.paket\paket.bootstrapper.exe
.paket\paket.exe install
packages\FAKE\tools\FAKE.exe build.fsx %*