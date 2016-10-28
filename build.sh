#!/bin/bash

function dotnet { if test "$OS" = "Windows_NT"; then $@; else mono $@; fi }

dotnet paket.exe restore || { exit $?; }
dotnet packages/FAKE/tools/FAKE.exe $@ --fsiargs build/build.fsx
