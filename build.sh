#!/bin/bash

./paket.sh restore || { exit $?; }

dotnet run --project src/BlackFox.Stidgen.Build/BlackFox.Stidgen.Build.fsproj -- $@
