#!/bin/bash

./paket.sh restore || { exit $?; }

pushd src/BlackFox.Stidgen.Build/
dotnet run $@
popd
