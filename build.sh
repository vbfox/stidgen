#!/bin/bash

./paket.sh restore

mono packages/FAKE/tools/FAKE.exe "$@" --fsiargs -d:MONO "build/build.fsx"
