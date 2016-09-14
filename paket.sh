#!/usr/bin/env bash

mono .paket/paket.bootstrapper.exe -s
exit_code=$?
if [ $exit_code -ne 0 ]; then
  exit $exit_code
fi

mono .paket/paket.exe $@