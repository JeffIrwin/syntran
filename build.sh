#!/bin/bash

config=Debug
if [[ "${1,,}" == "release" ]]; then
	config=Release
elif [[ "${1,,}" == "debug" ]]; then
	config=Debug
elif [[ $# -gt 0 ]]; then
	echo "Error: unrecognized cmd arg \"$1\""
	exit -1
fi

## Uncomment to tell cmake to use ifort instead of gfortran (untested)
#export SYNTRAN_INTEL=true

build=build
cmake -S . -B "$build" -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=$config
cmake --build "$build" --config $config

