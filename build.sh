#!/bin/bash

set -x

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

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
echo "machine = ${machine}"

generator=""
if [[ "$machine" == "MinGw" ]]; then
	generator=(-G 'MSYS Makefiles')
fi

if [[ "$machine" == "Mac" ]]; then

	#which gcc
	#which gfortran
	which gfortran-11

	#mkdir ~/bin/
	cp $(which gfortran-11) /usr/local/bin/gfortran
	#export PATH=$PATH:~/bin/

	which gfortran

fi

build=build
cmake -S . -B "$build" "${generator[@]}" -DCMAKE_BUILD_TYPE=$config
cmake --build "$build" --config $config

