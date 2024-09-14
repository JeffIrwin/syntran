#!/bin/bash

set -x

config=Debug
if [[ "${1}" == "release" || "${1}" == "Release" ]]; then
	config=Release
elif [[ "${1}" == "debug" || "${1}" == "Debug" ]]; then
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

	# If you don't have it:  brew install gcc@12
	#
	# The gfortran included with gcc 13.2.0 seems to have a bug on macos.  Might
	# be worth trying a later 13 version when it's available

	#which gcc
	#which gfortran
	which gfortran-12

	## CMake doesn't like this hack, renamed gfortran won't pass its tests
	#mkdir ~/bin/
	#cp $(which gfortran-12) /usr/local/bin/gfortran
	#export PATH=$PATH:~/bin/
	#which gfortran

	export FC=$(which gfortran-12)
	echo "FC = $FC"

fi

#build=build
build="build/$config"

cmake -S . -B "$build" "${generator[@]}" -DCMAKE_BUILD_TYPE=$config
cmake --build "$build" --config $config

