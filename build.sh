#!/bin/bash

set -exu
set +u

config=Debug

# I used to use ${1,,} for lowercase conversion, but that's bash 4 which isn't
# available on poor little macos :(
if [[ "${1}" == "release" || "${1}" == "Release" ]]; then
	config=Release
elif [[ "${1}" == "debug" || "${1}" == "Debug" ]]; then
	config=Debug
elif [[ $# -gt 0 ]]; then
	echo "Error: unrecognized cmd arg \"$1\""
	exit -1
fi
set -u

## Uncomment to tell cmake to use ifort instead of gfortran
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
	export FC=gfortran
	echo "FC = $FC"
fi

if [[ "$machine" == "Linux" ]]; then

	## ninja works, similar performance as gnu make
	#generator=(-G 'Ninja')

	export FC=$(which gfortran-14)
	echo "FC = $FC"
fi

if [[ "$machine" == "Mac" ]]; then

	# If you don't have it:  brew install gcc@14
	#
	# The gfortran included with gcc 13.2.0 seems to have a bug on macos.  Might
	# be worth trying a later 13 version when it's available

	#which gcc
	#which gfortran
	which gfortran-14

	## CMake doesn't like this hack, renamed gfortran won't pass its tests
	#mkdir ~/bin/
	#cp $(which gfortran-14) /usr/local/bin/gfortran
	#export PATH=$PATH:~/bin/
	#which gfortran

	export FC=$(which gfortran-14)
	echo "FC = $FC"

fi

#build=build
build="build/$config"

cmake -S . -B "$build" "${generator[@]}" -DCMAKE_BUILD_TYPE=$config -DCMAKE_Fortran_COMPILER=$FC
cmake --build "$build" --config $config --parallel

