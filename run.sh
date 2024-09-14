#!/bin/bash

set -e
#set -xe

config="Debug"

./build.sh $config

if [[ -x "$(command -v rlwrap)" ]]; then
	time rlwrap ./build/$config/syntran $*
else
	time ./build/$config/syntran $*
fi

