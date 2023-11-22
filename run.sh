#!/bin/bash

set -e
#set -xe

./build.sh

if [[ -x "$(command -v rlwrap)" ]]; then
	time rlwrap ./build/syntran $*
else
	time ./build/syntran $*
fi

