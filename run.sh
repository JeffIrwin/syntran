#!/bin/bash

set -e
#set -xe

config="Debug"

./build.sh $config

time ./build/$config/syntran "$@"

