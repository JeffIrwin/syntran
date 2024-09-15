#!/usr/bin/env bash

#set -exu
set -eu

# `fpm run long` runs all of these and further validates the results, but this
# script is nice for running in docker without fpm or other build dependencies

dirs=*

for dir in $dirs ; do
	echo "dir = $dir"
	pushd "$dir"

	#syntran main.syntran
	/mnt/c/Users/jirwi/Downloads/syntran-release-0.0.51/syntran-linux/syntran main.syntran

	popd
done

echo "Done run.sh!"

