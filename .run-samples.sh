#!/usr/bin/env bash

set -exu

# Samples are not covered by tests, so these are not guaranteed to run.  Some of
# theme are also computationally expensive and take a while to run, especially
# the wave solvers
#
# Also, syntran doesn't return any error code on parse errors, so this script
# doesn't catch anything

file_list=$(ls samples/*.syntran)
#file_list=$(find samples/ -name "*.syntran")

for file in ${file_list[@]} ; do
	#echo $file
	fpm run -- "$file"
done

