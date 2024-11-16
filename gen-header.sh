#!/usr/bin/env bash

set -exu

hash=$(git rev-parse --short HEAD)
header=./src/compiler.F90

sed -i \
	's/\(git_commit\s*=\s*\)".*"/\1"'$hash'"/' \
	"$header"

# TODO: add build OS info?  Some linux distros build on ubuntu and others build
# on rocky now

#cat "$header"

