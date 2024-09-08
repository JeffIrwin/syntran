#!/usr/bin/env bash

set -exu

hash=$(git rev-parse --short HEAD)
header=./src/compiler.F90

# `sed -i` doesn't fucking work on macos so we have to pipe to a temp file and
# then move it
sed \
	's/\(git_commit\s*=\s*\)".*"/\1"'$hash'"/' \
	"$header" > "$header.sed"
mv "$header.sed" "$header"

