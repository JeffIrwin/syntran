#!/usr/bin/env bash

set -exu

hash=$(git rev-parse --short HEAD)
header=./src/compiler.F90

sed -i \
	's/\(git_commit\s*=\s*\)".*"/\1"'$hash'"/' \
	"$header"

cat "$header"

