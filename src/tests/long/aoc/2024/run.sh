#!/usr/bin/env bash

# Go to every sub dir and run the aoc syntran script

#set -exu
set -eu

echo "starting run.sh"

dirs=$( \
	find . -type d -printf '%P\n' \
	| sort -g\
)

for dir in ${dirs[@]} ; do
	pushd "$dir" >/dev/null

	main=main.syntran
	grep 'Expect' "$main" | tr -d '\t'

	time syntran "$main"
	#syntran "$main"

	echo
	popd >/dev/null
done

echo "ending run.sh"

