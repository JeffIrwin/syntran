#!/usr/bin/env bash

set -exu

# Samples are not covered by tests, so these are not guaranteed to run.  Some of
# theme are also computationally expensive and take a while to run, especially
# the wave solvers.  A few (e.g. bad-syntax.syntran) are deliberately invalid,
# and others use bare top-level expression statements that are only legal in
# REPL mode, not file mode, so `fpm run -- "$file"` errors on them too.
#
# syntran now exits nonzero on parse/runtime errors, so each sample's `fpm
# run` is allowed to fail without aborting this script; failures are
# collected and reported at the end instead

file_list=$(ls samples/*.syntran)
#file_list=$(find samples/ -name "*.syntran")

failed=()

for file in ${file_list[@]} ; do
	#echo $file
	fpm run -- "$file" || failed+=("$file")
done

if [[ ${#failed[@]} -gt 0 ]]; then
	echo "The following samples did not run cleanly:"
	printf '    %s\n' "${failed[@]}"
fi

