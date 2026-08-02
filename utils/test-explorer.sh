#!/usr/bin/env bash
set -exu

# TODO: take a 'profile' arg (and other args?) here, and then DRY up this script
# with its copies in main.yml
#
# Be careful not to trigger a re-compile of release mode in ci/cd due to 1
# differring arg.  Maybe just pass along $*

dir_="src/tests/test-src/explorer/"

for f in $dir_/*.syntran ; do
	echo $f
	fpm \
		run \
		--profile debug \
		-- \
		-q \
		$f \
		> ${f%.syntran}.out

	diff ${f%.syntran}.out ${f%.syntran}.exp

done

