#!/usr/bin/env bash
set -exu

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

