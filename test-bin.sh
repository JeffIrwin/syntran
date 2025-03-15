#!/usr/bin/env bash

set -uxe
GREEN="\033[1;32m"
RESET="\033[0m"

pushd docker
files=Dockerfile.bin-*

docker_args=""
#docker_args+=" --no-cache"

ntest=0

retry()
{
	# Retries a command on failure.
	# $1 - the max number of attempts
	# $2... - the command to run
	#
	# Source:
	#
	#     https://stackoverflow.com/a/35977896/4347028

	local -r -i max_attempts="$1"; shift
	local -r cmd="$@"
	local -i attempt_num=1

	until $cmd
		do
			if (( attempt_num == max_attempts )) ; then
				echo "Attempt $attempt_num failed and there are no more attempts left!"
				return 1
			else
				echo "Attempt $attempt_num failed! Trying again in $attempt_num seconds..."
				sleep $(( attempt_num++ ))
			fi
	done
}

for file in ${files[@]}; do
	echo "file = $file"
	(( ntest += 1 ))

	retry 3 sudo docker build . \
		$docker_args \
		--file "$file"

done

popd  # from docker

echo -e $GREEN"Successfully finished $ntest tests!"$RESET

