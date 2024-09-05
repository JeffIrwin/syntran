#!/usr/bin/env bash

set -uxe
GREEN="\033[1;32m"
RESET="\033[0m"

pushd docker
files=Dockerfile.bin-*

docker_args=""
#docker_args+=" --no-cache"

ntest=0

for file in ${files[@]}; do
	echo "file = $file"
	(( ntest += 1 ))

	sudo docker build . \
		$docker_args \
		--file "$file"

done

popd  # from docker

echo -e $GREEN"Successfully finished $ntest tests!"$RESET

