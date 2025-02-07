#!/usr/bin/env bash

# To run this locally, you first have to download syntran-linux.zip (fka
# syntran-rocky.zip) and extract it to "docker/artifact-download/", e.g.:
#
#     mkdir -p docker/artifact-download
#     pushd docker/artifact-download
#     curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-linux.zip"
#     unzip syntran-linux.zip
#
# It github actions, the "download-artifact" action does this

set -uxe
GREEN="\033[1;32m"
RESET="\033[0m"

pushd docker
files=Dockerfile.branch-*

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

