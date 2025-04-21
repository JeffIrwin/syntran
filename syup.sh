#!/usr/bin/env bash

# This script updates syntran on Linux by downloading the latest github release
# binary

#****************

set -eu

# Make a temp dir for download and then cleanup afterwards

dir_=$(mktemp -d)
pushd "$dir_"

curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-linux.zip"

unzip syntran-linux.zip
rm syntran-linux.zip

chmod +x ./syntran

#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.

#./syntran -h
#./syntran --version

# TODO: check for existing syntran installation and use that as dest dir.
# Otherwise, require dest arg?
bin_dest="$HOME/.local/bin/"

mv * "$bin_dest"

syntran --version

popd
rm -rf "$dir_"

