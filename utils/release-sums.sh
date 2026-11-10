#!/usr/bin/env bash

# Write SHA256SUMS for release assets, in the format of `sha256sum`, which is
# what `synup` checks every download against.  Upload it next to the zips of
# the release, and upload it last.  See the "Toolchain manager" section of
# CLAUDE.md
#
#     bash utils/release-sums.sh syntran-linux.zip syntran-macos.zip syntran-windows.zip
#
# SHA256SUMS goes in the dir of the first asset, and the names in it are
# relative to that dir

set -eu

if [[ $# -eq 0 ]]; then
	echo "usage: $0 <asset>..." >&2
	exit 2
fi

if command -v sha256sum > /dev/null; then
	sha() { sha256sum "$1" | cut -d' ' -f1; }
else
	sha() { shasum -a 256 "$1" | cut -d' ' -f1; }
fi

dir="$(dirname "$1")"
out="$dir/SHA256SUMS"
: > "$out"

for f in "$@"; do
	if [[ "$(dirname "$f")" != "$dir" ]]; then
		echo "release-sums: $f is not in $dir" >&2
		exit 1
	fi
	printf '%s  %s\n' "$(sha "$f")" "$(basename "$f")" >> "$out"
done

cat "$out"
