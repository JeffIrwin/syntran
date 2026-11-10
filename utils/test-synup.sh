#!/usr/bin/env bash

# Test `synup` offline, against a fake release tree that curl serves with
# file://, through SYNUP_RELEASES_URL.  The "syntran" in each zip is a shell
# script that prints its version, so no real release is needed.
#
#     bash utils/test-synup.sh path/to/synup
#
# Not for Windows yet:  the stubs there would have to be exes

set -eu

if [[ $# -ne 1 ]]; then
	echo "usage: $0 path/to/synup" >&2
	exit 2
fi

synup="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

case "$(uname -s)-$(uname -m)" in
	Linux-x86_64) asset=syntran-linux.zip ;;
	Darwin-arm64) asset=syntran-macos.zip ;;
	*)
		echo "test-synup: no prebuilt syntran for this platform, skipping"
		exit 0
		;;
esac

command -v zip > /dev/null || { echo "test-synup: needs zip" >&2; exit 2; }
command -v curl > /dev/null || { echo "test-synup: needs curl" >&2; exit 2; }

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

rel="$work/rel"
export SYNUP_HOME="$work/home"
export SYNUP_RELEASES_URL="file://$rel"

# Make a release dir with a stub syntran and synup, and its SHA256SUMS
mk_release()
{
	local dir="$1" ver="$2"
	local stub="$work/stub-$ver"
	rm -rf "$dir" "$stub"
	mkdir -p "$dir" "$stub"
	printf '#!/bin/sh\necho "syntran %s"\n' "$ver" > "$stub/syntran"
	printf '#!/bin/sh\necho stub-synup-%s\n' "$ver" > "$stub/synup"
	chmod +x "$stub/syntran" "$stub/synup"
	(cd "$stub" && zip -q -j "$dir/$asset" syntran synup)
	(cd "$dir" && printf '%s  %s\n' \
		"$(shasum -a 256 "$asset" | cut -d' ' -f1)" "$asset" > SHA256SUMS)
}

nfail=0
check()
{
	# check <description> <command...>
	local desc="$1"
	shift
	if "$@" > "$work/out" 2>&1; then
		echo "  ok    $desc"
	else
		echo "  FAIL  $desc"
		sed 's/^/        /' "$work/out"
		nfail=$((nfail + 1))
	fi
}

fails()
{
	local desc="$1"
	shift
	if "$@" > "$work/out" 2>&1; then
		echo "  FAIL  $desc (should have failed)"
		sed 's/^/        /' "$work/out"
		nfail=$((nfail + 1))
	else
		echo "  ok    $desc"
	fi
}

is_ver() { [[ "$("$SYNUP_HOME/bin/syntran" --version)" == "syntran $1" ]]; }
has_out() { local want="$1"; shift; "$@" 2>&1 | grep -q -- "$want"; }

mk_release "$rel/latest/download"      9.9.9
mk_release "$rel/download/9.9.8"       9.9.8
mk_release "$rel/download/dev-latest"  10.0.0

mkdir -p "$work/bin"
cp "$synup" "$work/bin/synup"
synup="$work/bin/synup"

echo "init"
check "init succeeds"             "$synup" init
check "bin/syntran is the latest" is_ver 9.9.9
check "records the checksum"      test -s "$SYNUP_HOME/versions/9.9.9/.sha256"
check "copies synup into bin"     test -x "$SYNUP_HOME/bin/synup"

echo "install"
check "install a version"         "$synup" install 9.9.8
check "default is still stable"   is_ver 9.9.9
check "install dev"               "$synup" install dev
check "reinstall is a no-op"      has_out "up to date" "$synup" install 9.9.8
fails "bad version"               "$synup" install 1.2
fails "version that isn't there"  "$synup" install 9.9.7

echo "default"
check "default 9.9.8"             "$synup" default 9.9.8
check "bin/syntran follows"       is_ver 9.9.8
check "default dev"               "$synup" default dev
check "bin/syntran is dev"        is_ver 10.0.0
check "default stable"            "$synup" default stable
check "stable is the newest"      is_ver 9.9.9
fails "default not installed"     "$synup" default 9.9.7

echo "run, list"
check "run a version"             has_out "syntran 9.9.8" "$synup" run 9.9.8 -- --version
check "list marks the default"    has_out "9.9.9  (default: stable)" "$synup" list
fails "run a missing version"     "$synup" run 9.9.7

echo "first install of dev"
home2="$work/home2"
dev_ver() { [[ "$("$home2/bin/syntran" --version)" == "syntran $1" ]]; }
check "install dev into a fresh home"  env SYNUP_HOME="$home2" "$synup" install dev
check "dev is its own default"         dev_ver 10.0.0
rm -rf "$home2"

echo "init keeps the default"
check "default 9.9.8"             "$synup" default 9.9.8
check "init again"                "$synup" init
check "default is still 9.9.8"    is_ver 9.9.8
check "default stable"            "$synup" default stable

echo "a dangling link is replaced"
ln -s "$work/nothing" "$SYNUP_HOME/bin/.syntran.tmp"
check "default works anyway"      "$synup" default 9.9.8
check "bin/syntran follows"       is_ver 9.9.8
check "default stable (2)"        "$synup" default stable

echo "extra args"
fails "install two versions"      "$synup" install 9.9.8 9.9.9
fails "default two versions"      "$synup" default 9.9.8 9.9.9
fails "remove two versions"       "$synup" remove 9.9.8 dev
fails "list typo"                 "$synup" list --avail
fails "list extra"                "$synup" list --available more
check "nothing was removed"       test -d "$SYNUP_HOME/versions/9.9.8"

echo "specs are never paths"
mkdir -p "$SYNUP_HOME/versions/9.9.8/sub"
fails "remove x/../.."            "$synup" remove "9.9.8/../.."
fails "remove .."                 "$synup" remove ..
fails "remove empty"              "$synup" remove ""
fails "run a path"                "$synup" run "9.9.8/../9.9.8"
check "home is intact"            test -d "$SYNUP_HOME/versions/9.9.8/sub"

echo "update"
mk_release "$rel/latest/download"      9.9.10
mk_release "$rel/download/dev-latest"  10.0.1
check "update"                    "$synup" update
check "new release is stable"     is_ver 9.9.10
check "dev is refreshed"          has_out "syntran 10.0.1" "$synup" run dev
check "newer synup replaces bin"  grep -q stub-synup-9.9.10 "$SYNUP_HOME/bin/synup"
check "update again is a no-op"   has_out "up to date" "$synup" update

echo "remove"
fails "can't remove the default"  "$synup" remove 9.9.10
check "remove another"            "$synup" remove 9.9.8
check "it is gone"                test ! -e "$SYNUP_HOME/versions/9.9.8"

echo "checksums"
mk_release "$rel/latest/download" 9.9.11
echo "0000000000000000000000000000000000000000000000000000000000000000  $asset" \
	> "$rel/latest/download/SHA256SUMS"
fails "a mismatch is refused"     "$synup" install latest
check "mismatch says so"          has_out "checksum mismatch" "$synup" install latest
check "nothing was installed"     test ! -e "$SYNUP_HOME/versions/9.9.11"
check "no temp dir is left"       test ! -e "$SYNUP_HOME/versions/.tmp"
check "bin/syntran is unchanged"  is_ver 9.9.10

rm "$rel/latest/download/SHA256SUMS"
fails "no SHA256SUMS is refused"  "$synup" install latest
check "nothing was installed (2)" test ! -e "$SYNUP_HOME/versions/9.9.11"

printf '%s\n' "$(shasum -a 256 "$rel/latest/download/$asset" | cut -d' ' -f1)  other.zip" \
	> "$rel/latest/download/SHA256SUMS"
fails "no line for the asset"     "$synup" install latest

echo
if [[ $nfail -ne 0 ]]; then
	echo "test-synup: $nfail failed"
	exit 1
fi
echo "test-synup: all passed"
