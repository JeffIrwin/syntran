#!/usr/bin/env bash

# Set up a static LLVM link for the Windows release binary (the equivalent of
# /opt/llvm-static in Dockerfile.rocky).  Run it in MSYS2's ucrt64 environment, or
# anywhere with MSYS2's llvm-config on the PATH.  It writes two files into the dir $1,
# which defaults to build/llvm-static:
#
#   libLLVM.a    A GNU ld linker script that groups the LLVM component archives
#                that src/llvm_c.f90 needs, with zlib and zstd.  fpm's `llvm-blas`
#                feature links `-lLLVM`, so this stands in for a monolithic libLLVM.a
#   syntran.def  The exports of syntran.exe.  src/c/llvm_shim.c finds some of LLVM's
#                fns with GetProcAddress(), because which ones exist depends on the
#                version of LLVM.  A static LLVM doesn't export its symbols, so these
#                are exported on purpose
#
# Then build with `-L<dir> -Wl,<dir>/syntran.def`, and without MSYS2 on the PATH
#
# Don't put MSYS2's lib dir itself on -L, because its libgfortran.a would shadow the
# toolchain's.  So the archives are named by their full paths

set -eu

out="${1:-build/llvm-static}"
llvm_config="${LLVM_CONFIG:-llvm-config}"
llvm_nm="${LLVM_NM:-llvm-nm}"
shim="$(dirname "$0")/../src/c/llvm_shim.c"

mkdir -p "$out"

libdir="$("$llvm_config" --libdir)"
libs="$("$llvm_config" --link-static --libfiles core analysis passes target native orcjit bitreader bitwriter)"

# zlib and zstd by their static archives, and no xml2 (only for merging Windows
# manifests, which syntran doesn't use).  The rest, such as -lpsapi, are system
# libs from the toolchain
sys=
for l in $("$llvm_config" --link-static --system-libs); do
	case "$l" in
		-lz)             sys="$sys $libdir/libz.a";;
		-lzstd|-lzstd.*) sys="$sys $libdir/libzstd.a";;
		-lxml2)          ;;
		*)               sys="$sys $l";;
	esac
done

# The C++ runtime comes from the toolchain
echo "GROUP ( $libs$sys -lstdc++ )" > "$out/libLLVM.a"

# Only the names that this LLVM has: ld refuses to export a symbol that is undefined,
# and an older or newer LLVM has only one of the two fns that sy_jit_wrap() looks for
names="$(grep -o 'sy_dlsym("[A-Za-z0-9_]*")' "$shim" | sed 's/.*("\(.*\)")/\1/')"
# llvm-nm comes with LLVM, whereas a plain nm may not be installed
have="$("$llvm_nm" -g --defined-only $libs | awk '{print $NF}')"
{
	echo EXPORTS
	for n in $names; do
		if printf '%s\n' "$have" | grep -qx "$n"; then
			echo "	$n"
		else
			echo "$n is not in this LLVM, so not exporting it" >&2
		fi
	done
} > "$out/syntran.def"

echo "--- $out/libLLVM.a"
cat "$out/libLLVM.a"
echo "--- $out/syntran.def"
cat "$out/syntran.def"

# As in Dockerfile.rocky, make sure that grep found the right thing.  One of the two
# fns is enough for llvm_shim.c, which looks for either, so check for at least one
[ "$(grep -c LLVM "$out/syntran.def")" -ge 1 ]
