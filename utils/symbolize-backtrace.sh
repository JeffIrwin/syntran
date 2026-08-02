#!/usr/bin/env bash
#
# Symbolize a gfortran -fbacktrace address dump from a Windows build.
#
# Why this is needed: the exe is relocated by ASLR on every run, and gfortran's
# backtrace prints only the LOW 32 BITS of each address, so the numbers it
# emits are meaningless on their own and can't be fed to addr2line directly.
#
# The fix is an anchor: `test.exe --anchor` prints the runtime address of a
# known saved symbol (aslr_anchor in test_m) before running.  The whole image
# relocates by a single delta, so for any frame in the exe
#
#     link_addr = link_addr_of_anchor + (frame_low32 - anchor_low32)
#
# with the difference taken as a signed 32-bit value.  Frames belonging to
# system DLLs (ucrtbase, KERNEL32, ...) land far outside the image and are
# reported as such rather than mis-symbolized.
#
# Usage:
#   test.exe --anchor > crash.log 2>&1        # loop until it crashes
#   utils/symbolize-backtrace.sh <exe> crash.log
#
# Requires binutils (nm, addr2line).

set -u

EXE="${1:?usage: symbolize-backtrace.sh <exe> <crash-log>}"
LOG="${2:?usage: symbolize-backtrace.sh <exe> <crash-log>}"

ANCHOR_SYM=__test_m_MOD_aslr_anchor

anchor_link=$(nm "$EXE" | grep -i " $ANCHOR_SYM\$" | awk '{print $1}')
if [[ -z "$anchor_link" ]]; then
	echo "error: symbol $ANCHOR_SYM not found in $EXE" >&2
	echo "       (is this a test.exe built from src/tests/test.f90?)" >&2
	exit 1
fi

anchor_rt=$(grep -oiE 'ASLR_ANCHOR=0x *[0-9A-F]+' "$LOG" | head -1 | grep -oiE '[0-9A-F]+$')
if [[ -z "$anchor_rt" ]]; then
	echo "error: no ASLR_ANCHOR line in $LOG -- rerun the exe with --anchor" >&2
	exit 1
fi

# Image size, to tell exe frames from system-DLL frames
img_end=$(nm "$EXE" | awk '{print $1}' | grep -iE '^[0-9a-f]+$' | sort | tail -1)

anchor_link_d=$((16#$anchor_link))
anchor_lo=$(( 16#$anchor_rt & 0xFFFFFFFF ))
img_end_d=$((16#$img_end))

echo "anchor: link=0x$anchor_link runtime_low32=0x$(printf '%x' "$anchor_lo")"
echo

grep -oE '^#[0-9]+ +0x[0-9a-f]+' "$LOG" | while read -r frame addr; do
	lo=$((addr))

	# Signed 32-bit difference from the anchor
	d=$(( lo - anchor_lo ))
	if   (( d >  2147483647 )); then d=$(( d - 4294967296 ))
	elif (( d < -2147483648 )); then d=$(( d + 4294967296 ))
	fi

	target=$(( anchor_link_d + d ))

	if (( target < 0x140000000 || target > img_end_d + 0x100000 )); then
		printf '%-5s 0x%08x   [outside image -- system DLL frame]\n' "$frame" "$lo"
		continue
	fi

	sym=$(addr2line -e "$EXE" -f -C -i "$(printf '0x%x' "$target")" 2>/dev/null | paste -sd' | ' -)
	printf '%-5s 0x%08x -> 0x%x  %s\n' "$frame" "$lo" "$target" "$sym"
done
