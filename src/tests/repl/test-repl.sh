#!/usr/bin/env bash

# Drive the interactive REPL (isocline history, Ctrl+R search, continuation
# hints, arrow-key editing) through a real pty via `expect`.
#
# These features only engage when stdin is a real terminal -- see is_tty()
# in src/line_edit.f90 and its use at src/syntran.f90:130 -- so none of the
# piped-stdin or `-c` string Dockerfile one-liner tests exercise a single
# line of isocline code.  `expect`'s `spawn` allocates its own pty for the
# child process, so this works even inside a `docker build` RUN step, which
# has no controlling terminal of its own.
#
# Usage: test-repl.sh <path-to-syntran-binary>

set -ue

if [ $# -ne 1 ]; then
	echo "Usage: $0 <path-to-syntran-binary>"
	exit 1
fi

bin="$1"
dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Isolate the persistent isocline history file ($HOME/.syntran_history, see
# syntran_history_path() in src/c/isocline_wrap.c) so these tests never
# read or pollute a real user's history
export HOME
HOME="$(mktemp -d)"
export TERM=xterm

ntest=0
for exp in "$dir"/*.exp; do
	ntest=$((ntest + 1))
	echo "Running $(basename "$exp") ..."
	expect -f "$exp" "$bin"
done

echo "All $ntest REPL tests passed"
