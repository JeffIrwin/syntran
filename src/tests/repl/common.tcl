
# Shared expect helpers for the REPL/isocline PTY tests in this directory.
# Sourced by each *.exp script via:
#
#     source [file join [file dirname [info script]] common.tcl]

proc die {msg} {
	send_user "\nFAIL: $msg\n"
	exit 1
}

proc wait_prompt {} {
	expect {
		{syntran$ } {}
		timeout { die "timeout waiting for initial prompt" }
		eof     { die "unexpected eof waiting for initial prompt" }
	}
}

proc wait_hint {} {
	expect {
		{Hint} {}
		timeout { die "timeout waiting for continuation hint prompt" }
		eof     { die "unexpected eof waiting for continuation hint prompt" }
	}
}

# Resync on the next `syntran$ ` prompt after sending input AT the
# `syntran$ ` prompt itself that produces no printed output to anchor on
# first (e.g. a silent directive like `#clear` or `#hint`).  Unlike
# wait_prompt, this anchors on a literal line feed, because a bare
# `{syntran$ }` match is unsafe here: isocline redraws "syntran$ <buffer
# so far>" on every keystroke (see wait_result's comment below for the
# general redraw-noise issue), so it would match on the very first
# character typed rather than the real fresh prompt that appears once the
# directive actually finishes.  wait_result's own trailing `wait_prompt`
# call doesn't need this because nothing is sent between its anchored
# match and that call -- this proc is only for the "nothing to anchor on"
# case, i.e. right after send at the `syntran$ ` prompt
#
# The prompt text is colored (bold green ANSI escapes), so it is never
# truly adjacent to the line feed the way a plain result string is in
# wait_result -- hence `-re` with a `[^\n]*` gap instead of a plain `--`
# substring match, to skip over the escape bytes without also skipping
# over an unrelated earlier line
proc wait_prompt_resync {} {
	expect {
		-re {\n[^\n]*syntran\$ } {}
		timeout { die "timeout waiting to resync on the next prompt" }
		eof     { die "unexpected eof waiting to resync on the next prompt" }
	}
}

# Wait for the plain (non-hint) continuation prompt "> ", anchored the same
# way as wait_prompt_resync and for the same reason: this is only safe to
# call once, right after send, with nothing else matched in between
proc wait_cont_prompt {} {
	expect {
		-re {\n[^\n]*> } {}
		timeout { die "timeout waiting for continuation prompt" }
		eof     { die "unexpected eof waiting for continuation prompt" }
	}
}

# Wait for a submitted line's printed result, then resync on the following
# fresh prompt before returning.  Both parts matter:
#
#  - the result must be matched as "\n$text", anchored on a literal line
#    feed, because isocline redraws the whole "prompt + typed-so-far buffer"
#    on every keystroke using only carriage returns (never a bare '\n'); a
#    plain substring match on $text would also fire on that per-keystroke
#    repaint noise, or on the echoed input text itself if it happens to
#    contain the same digits as the result (discovered the hard way: a
#    filler statement like "999;" makes "\n999" match its own echoed input)
#
#  - resyncing on the prompt matters because isocline only holds the
#    terminal in raw/character mode for the duration of a single
#    ic_readline() call; immediately after it returns, the tty is briefly
#    back in canonical mode until the next ic_readline() call begins.  A
#    multi-character command sent in that window lands in the kernel's
#    canonical input queue instead of isocline's live editor, so waiting
#    for the next prompt to actually render (proof isocline is back and
#    reading) avoids that race
proc wait_result {text} {
	expect {
		-- "\n$text" {}
		timeout { die "timeout waiting for output: $text" }
		eof     { die "unexpected eof waiting for output: $text" }
	}
	wait_prompt
}

proc finish {} {
	send "\004"
	expect {
		eof {}
		timeout { die "timeout waiting for clean exit after ctrl-d" }
	}
	send_user "PASS\n"
}
