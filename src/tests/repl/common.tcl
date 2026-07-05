
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
