# Rewrite README.md's hidden `<!-- syntran-expect ... -->` bodies from
# freshly-captured `<group>.actual` files (written by test-readme.sh --update
# into `dirs/<dir>/<group>.actual`, already normalized).
#
# Invoked as:
#   awk -v readme_path=README.md -v dirs=/path/to/dirs \
#       -f utils/readme-update-expect.awk README.md > README.md.new
#
# The expect block for a group is always attached to that group's LAST
# `syntran-begin` occurrence in the document (matching how the blocks were
# originally authored), so a first pass over readme_path locates that line
# number per group before the real (line-by-line) rewrite pass runs.

function get_attr(str, key,    re, val) {
	re = key "=\"[^\"]*\""
	if (match(str, re)) {
		val = substr(str, RSTART, RLENGTH)
		sub("^" key "=\"", "", val)
		sub("\"$", "", val)
		return val
	}
	re = key "=[^ ]+"
	if (match(str, re)) {
		val = substr(str, RSTART, RLENGTH)
		sub("^" key "=", "", val)
		return val
	}
	return ""
}

BEGIN {
	ln = 0
	while ((getline line0 < readme_path) > 0) {
		ln++
		if (line0 ~ /^<!-- syntran-begin .* -->$/) {
			attrs = line0
			sub(/^<!-- syntran-begin /, "", attrs)
			sub(/ -->$/, "", attrs)
			m = get_attr(attrs, "mode")
			g = get_attr(attrs, "group")
			f = get_attr(attrs, "file")
			if (m != "skip" && f == "" && g != "") {
				last_line[g] = ln
			}
		}
	}
	close(readme_path)
	state = "OUTSIDE"
}

{
	line = $0
	sub(/\r$/, "", line)

	if (state == "OUTSIDE") {
		print $0
		if (line ~ /^<!-- syntran-begin .* -->$/) {
			attrs = line
			sub(/^<!-- syntran-begin /, "", attrs)
			sub(/ -->$/, "", attrs)
			mode  = get_attr(attrs, "mode")
			group = get_attr(attrs, "group")
			dir   = get_attr(attrs, "dir")
			file_ = get_attr(attrs, "file")
			if (dir == "" && group != "") dir = group
			is_src = (file_ != "")
			is_last = (mode != "skip" && !is_src && (group in last_line) && last_line[group] == NR)
			state = "WAIT_FENCE_OPEN"
		}
		next
	}

	if (state == "WAIT_FENCE_OPEN") {
		print $0
		state = "IN_FENCE"
		next
	}

	if (state == "IN_FENCE") {
		print $0
		if (line == "```") {
			state = "AFTER_FENCE"
		}
		next
	}

	if (state == "AFTER_FENCE") {
		if (is_last) {
			actualfile = dirs "/" dir "/" group ".actual"
			got = 0
			while ((getline aline < actualfile) > 0) {
				if (!got) print "<!-- syntran-expect"
				print aline
				got = 1
			}
			close(actualfile)
			if (got) print "-->"
		}
		if (line == "<!-- syntran-expect") {
			state = "SKIP_OLD_EXPECT"
			next
		}
		if (line == "<!-- syntran-end -->") {
			print $0
			state = "OUTSIDE"
			next
		}
		print "malformed README.md: expected syntran-expect or syntran-end after fence, line " NR > "/dev/stderr"
		exit 2
	}

	if (state == "SKIP_OLD_EXPECT") {
		if (line == "-->") {
			state = "WAIT_END_AFTER_EXPECT"
		}
		next
	}

	if (state == "WAIT_END_AFTER_EXPECT") {
		if (line != "<!-- syntran-end -->") {
			print "malformed README.md: expected syntran-end after syntran-expect, line " NR > "/dev/stderr"
			exit 2
		}
		print $0
		state = "OUTSIDE"
		next
	}
}
