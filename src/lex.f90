
!===============================================================================

module syntran__lex_m

	use syntran__types_m
	use syntran__utils_m

	implicit none

	type lexer_t

		! The lexer takes a string of characters and divides it into into tokens
		! or words

		integer :: pos

		type(string_vector_t) :: diagnostics

		! Lexer only lexes 1 (include) file at a time, so it only has 1 context
		type(text_context_t) :: context

		integer :: unit_  ! translation unit (src file) index for error diagnostic context

		! Both the lexer and the parser have current() and lex()/next() member
		! fns.  current_char() returns a char, while the others return syntax
		! tokens
		contains
			procedure :: lex => lex_wrap, peek => peek_char, &
				current => current_char, lookahead => lookahead_char, &
				read_single_line_comment, get_text

	end type lexer_t

!===============================================================================

contains

!===============================================================================

subroutine lex_impl(lexer, token)

	class(lexer_t) :: lexer

	type(syntax_token_t), intent(out) :: token

	!********

	character(len = :), allocatable :: text, text_strip, suffix

	integer :: kind, type, start, end_, io, suffix_start, suffix_end
	integer(kind = 4) :: i32
	integer(kind = 8) :: i64

	integer :: n_hashes, j

	logical :: float, float32, float64, terminated

	real(kind = 4) :: f32
	real(kind = 8) :: f64

	type(char_vector_t) :: char_vec
	type(text_span_t) :: span
	type(value_t) :: val

	!print *, 'lexer%unit_ = ', lexer%unit_

	! A float literal that fails to `read()`, e.g. `1.234e+1e+2`, still makes a
	! token with a diagnostic, and a failed read leaves its target undefined.
	! Without this, the token has a different garbage value on every run
	f32 = 0.0
	f64 = 0.0d0

	if (lexer%pos > len(lexer%context%text)) then

		call new_token(token, eof_token, lexer%pos, null_char)

		return

	end if

	start = lexer%pos

	!********

	if (lexer%get_text(0,2) == "0x" .and. has_radix_digit(lexer, "x")) then
		! Hex literal
		lexer%pos = lexer%pos + 2  ! skip "0x"

		do while (is_hex_under(lexer%current()))
			! Lex literal body
			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		call lex_type_suffix(lexer, "hex", .false., type, suffix, &
			suffix_start, suffix_end)

		text = lexer%context%text(start: end_ - 1)
		text_strip = rm_leading_zeros(rm_char(text(3:), "_"))

		if (type == i32_type) then

			read(text_strip, "(z12)", iostat = io) i32
			if (io == exit_success) then
				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)
			else
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_hex32( &
					lexer%context, span, text))
			end if

		else if (type == i64_type) then

			read(text_strip, "(z20)", iostat = io) i64
			if (io == exit_success) then
				val   = new_literal_value(i64_type, i64 = i64)
				call new_token(token, i64_token, start, text, val)
			else
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_hex64( &
					lexer%context, span, text))
			end if

		else

			! 8 chars should be sufficient. pad by an extra 4 for safety
			read(text_strip, "(z12)", iostat = io) i32
			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)

			else

				read(text_strip, "(z20)", iostat = io) i64  ! 16 chars should suffice
				if (io == exit_success) then

					!print *, "i64 = ", i64
					val   = new_literal_value(i64_type, i64 = i64)
					call new_token(token, i64_token, start, text, val)

				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_hex64( &
						lexer%context, span, text))
				end if
			end if
		end if

		return
	end if  ! "0x"

	!********

	if (lexer%get_text(0,2) == "0o" .and. has_radix_digit(lexer, "o")) then
		! Octal literal
		lexer%pos = lexer%pos + 2

		do while (is_oct_under(lexer%current()))
			! Lex literal body
			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		call lex_type_suffix(lexer, "octal", .false., type, suffix, &
			suffix_start, suffix_end)

		text = lexer%context%text(start: end_ - 1)
		text_strip = rm_leading_zeros(rm_char(text(3:), "_"))

		if (type == i32_type) then

			read(text_strip, "(o15)", iostat = io) i32
			if (io == exit_success) then
				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)
			else
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_oct32( &
					lexer%context, span, text))
			end if

		else if (type == i64_type) then

			read(text_strip, "(o26)", iostat = io) i64
			if (io == exit_success) then
				val   = new_literal_value(i64_type, i64 = i64)
				call new_token(token, i64_token, start, text, val)
			else
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_oct64( &
					lexer%context, span, text))
			end if

		else

			! An i32 has at most 11 octal digits (0o37777777777 == -1); pad by 4
			read(text_strip, "(o15)", iostat = io) i32
			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)

			else

				! An i64 has at most 22 octal digits; pad by 4
				read(text_strip, "(o26)", iostat = io) i64
				if (io == exit_success) then

					val   = new_literal_value(i64_type, i64 = i64)
					call new_token(token, i64_token, start, text, val)

				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_oct64( &
						lexer%context, span, text))
				end if
			end if
		end if

		return
	end if  ! "0o"

	!********

	if (lexer%get_text(0,2) == "0b" .and. has_radix_digit(lexer, "b")) then
		! Binary literal
		lexer%pos = lexer%pos + 2

		do while (is_bin_under(lexer%current()))
			! Lex literal body
			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		call lex_type_suffix(lexer, "binary", .false., type, suffix, &
			suffix_start, suffix_end)

		text = lexer%context%text(start: end_ - 1)
		text_strip = rm_leading_zeros(rm_char(text(3:), "_"))

		if (type == i32_type) then

			read(text_strip, "(b36)", iostat = io) i32
			if (io == exit_success) then
				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)
			else
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_bin32( &
					lexer%context, span, text))
			end if

		else if (type == i64_type) then

			read(text_strip, "(b68)", iostat = io) i64
			if (io == exit_success) then
				val   = new_literal_value(i64_type, i64 = i64)
				call new_token(token, i64_token, start, text, val)
			else
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_bin64( &
					lexer%context, span, text))
			end if

		else

			! 32 chars should be sufficient. pad by an extra 4 for safety
			read(text_strip, "(b36)", iostat = io) i32
			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)

			else

				read(text_strip, "(b68)", iostat = io) i64  ! 64 chars should suffice
				if (io == exit_success) then

					val   = new_literal_value(i64_type, i64 = i64)
					call new_token(token, i64_token, start, text, val)

				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_bin64( &
						lexer%context, span, text))
				end if
			end if
		end if

		return
	end if  ! "0b"

	!********

	if (is_digit(lexer%current())) then
		! Numeric decimal integer or float.  It must start with a digit, so that
		! `_x` is an identifier

		float = .false.

		do while (is_float_under(lexer%current()))

			if (is_sign(lexer%current()) .and. .not. &
				is_expo(lexer%peek(-1))) exit

			float = float .or. .not. is_digit_under(lexer%current())

			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		! Legacy float-32 "f" type suffix.  Might break compat and remove?
		float32 = .false.
		float64 = .false.
		if (float .and. lexer%current() == "f") then
			float32 = .true.
			lexer%pos = lexer%pos + 1
		else if (float) then
			float64 = .true.
		end if

		! Preferred apostrophe type suffix
		call lex_type_suffix(lexer, "decimal", .true., type, suffix, &
			suffix_start, suffix_end)

		if (float .and. (type == i32_type .or. type == i64_type)) then
			! An integer suffix on a literal that's already shaped like a float
			! (has a `.` or exponent) is a different mistake than an out-of-range
			! integer, so it gets its own diagnostic instead of falling through
			! to err_bad_i32()/err_bad_i64() below
			text = lexer%context%text(start: suffix_end - 1)
			call new_token(token, bad_token, lexer%pos, text)
			span = new_span(start, len(text))
			call lexer%diagnostics%push(err_float_int_suffix( &
				lexer%context, span, suffix, lexer%context%text(start: end_ - 1)))
			return
		end if

		text = lexer%context%text(start: end_ - 1)
		text_strip = rm_char(text, "_")

		!print *, 'float text = ', quote(text)

		if (type /= unknown_type) then
			! Handle explicit apostrophe type ascription suffixes

			select case (type)
			case (f32_type)

				read(text_strip, *, iostat = io) f32
				if (io == exit_success) then
					val   = new_literal_value(f32_type, f32 = f32)
					call new_token(token, f32_token, start, text, val)
				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_f32( &
						lexer%context, span, text))
				end if

			case (f64_type)

				read(text_strip, *, iostat = io) f64
				if (io == exit_success) then
					val   = new_literal_value(f64_type, f64 = f64)
					call new_token(token, f64_token, start, text, val)
				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_f64( &
						lexer%context, span, text))
				end if

			case (i32_type)

				read(text_strip, *, iostat = io) i32
				if (io == exit_success) then
					val   = new_literal_value(i32_type, i32 = i32)
					call new_token(token, i32_token, start, text, val)
				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_i32( &
						lexer%context, span, text))
				end if

			case (i64_type)

				read(text_strip, *, iostat = io) i64
				if (io == exit_success) then
					val   = new_literal_value(i64_type, i64 = i64)
					call new_token(token, i64_token, start, text, val)
				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_i64( &
						lexer%context, span, text))
				end if

			end select

			return
		end if

		! Handle inferred type literals (and legacy "f" f32 suffix).  As noted
		! above, I could clean this code up more if I broke compat and removed
		! legacy f suffix

		if (float32) then

			! This io check can catch problems like `1.234e+1e+2` which look
			! like a float but aren't correctly formatted
			read(text_strip, *, iostat = io) f32
			if (io /= exit_success) then
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_f32( &
					lexer%context, span, text))
			end if

			val   = new_literal_value(f32_type, f32 = f32)
			call new_token(token, f32_token, start, text, val)

		else if (float64) then

			! This io check can catch problems like `1.234e+1e+2` which look
			! like a float but aren't correctly formatted
			read(text_strip, *, iostat = io) f64
			if (io /= exit_success) then
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_f64( &
					lexer%context, span, text))
			end if

			val   = new_literal_value(f64_type, f64 = f64)
			call new_token(token, f64_token, start, text, val)

		else

			read(text_strip, *, iostat = io) i32

			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				call new_token(token, i32_token, start, text, val)

			else

				read(text_strip, *, iostat = io) i64

				if (io == exit_success) then

					val   = new_literal_value(i64_type, i64 = i64)
					call new_token(token, i64_token, start, text, val)

				else
					call new_token(token, bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_i64( &
						lexer%context, span, text))
				end if

			end if
		end if

		return

	end if

	! Raw string literal: r"...", r#"..."#, r##"..."##, etc.
	! Count the '#' chars after 'r'; the same count must precede the closing '"'.
	! Content is taken verbatim — no doubled-quote escape processing.
	if (lexer%current() == 'r') then

		n_hashes = 0
		do while (lexer%peek(1 + n_hashes) == '#')
			n_hashes = n_hashes + 1
		end do

		if (lexer%peek(1 + n_hashes) == '"') then

			! Advance past 'r', the n_hashes '#' chars, and the opening '"'
			lexer%pos = lexer%pos + n_hashes + 2

			char_vec = new_char_vector()
			terminated = .false.
			do

				if (lexer%pos > len(lexer%context%text)) exit

				if (lexer%current() == '"') then
					! Check whether the next n_hashes chars are all '#'
					terminated = .true.
					do j = 1, n_hashes
						if (lexer%peek(j) /= '#') then
							terminated = .false.
							exit
						end if
					end do
					if (terminated) then
						! Advance past the closing '"' and its n_hashes '#' chars
						lexer%pos = lexer%pos + n_hashes + 1
						exit
					end if
				end if

				call char_vec%push(lexer%current())
				lexer%pos = lexer%pos + 1

			end do

			text = lexer%context%text(start: lexer%pos-1)

			if (.not. terminated) then
				call new_token(token, bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push( &
					err_unterminated_raw_str(lexer%context, &
					span, text))
				return
			end if

			val   = new_literal_value(str_type, str_ = char_vec%v( 1: char_vec%len_ ))
			call new_token(token, str_token, start, text, val)

			return

		end if

	end if

	if (lexer%current() == '"') then

		! Skip the current quote
		lexer%pos = lexer%pos + 1

		char_vec = new_char_vector()
		terminated = .false.
		do

			! Check before reading, so a quote at the very end of the input
			! doesn't push (and then slice) a char past the end
			if (lexer%pos > len(lexer%context%text)) exit

			! Make a quote literal by doubling it
			if (lexer%current() == '"') then
				lexer%pos = lexer%pos + 1
				if (lexer%pos > len(lexer%context%text)) then
					terminated = .true.
					exit
				end if
				if (lexer%current() /= '"') then
					terminated = .true.
					exit
				end if
			end if

			call char_vec%push(lexer%current())
			lexer%pos = lexer%pos + 1

		end do

		text  = lexer%context%text(start: lexer%pos-1)

		if (.not. terminated) then
			call new_token(token, bad_token, lexer%pos, text)
			span = new_span(start, len(text))
			call lexer%diagnostics%push( &
				err_unterminated_str(lexer%context, &
				span, text))
			return
		end if

		val   = new_literal_value(str_type, str_ = char_vec%v( 1: char_vec%len_ ))
		call new_token(token, str_token, start, text, val)

		return

	end if

	if (is_whitespace(lexer%current())) then

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%context%text(start: lexer%pos-1)

		call new_token(token, whitespace_token, start, text)
		return

	end if

	if (is_letter(lexer%current()) .or. lexer%current() == '_') then

		do while (is_alphanum(lexer%current()) .or. lexer%current() == '_')
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%context%text(start: lexer%pos-1)

		! This block handles booleans as well as identifiers, but note that it
		! does not set the value here like the is_digit_under() case for numbers
		! above.  The boolean value is not set until parse_primary_expr().

		kind = get_keyword_kind(text)
		call new_token(token, kind, start, text)
		return

	end if

	if (lexer%get_text(0,2) == "//" .or. &
		(lexer%pos == 1 .and. lexer%get_text(0,2) == "#!")) then

		! A `//` comment, or a special shebang `#!` case at very beginning of
		! file.  Ignore the rest of the line.  The line feed after it is the next
		! token, a whitespace
		call lexer%read_single_line_comment()

		text = lexer%context%text(start: lexer%pos-1)
		call new_token(token, comment_token, start, text)
		return

	end if

	select case (lexer%current())

		case ("+")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, plus_equals_token, lexer%pos, "+=")
			else
				call new_token(token, plus_token, lexer%pos, lexer%current())
			end if

		case ("-")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, minus_equals_token, lexer%pos, "-=");
			else
				call new_token(token, minus_token, lexer%pos, lexer%current())
			end if

		case ("*")

			if (lexer%lookahead() == "*") then

				if (lexer%peek(2) == "=") then
					!print *, '**='
					lexer%pos = lexer%pos + 2
					call new_token(token, double_star_equals_token, lexer%pos, "**=")

				else
					lexer%pos = lexer%pos + 1
					call new_token(token, double_star_token, lexer%pos, "**")

				end if

			else if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, star_equals_token, lexer%pos, "*=")

			else
				call new_token(token, star_token, lexer%pos, lexer%current())

			end if

		case ("/")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, slash_equals_token, lexer%pos, "/=")

			else
				call new_token(token, slash_token, lexer%pos, lexer%current())
			end if

		case ("%")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, percent_equals_token, lexer%pos, "%=");
			else
				call new_token(token, percent_token, lexer%pos, lexer%current())
			end if

		case ("@")
			call new_token(token, matmul_token, lexer%pos, lexer%current())

		case ("(")
			call new_token(token, lparen_token, lexer%pos, lexer%current())

		case (")")
			call new_token(token, rparen_token, lexer%pos, lexer%current())

		case ("{")
			call new_token(token, lbrace_token, lexer%pos, lexer%current())

		case ("}")
			call new_token(token, rbrace_token, lexer%pos, lexer%current())

		case ("[")
			call new_token(token, lbracket_token, lexer%pos, lexer%current())

		case ("]")
			call new_token(token, rbracket_token, lexer%pos, lexer%current())

		case (":")
			if (lexer%lookahead() == ":") then
				lexer%pos = lexer%pos + 1
				call new_token(token, double_colon_token, lexer%pos, "::")
			else
				call new_token(token, colon_token, lexer%pos, lexer%current())
			end if

		case (";")
			call new_token(token, semicolon_token, lexer%pos, lexer%current())

		case (",")
			call new_token(token, comma_token, lexer%pos, lexer%current())

		case (".")
			call new_token(token, dot_token, lexer%pos, lexer%current())

		case ("#")
			call new_token(token, hash_token, lexer%pos, lexer%current())

		case ("?")
			call new_token(token, question_token, lexer%pos, lexer%current())

		case ("=")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, double_equals_token, lexer%pos, "==")
			else
				call new_token(token, equals_token, lexer%pos, lexer%current())
			end if

		case ("!")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, bang_equals_token, lexer%pos, "!=")
			else
				call new_token(token, bang_token, lexer%pos, lexer%current())
			end if

		case ("<")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, less_equals_token, lexer%pos, "<=")
			else if (lexer%lookahead() == "<") then

				if (lexer%peek(2) == "=") then
					lexer%pos = lexer%pos + 2
					call new_token(token, double_less_equals_token, lexer%pos, "<<=")
				else
					lexer%pos = lexer%pos + 1
					call new_token(token, double_less_token, lexer%pos, "<<")
				end if

			else
				call new_token(token, less_token, lexer%pos, lexer%current())
			end if

		case (">")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, greater_equals_token, lexer%pos, ">=")
			else if (lexer%lookahead() == ">") then

				if (lexer%peek(2) == "=") then
					lexer%pos = lexer%pos + 2
					call new_token(token, double_greater_equals_token, lexer%pos, ">>=")
				else
					lexer%pos = lexer%pos + 1
					call new_token(token, double_greater_token, lexer%pos, ">>")
				end if

			else
				call new_token(token, greater_token, lexer%pos, lexer%current())
			end if

		case ("^")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, caret_equals_token, lexer%pos, "^=")
			else
				call new_token(token, caret_token, lexer%pos, lexer%current())
			end if

		case ("|")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, pipe_equals_token, lexer%pos, "|=")
			else
				call new_token(token, pipe_token, lexer%pos, lexer%current())
			end if

		case ("&")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				call new_token(token, amp_equals_token, lexer%pos, "&=")
			else
				call new_token(token, amp_token, lexer%pos, lexer%current())
			end if

		case default

			!print *, 'bad token text = ', quote(lexer%current())

			call new_token(token, bad_token, lexer%pos, lexer%current())
			span = new_span(lexer%pos, len(lexer%current()))
			call lexer%diagnostics%push( &
				err_unexpected_char(lexer%context, &
				span, lexer%current()))

	end select

	lexer%pos = lexer%pos + 1

	! Note that syntran uses the isocline to read lines. Previously it used
	! rlwrap in syntran <= 1.3 and there used to be a lengthy comment here about
	! it

end subroutine lex_impl

!===============================================================================

subroutine lex_wrap(lexer, token)

	! Thin wrapper so every return path inside lex_impl() doesn't need its own
	! `token%unit_ = lexer%unit_` assignment

	class(lexer_t) :: lexer
	type(syntax_token_t), intent(out) :: token

	call lex_impl(lexer, token)
	token%unit_ = lexer%unit_

end subroutine lex_wrap

!===============================================================================

logical function has_radix_digit(lexer, radix)

	! Is the `0x`/`0o`/`0b` prefix at the lexer's position followed by at least
	! one digit of that radix (not counting `_` separators)?  A bare prefix has
	! no value, so it isn't lexed as a prefixed literal.  The `0` is a decimal
	! literal and the rest is lexed on its own, which the parser reports

	class(lexer_t) :: lexer

	character, intent(in) :: radix

	!********

	character :: c
	integer :: i

	i = 2
	do
		c = lexer%peek(i)
		if (c /= "_") exit
		i = i + 1
	end do

	select case (radix)
	case ("x");  has_radix_digit = is_hex_under(c)
	case ("o");  has_radix_digit = is_oct_under(c)
	case default; has_radix_digit = is_bin_under(c)
	end select

end function has_radix_digit

!===============================================================================

subroutine lex_type_suffix(lexer, radix_name, allow_float, type, suffix, &
		suffix_start, suffix_end)

	! Lex an optional apostrophe type-ascription suffix, e.g. `'i32` in
	! `0x1'i32` or `1.0'f64`.  Pushes err_bad_type_suffix() if the suffix
	! text isn't a recognized type, or isn't one of the types allowed for
	! this literal's radix (allow_float is .false. for hex/octal/binary,
	! which only accept `i32`/`i64`)

	class(lexer_t) :: lexer

	character(len = *), intent(in) :: radix_name
	logical, intent(in) :: allow_float

	integer, intent(out) :: type, suffix_start, suffix_end
	character(len = :), allocatable, intent(out) :: suffix

	!********

	character(len = :), allocatable :: allowed
	type(text_span_t) :: span

	type = unknown_type
	suffix = ""

	if (lexer%current() /= "'") then
		suffix_start = lexer%pos
		suffix_end   = lexer%pos
		return
	end if
	lexer%pos = lexer%pos + 1

	suffix_start = lexer%pos
	do while (is_alphanum_under(lexer%current()))
		lexer%pos = lexer%pos + 1
	end do
	suffix_end = lexer%pos

	suffix = lexer%context%text(suffix_start: suffix_end-1)

	select case (suffix)
	case ("i32")
		type = i32_type
	case ("i64")
		type = i64_type
	case ("f32")
		if (allow_float) type = f32_type
	case ("f64")
		if (allow_float) type = f64_type
	end select

	if (type == unknown_type) then
		if (allow_float) then
			allowed = "`f32`, `f64`, `i32`, `i64`"
		else
			allowed = "`i32`, `i64`"
		end if
		span = new_span(suffix_start, suffix_end - suffix_start)
		call lexer%diagnostics%push(err_bad_type_suffix( &
			lexer%context, span, suffix, radix_name, allowed))
	end if

end subroutine lex_type_suffix

!===============================================================================

character function peek_char(lexer, offset)

	class(lexer_t) :: lexer

	integer, intent(in) :: offset

	!********

	integer :: pos

	pos = lexer%pos + offset

	if (pos < 1 .or. pos > len(lexer%context%text)) then
		peek_char = null_char
		return
	end if

	peek_char = lexer%context%text(pos: pos)

end function peek_char

!===============================================================================

function get_text(lexer, start, end_) result(text)
	! start and end_ are 0-based offset indices.  start offset is inclusive,
	! end_ is exclusive

	class(lexer_t) :: lexer
	integer, intent(in) :: start, end_
	character(len = :), allocatable :: text

	text = lexer%context%text( &
		max(lexer%pos + start   , 1) : &
		min(lexer%pos + end_ - 1, len(lexer%context%text)) &
	)

	!print *, "text = """, text, """"

end function get_text

!===============================================================================

character function current_char(lexer)
	class(lexer_t) :: lexer
	current_char = lexer%peek(0)
end function current_char

character function lookahead_char(lexer)
	class(lexer_t) :: lexer
	lookahead_char = lexer%peek(1)
end function lookahead_char

!===============================================================================

! I am NOT planning on implementing multi-line comments.  Use block-insertion in
! your editor to comment-out multiple lines with "//"

subroutine read_single_line_comment(lexer)

	class(lexer_t) :: lexer

	lexer%pos = lexer%pos + 2

	loop: do

		!print *, 'char = ', lexer%current()
		select case (lexer%current())
			case (null_char, carriage_return, line_feed)
				exit loop
		end select

		lexer%pos = lexer%pos + 1
	end do loop
	!print *, 'done'

end subroutine read_single_line_comment

!===============================================================================

logical function is_line_end(text, i)

	! Is the char `i` of `text` the end of a line, for counting lines in
	! new_lexer()?  It's a line feed, or the CR that is not the first half of a
	! CRLF, so a CRLF is one line end, like a line feed alone.  The last char
	! always ends a line, even without a line feed

	character(len = *), intent(in) :: text
	integer, intent(in) :: i

	!********

	is_line_end = i == len(text) .or. text(i:i) == line_feed

	if (text(i:i) == carriage_return .and. i < len(text)) then
		is_line_end = is_line_end .or. text(i+1: i+1) /= line_feed
	end if

end function is_line_end

!===============================================================================

function new_lexer(text, src_file, unit_) result(lexer)

	character(len = *) :: text, src_file

	type(lexer_t) :: lexer

	! Index that this lexer's context will have in the parser's context vector,
	! i.e. 1 + the number of contexts already pushed
	integer, intent(in) :: unit_

	!********

	integer :: i, i0, nlines

	integer, allocatable :: lines(:)

	! Every token keeps track of which file it came from for error diagnostic
	! context
	lexer%unit_ = unit_

	!print *, 'lexer%unit_ = ', lexer%unit_

	lexer%pos      = 1

	lexer%diagnostics = new_string_vector()

	! Count lines
	nlines = 0
	i = 0
	!outer: do
	do
		i = i + 1
		if (i > len(text)) exit !outer

		if (is_line_end(text, i)) nlines = nlines + 1

	end do !outer

	!print *, 'nlines = ', nlines

	allocate(lines(nlines + 1))

	! Get character indices for the start of each line and save them in lines(:)
	nlines = 0
	i = 0
	i0 = 0
	do
		i = i + 1
		if (i > len(text)) exit

		if (is_line_end(text, i)) then

			nlines = nlines + 1

			lines(nlines) = i0 + 1
			i0 = i

		end if

	end do
	lines(nlines + 1) = len(text) + 1

	!print *, 'lines = ', lines

	if (debug > 1) then
		write(*,*) 'lines = '
		do i = 1, nlines
			write(*, '(i5,a)') i, ' | '//text(lines(i): lines(i+1) - 2)
		end do
	end if

	lexer%context = new_context(text, src_file, lines)

end function new_lexer

!===============================================================================

function escape_dump(str_) result(esc)

	! Escape a token's text or value for lex_dump(), so that a token is always
	! exactly one line, with no tabs in it.  Bytes >= 128 are left alone

	character(len = *), intent(in) :: str_
	character(len = :), allocatable :: esc

	!********

	integer :: i

	type(char_vector_t) :: sb

	sb = new_char_vector()
	do i = 1, len(str_)
		select case (str_(i:i))
		case ("\")
			call sb%push("\")
			call sb%push("\")
		case (line_feed)
			call sb%push("\")
			call sb%push("n")
		case (carriage_return)
			call sb%push("\")
			call sb%push("r")
		case (tab)
			call sb%push("\")
			call sb%push("t")
		case (null_char)
			call sb%push("\")
			call sb%push("0")
		case default
			call sb%push(str_(i:i))
		end select
	end do
	esc = sb%trim()

end function escape_dump

!===============================================================================

function diag_code(diag) result(code)

	! Get the `E##` out of a diagnostic like "Error[E42]: ..."

	character(len = *), intent(in) :: diag
	character(len = :), allocatable :: code

	!********

	integer :: i, j

	code = "?"

	i = index(diag, "Error[")
	if (i == 0) return
	i = i + len("Error[")

	j = index(diag(i:), "]")
	if (j == 0) return

	code = diag(i: i + j - 2)

end function diag_code

!===============================================================================

function lex_dump(text, src_file) result(str_)

	! Lex `text` and dump every raw token, one per line, followed by the code of
	! each lexer diagnostic.  Unlike new_parser(), whitespace, comment, and bad
	! tokens are all kept.  This is used to test the syntran port of the lexer
	! in compiler/lex.syntran, which prints the same format:
	!
	!     <pos> <kind_name> <text> [<value>]
	!     ...
	!     diag <code>
	!
	! The fields are separated by one space, and the text and value can have
	! spaces of their own.  `pos` is 0-based, unlike the lexer's own 1-based pos

	character(len = *), intent(in) :: text, src_file
	character(len = :), allocatable :: str_

	!********

	integer :: i

	type(char_vector_t) :: sb
	type(lexer_t) :: lexer
	type(syntax_token_t) :: token

	sb = new_char_vector()
	lexer = new_lexer(text, src_file, 1)

	do
		call lexer%lex(token)

		call push_str(sb, str(token%pos - 1)//" "//kind_name(token%kind)//" " &
			//escape_dump(token%text))

		select case (token%kind)
		case (i32_token, i64_token, f32_token, f64_token, str_token)
			call push_str(sb, " "//escape_dump(token%val%to_str()))
		end select
		call push_str(sb, line_feed)

		if (token%kind == eof_token) exit
	end do

	do i = 1, lexer%diagnostics%len_
		call push_str(sb, "diag "//diag_code(lexer%diagnostics%v(i)%s)//line_feed)
	end do

	str_ = sb%trim()

end function lex_dump

!===============================================================================

subroutine push_str(sb, str_)
	type(char_vector_t), intent(inout) :: sb
	character(len = *), intent(in) :: str_
	integer :: i
	do i = 1, len(str_)
		call sb%push(str_(i:i))
	end do
end subroutine push_str

!===============================================================================

end module syntran__lex_m

!===============================================================================

