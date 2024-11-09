
!===============================================================================

module syntran__lex_m

	!use syntran__errors_m
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

		character(len = :), allocatable :: text

		! Both the lexer and the parser have current() and lex()/next() member
		! fns.  current_char() returns a char, while the others return syntax
		! tokens
		contains
			procedure :: lex, peek => peek_char, current => current_char, &
				lookahead => lookahead_char, read_single_line_comment, get_text

	end type lexer_t

!===============================================================================

contains

!===============================================================================

function lex(lexer) result(token)

	class(lexer_t) :: lexer

	type(syntax_token_t) :: token

	!********

	character(len = :), allocatable :: text, text_strip, suffix

	integer :: kind, type, start, end_, io, suffix_start, suffix_end
	integer(kind = 4) :: i32
	integer(kind = 8) :: i64

	logical :: float, float32, float64

	real(kind = 4) :: f32
	real(kind = 8) :: f64

	type(char_vector_t) :: char_vec
	type(text_span_t) :: span
	type(value_t) :: val

	!print *, 'lexer%unit_ = ', lexer%unit_

	if (lexer%pos > len(lexer%text)) then

		token = new_token(eof_token, lexer%pos, null_char)

		! TODO: it's kind of annoying to have to set unit_ before every return.
		! It might be better to modify all the new_*_token() fns
		token%unit_ = lexer%unit_
		return

	end if

	start = lexer%pos

	!********

	if (lexer%get_text(0,2) == "0x") then
		! Hex literal
		lexer%pos = lexer%pos + 2  ! skip "0x"

		do while (is_hex_under(lexer%current()))
			! Lex literal body
			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		type = unknown_type
		if (lexer%current() == "'") then
			lexer%pos = lexer%pos + 1
			!print *, "suffix"

			! Lex literal type suffix
			suffix_start = lexer%pos
			do while (is_alphanum_under(lexer%current()))
				lexer%pos = lexer%pos + 1
			end do
			suffix_end = lexer%pos

			suffix = lexer%text(suffix_start: suffix_end-1)

			! TODO: call lookup_type() on suffix? It would eliminate the magic
			! strings like "i32" which are duplicated here, but we would still
			! need a select/case here to block bad types (e.g. "f32")
			select case (suffix)
			case ("i32")
				!print *, "i32"
				type = i32_type
			case ("i64")
				type = i64_type
			case default
				! TODO: maybe hint in diag about which suffixes *are* allowed?
				! Might want entirely different diag fns for hex vs dec instead
				! of passing "hex" str arg
				span = new_span(suffix_start, suffix_end - suffix_start)
				call lexer%diagnostics%push(err_bad_type_suffix( &
					lexer%context, span, suffix, "hex"))
			end select

		end if

		text = lexer%text(start: end_ - 1)
		text_strip = rm_char(text(3:), "_")

		if (type == i32_type) then

			read(text_strip, "(z12)", iostat = io) i32
			if (io == exit_success) then
				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)
			else
				token = new_token(bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_hex32( &
					lexer%context, span, text))
			end if

		else if (type == i64_type) then

			read(text_strip, "(z20)", iostat = io) i64
			if (io == exit_success) then
				val   = new_literal_value(i64_type, i64 = i64)
				token = new_token(i64_token, start, text, val)
			else
				token = new_token(bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_hex64( &
					lexer%context, span, text))
			end if

		else

			! 8 chars should be sufficient. pad by an extra 4 for safety
			read(text_strip, "(z12)", iostat = io) i32
			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)

			else

				read(text_strip, "(z20)", iostat = io) i64  ! 16 chars should suffice
				if (io == exit_success) then

					!print *, "i64 = ", i64
					val   = new_literal_value(i64_type, i64 = i64)
					token = new_token(i64_token, start, text, val)

				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_hex64( &
						lexer%context, span, text))
				end if
			end if
		end if

		token%unit_ = lexer%unit_
		return
	end if  ! "0x"

	!********

	if (lexer%get_text(0,2) == "0o") then
		! Octal literal
		lexer%pos = lexer%pos + 2

		do while (is_oct_under(lexer%current()))
			! Lex literal body
			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		type = unknown_type
		if (lexer%current() == "'") then
			lexer%pos = lexer%pos + 1
			!print *, "suffix"

			! Lex literal type suffix
			suffix_start = lexer%pos
			do while (is_alphanum_under(lexer%current()))
				lexer%pos = lexer%pos + 1
			end do
			suffix_end = lexer%pos

			suffix = lexer%text(suffix_start: suffix_end-1)

			! TODO: call lookup_type() on suffix? It would eliminate the magic
			! strings like "i32" which are duplicated here, but we would still
			! need a select/case here to block bad types (e.g. "f32")
			select case (suffix)
			case ("i32")
				!print *, "i32"
				type = i32_type
			case ("i64")
				type = i64_type
			case default
				! TODO: maybe hint in diag about which suffixes *are* allowed?
				! Might want entirely different diag fns for oct vs dec instead
				! of passing "oct" str arg
				span = new_span(suffix_start, suffix_end - suffix_start)
				call lexer%diagnostics%push(err_bad_type_suffix( &
					lexer%context, span, suffix, "octal"))
			end select

		end if

		text = lexer%text(start: end_ - 1)
		text_strip = rm_char(text(3:), "_")

		if (type == i32_type) then

			read(text_strip, "(o20)", iostat = io) i32
			if (io == exit_success) then
				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)
			else
				token = new_token(bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_oct32( &
					lexer%context, span, text))
			end if

		else if (type == i64_type) then

			read(text_strip, "(o36)", iostat = io) i64
			if (io == exit_success) then
				val   = new_literal_value(i64_type, i64 = i64)
				token = new_token(i64_token, start, text, val)
			else
				token = new_token(bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_oct64( &
					lexer%context, span, text))
			end if

		else

			! 16 chars should be sufficient. pad by an extra 4 for safety
			!
			! TODO: these octal text widths are padded way to generously.
			! "(z8)" is the max for hex, but it's actually less then double that
			! width for octal.  See the test on 0o377_7777_7777 which is -1 in
			! octal, which is only 11 chars
			read(text_strip, "(o20)", iostat = io) i32
			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)

			else

				read(text_strip, "(o36)", iostat = io) i64  ! 32 chars should suffice
				if (io == exit_success) then

					val   = new_literal_value(i64_type, i64 = i64)
					token = new_token(i64_token, start, text, val)

				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_oct64( &
						lexer%context, span, text))
				end if
			end if
		end if

		token%unit_ = lexer%unit_
		return
	end if  ! "0o"

	!********

	if (lexer%get_text(0,2) == "0b") then
		! Binary literal
		lexer%pos = lexer%pos + 2

		do while (is_bin_under(lexer%current()))
			! Lex literal body
			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		type = unknown_type
		if (lexer%current() == "'") then
			lexer%pos = lexer%pos + 1
			!print *, "suffix"

			! Lex literal type suffix
			suffix_start = lexer%pos
			do while (is_alphanum_under(lexer%current()))
				lexer%pos = lexer%pos + 1
			end do
			suffix_end = lexer%pos

			suffix = lexer%text(suffix_start: suffix_end-1)

			! TODO: call lookup_type() on suffix? It would eliminate the magic
			! strings like "i32" which are duplicated here, but we would still
			! need a select/case here to block bad types (e.g. "f32")
			select case (suffix)
			case ("i32")
				!print *, "i32"
				type = i32_type
			case ("i64")
				type = i64_type
			case default
				! TODO: maybe hint in diag about which suffixes *are* allowed?
				! Might want entirely different diag fns for bin vs dec instead
				! of passing "bin" str arg
				span = new_span(suffix_start, suffix_end - suffix_start)
				call lexer%diagnostics%push(err_bad_type_suffix( &
					lexer%context, span, suffix, "binary"))
			end select

		end if

		text = lexer%text(start: end_ - 1)
		text_strip = rm_char(text(3:), "_")

		if (type == i32_type) then

			read(text_strip, "(b36)", iostat = io) i32
			if (io == exit_success) then
				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)
			else
				token = new_token(bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_bin32( &
					lexer%context, span, text))
			end if

		else if (type == i64_type) then

			read(text_strip, "(b68)", iostat = io) i64
			if (io == exit_success) then
				val   = new_literal_value(i64_type, i64 = i64)
				token = new_token(i64_token, start, text, val)
			else
				token = new_token(bad_token, lexer%pos, text)
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_bin64( &
					lexer%context, span, text))
			end if

		else

			! 32 chars should be sufficient. pad by an extra 4 for safety
			read(text_strip, "(b36)", iostat = io) i32
			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)

			else

				read(text_strip, "(b68)", iostat = io) i64  ! 64 chars should suffice
				if (io == exit_success) then

					val   = new_literal_value(i64_type, i64 = i64)
					token = new_token(i64_token, start, text, val)

				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_bin64( &
						lexer%context, span, text))
				end if
			end if
		end if

		token%unit_ = lexer%unit_
		return
	end if  ! "0b"

	!********

	if (is_digit_under(lexer%current())) then
		! Numeric decimal integer or float

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
		type = unknown_type
		if (lexer%current() == "'") then
			lexer%pos = lexer%pos + 1
			!print *, "suffix"

			! Lex literal type suffix
			suffix_start = lexer%pos
			do while (is_alphanum_under(lexer%current()))
				lexer%pos = lexer%pos + 1
			end do
			suffix_end = lexer%pos

			suffix = lexer%text(suffix_start: suffix_end-1)
			select case (suffix)
			case ("f32")
				type = f32_type
			case ("f64")
				type = f64_type
			case ("i32")
				type = i32_type
			case ("i64")
				type = i64_type
			case default
				span = new_span(suffix_start, suffix_end - suffix_start)
				call lexer%diagnostics%push(err_bad_type_suffix( &
					lexer%context, span, suffix, "decimal"))
			end select

			! TODO: throw new diag if float and i32 or i64? Currently, `4.0'i32`
			! throws err_bad_i32() which isn't exactly the right message

		end if

		text = lexer%text(start: end_ - 1)
		text_strip = rm_char(text, "_")

		!print *, 'float text = ', quote(text)

		if (type /= unknown_type) then
			! Handle explicit apostrophe type ascription suffixes

			select case (type)
			case (f32_type)

				read(text_strip, *, iostat = io) f32
				if (io == exit_success) then
					val   = new_literal_value(f32_type, f32 = f32)
					token = new_token(f32_token, start, text, val)
				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_f32( &
						lexer%context, span, text))
				end if

			case (f64_type)

				read(text_strip, *, iostat = io) f64
				if (io == exit_success) then
					val   = new_literal_value(f64_type, f64 = f64)
					token = new_token(f64_token, start, text, val)
				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_f64( &
						lexer%context, span, text))
				end if

			case (i32_type)

				read(text_strip, *, iostat = io) i32
				if (io == exit_success) then
					val   = new_literal_value(i32_type, i32 = i32)
					token = new_token(i32_token, start, text, val)
				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					! TODO: specific i32/i64 diags
					call lexer%diagnostics%push(err_bad_i32( &
						lexer%context, span, text))
				end if

			case (i64_type)

				read(text_strip, *, iostat = io) i64
				if (io == exit_success) then
					val   = new_literal_value(i64_type, i64 = i64)
					token = new_token(i64_token, start, text, val)
				else
					token = new_token(bad_token, lexer%pos, text)
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
			token = new_token(f32_token, start, text, val)

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
			token = new_token(f64_token, start, text, val)

		else

			read(text_strip, *, iostat = io) i32

			if (io == exit_success) then

				val   = new_literal_value(i32_type, i32 = i32)
				token = new_token(i32_token, start, text, val)

			else

				read(text_strip, *, iostat = io) i64

				if (io == exit_success) then

					val   = new_literal_value(i64_type, i64 = i64)
					token = new_token(i64_token, start, text, val)

				else
					token = new_token(bad_token, lexer%pos, text)
					span = new_span(start, len(text))
					call lexer%diagnostics%push(err_bad_i64( &
						lexer%context, span, text))
				end if

			end if
		end if

		token%unit_ = lexer%unit_
		return

	end if

	if (lexer%current() == '"') then

		! Skip the current quote
		lexer%pos = lexer%pos + 1

		char_vec = new_char_vector()
		do

			! Make a quote literal by doubling it
			if (lexer%current() == '"') then
				lexer%pos = lexer%pos + 1
				if (lexer%current() /= '"') then
					exit
				end if
			end if

			call char_vec%push(lexer%current())
			lexer%pos = lexer%pos + 1

			if (lexer%pos > len(lexer%text)) exit

		end do

		text  = lexer%text(start: lexer%pos-1)

		if (lexer%pos > len(lexer%text)) then
			token = new_token(bad_token, lexer%pos, text)
			span = new_span(start, len(text))
			call lexer%diagnostics%push( &
				err_unterminated_str(lexer%context, &
				span, text))
			token%unit_ = lexer%unit_
			return
		end if

		val   = new_literal_value(str_type, str = char_vec%v( 1: char_vec%len_ ))
		token = new_token(str_token, start, text, val)

		token%unit_ = lexer%unit_
		return

	end if

	if (is_whitespace(lexer%current())) then

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		token = new_token(whitespace_token, start, text)
		token%unit_ = lexer%unit_
		return

	end if

	if (is_letter(lexer%current()) .or. lexer%current() == '_') then

		do while (is_alphanum(lexer%current()) .or. lexer%current() == '_')
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		! This block handles booleans as well as identifiers, but note that it
		! does not set the value here like the is_digit_under() case for numbers
		! above.  The boolean value is not set until parse_primary_expr().

		kind = get_keyword_kind(text)
		token = new_token(kind, start, text)
		token%unit_ = lexer%unit_
		return

	end if

	if (lexer%pos == 1           .and. &
		lexer%get_text(0,2) == "#!") then

		! Handle a special shebang `#!` case at very beginning of file and
		! ignore the rest of the first line
		call lexer%read_single_line_comment()

		text = lexer%text(start: lexer%pos-1)
		token = new_token(whitespace_token, start, text)
		return

	end if

	select case (lexer%current())

		case ("+")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(plus_equals_token, lexer%pos, "+=")
			else
				token = new_token(plus_token, lexer%pos, lexer%current())
			end if

			! FIXME: prefix/postfix inc/dec operators (++, --)

		case ("-")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(minus_equals_token, lexer%pos, "-=");
			else
				token = new_token(minus_token, lexer%pos, lexer%current())
			end if

		case ("*")

			if (lexer%lookahead() == "*") then

				if (lexer%peek(2) == "=") then
					!print *, '**='
					lexer%pos = lexer%pos + 2
					token = new_token(sstar_equals_token, lexer%pos, "**=")

				else
					lexer%pos = lexer%pos + 1
					token = new_token(sstar_token, lexer%pos, "**")

				end if

			else if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(star_equals_token, lexer%pos, "*=")

			else
				token = new_token(star_token, lexer%pos, lexer%current())

			end if

		case ("/")
			if (lexer%lookahead() == "/") then

				call lexer%read_single_line_comment()

				! FIXME: make "trivia" token types instead of overloading
				! whitespace_token for comments.  This is what Immo did
				text = lexer%text(start: lexer%pos-1)
				token = new_token(whitespace_token, start, text)

			else if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(slash_equals_token, lexer%pos, "/=")

			else
				token = new_token(slash_token, lexer%pos, lexer%current())
			end if

		case ("%")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(percent_equals_token, lexer%pos, "%=");
			else
				token = new_token(percent_token, lexer%pos, lexer%current())
			end if

		case ("(")
			token = new_token(lparen_token, lexer%pos, lexer%current())

		case (")")
			token = new_token(rparen_token, lexer%pos, lexer%current())

		case ("{")
			token = new_token(lbrace_token, lexer%pos, lexer%current())

		case ("}")
			token = new_token(rbrace_token, lexer%pos, lexer%current())

		case ("[")
			token = new_token(lbracket_token, lexer%pos, lexer%current())

		case ("]")
			token = new_token(rbracket_token, lexer%pos, lexer%current())

		case (":")
			token = new_token(colon_token, lexer%pos, lexer%current())

		case (";")
			token = new_token(semicolon_token, lexer%pos, lexer%current())

		case (",")
			token = new_token(comma_token, lexer%pos, lexer%current())

		case (".")
			token = new_token(dot_token, lexer%pos, lexer%current())

		case ("#")
			token = new_token(hash_token, lexer%pos, lexer%current())

		case ("=")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(eequals_token, lexer%pos, "==")
			else
				token = new_token(equals_token, lexer%pos, lexer%current())
			end if

		case ("!")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(bang_equals_token, lexer%pos, "!=")
			else

				! FIXME: refactor w/ default case below since Fortran is weird
				! about breaking in select case
				token = new_token(bad_token, lexer%pos, lexer%current())
				span = new_span(lexer%pos, len(lexer%current()))
				call lexer%diagnostics%push( &
					err_unexpected_char(lexer%context, &
					span, lexer%current()))

			end if

		case ("<")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(less_equals_token, lexer%pos, "<=")
			else
				token = new_token(less_token, lexer%pos, lexer%current())
			end if

		case (">")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(greater_equals_token, lexer%pos, ">=")
			else
				token = new_token(greater_token, lexer%pos, lexer%current())
			end if

		case default

			!print *, 'bad token text = ', quote(lexer%current())

			token = new_token(bad_token, lexer%pos, lexer%current())
			span = new_span(lexer%pos, len(lexer%current()))
			call lexer%diagnostics%push( &
				err_unexpected_char(lexer%context, &
				span, lexer%current()))

	end select
	token%unit_ = lexer%unit_

	lexer%pos = lexer%pos + 1

	! FIXME: arrow keys create bad tokens in bash on Windows.  Fix that (better
	! yet, override up arrow to do what it does in bash.  c.f. rubik-js)
	!
	! Actually this is somewhat difficult bc I think it requires event
	! listening.  Currently syntran does not get any stdin until the user hits
	! <enter>.  So if they type <up-arrow>, syntran doesn't know anything about
	! that until they subsequently hit <enter>.  I guess we could use some 3p
	! C(++) lib to do keypress event listening, but there's an easier solution.
	!
	! Just use rlwrap:
	!
	!     sudo apt install rlwrap
	!     rlwrap syntran
	!
	! Then rlwrap does the listening, handles all arrow keys as expected, and
	! passes stdin along to syntran.  See run.sh which checks if you have rlwrap
	! installed.  Windows cmd works good enough without rlwrap.
	!
	! You can make a shell alias for that.
	!
	! Idris lang also makes the same recommendation:
	!
	!     https://github.com/idris-lang/Idris2/issues/54

end function lex

!===============================================================================

character function peek_char(lexer, offset)

	class(lexer_t) :: lexer

	integer, intent(in) :: offset

	!********

	integer :: pos

	pos = lexer%pos + offset

	if (pos < 1 .or. pos > len(lexer%text)) then
		peek_char = null_char
		return
	end if

	peek_char = lexer%text(pos: pos)

end function peek_char

!===============================================================================

function get_text(lexer, start, end_) result(text)
	! start and end_ are 0-based offset indices.  start offset is inclusive,
	! end_ is exclusive

	class(lexer_t) :: lexer
	integer, intent(in) :: start, end_
	character(len = :), allocatable :: text

	text = lexer%text( &
		max(lexer%pos + start   , 1) : &
		min(lexer%pos + end_ - 1, len(lexer%text)) &
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

function new_lexer(text, src_file, unit_) result(lexer)

	character(len = *) :: text, src_file

	type(lexer_t) :: lexer

	integer, intent(inout) :: unit_

	!********

	integer :: i, i0, nlines
	!integer, save :: unit_ = 0

	integer, allocatable :: lines(:)

	! Every token keeps track of which file it came from for error diagnostic
	! context
	unit_ = unit_ + 1
	lexer%unit_ = unit_

	!print *, 'lexer%unit_ = ', lexer%unit_

	lexer%text     = text
	lexer%pos      = 1

	lexer%diagnostics = new_string_vector()

	! Count lines
	nlines = 0
	i = 0
	!outer: do
	do
		i = i + 1
		if (i > len(text)) exit !outer

		if (i == len(text) .or. &
			text(i:i) == line_feed .or. &
			text(i:i) == carriage_return) then

			nlines = nlines + 1

			!do
			!	i = i + 1
			!	if (i > len(text)) exit outer
			!	if (text(i:i) /= line_feed .and. &
			!	    text(i:i) /= carriage_return) exit
			!end do

		end if

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

		if (i == len(text) .or. &
			text(i:i) == line_feed .or. &
			text(i:i) == carriage_return) then

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

	! TODO: delete lexer%text in favor of lexer%context%text.  It appears in
	! a lot of places
	lexer%context = new_context(text, src_file, lines)

end function new_lexer

!===============================================================================

end module syntran__lex_m

!===============================================================================

