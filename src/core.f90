
!===============================================================================

module core_m

	use iso_fortran_env
	use utils

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = 'syntran'

	integer, parameter :: debug = 3

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
			bool_expr        = 18, &
			literal_expr     = 17, &
			true_keyword     = 16, &
			false_keyword    = 15, &
			identifier_token = 14, &
			unary_expr       = 13, &
			lparen_token     = 12, &
			rparen_token     = 11, &
			num_expr         = 10, &
			binary_expr      =  9, &
			star_token       =  8, &
			slash_token      =  7, &
			bad_token        =  6, &
			plus_token       =  5, &
			minus_token      =  4, &
			whitespace_token =  3, &
			num_token        =  2, &
			eof_token        =  1

	!********

	type syntax_token_t

		integer :: kind

		! TODO: how to handle values of different types?
		integer :: val

		integer :: pos
		character(len = :), allocatable :: text

	end type syntax_token_t

	!********

	type value_t
		!integer :: kind  ! TODO: is this redundant?  parent node should always have its kind
		logical :: bval
		integer :: ival
	end type value_t

	!********

	type syntax_node_t

		integer :: kind
		type(syntax_node_t), allocatable :: left, right
		type(syntax_token_t) :: op, num
		type(value_t) :: val

		! TODO: remove num and use val instead

		type(string_vector_t) :: diagnostics

		contains
			procedure :: str => syntax_node_str, log_diagnostics

			procedure, pass(dst) :: copy => syntax_node_copy
			generic, public :: assignment(=) => copy

	end type syntax_node_t

	!********

	type lexer_t

		! The lexer takes a string of characters and divides it into into tokens
		! or words

		character(len = :), allocatable :: text
		integer :: pos

		type(string_vector_t) :: diagnostics

		! Both the lexer and the parser have current() and lex()/next() member
		! fns.  current_char() returns a char, while the others return syntax
		! tokens
		contains
			procedure :: lex, current => current_char

	end type lexer_t

	!********

	type parser_t

		! The parser takes a string of tokens (technically an array) and
		! constructs higher-level structures such as terms and expressions, like
		! constructing a phrase or sentence from words

		type(syntax_token_t), allocatable :: tokens(:)
		integer :: pos

		type(string_vector_t) :: diagnostics

		contains
			procedure :: match, tokens_str, current_kind, &
				current => current_token, next => next_parser_token, &
				parse_expr, parse_primary_expr

	end type parser_t

	!********

	type syntax_token_vector_t
		type(syntax_token_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push => push_token
	end type syntax_token_vector_t

!===============================================================================

contains

!===============================================================================

function new_syntax_token_vector() result(vector)

	type(syntax_token_vector_t) :: vector

	vector%len = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_token_vector

!===============================================================================

subroutine push_token(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(syntax_token_vector_t) :: vector
	type(syntax_token_t) :: val

	!********

	type(syntax_token_t), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len ) = val

end subroutine push_token

!===============================================================================

function kind_name(kind)

	integer, intent(in) :: kind

	character(len = :), allocatable :: kind_name

	character(len = *), parameter :: names(*) = [ &
			"eof_token       ", & !  1
			"num_token       ", & !  2
			"whitespace_token", & !  3
			"minus_token     ", & !  4
			"plus_token      ", & !  5
			"bad_token       ", & !  6
			"slash_token     ", & !  7
			"star_token      ", & !  8
			"binary_expr     ", & !  9
			"num_expr        ", & ! 10
			"rparen_token    ", & ! 11
			"lparen_token    ", & ! 12
			"unary_expr      ", & ! 13
			"identifier_token", & ! 14
			"false_keyword   ", & ! 15
			"true_keyword    ", & ! 16
			"literal_expr    ", & ! 17
			"bool_expr       "  & ! 18
		]

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if

	kind_name = trim(names(kind))

end function

!===============================================================================

recursive function syntax_node_str(node, indent) result(str)

	! Convert tree to string in JSON-ish format

	class(syntax_node_t) :: node

	character(len = *), optional :: indent

	character(len = :), allocatable :: str

	!********

	character(len = :), allocatable :: indentl, kind, num, left, op, right

	indentl = ''
	if (present(indent)) indentl = indent

	kind = indentl//'    kind  = '//kind_name(node%kind)//line_feed

	left  = ''
	op    = ''
	right = ''
	num   = ''

	if      (node%kind == binary_expr) then

		left  = indentl//'    left  = '//node%left %str(indentl//'    ') &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == unary_expr) then

		op    = indentl//'    op    = '//node%op%text//line_feed
		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == num_expr) then
		num   = indentl//'    num   = '//node%num%text   //line_feed

		! TODO: literal or at least specific bool

	end if

	str = line_feed// &
		indentl//'{'//line_feed// &
			kind // &
			left // &
			op   // &
			right// &
			num  // &
		indentl//'}'

end function syntax_node_str

!===============================================================================

recursive subroutine syntax_node_copy(dst, src)

	! Deep copy.  Default Fortran assignment operator doesn't handle recursion
	! correctly for my node type, leaving dangling refs to src when it is
	! deallocated.
	!
	! Args have to be in the confusing dst, src order for overloading

	class(syntax_node_t), intent(inout) :: dst
	class(syntax_node_t), intent(in)    :: src

	if (debug > 3) print *, 'starting syntax_node_copy()'

	dst%kind = src%kind
	dst%op   = src%op
	dst%num  = src%num

	dst%diagnostics = src%diagnostics

	if (allocated(src%left)) then
		!if (debug > 1) print *, 'copy() left'
		if (.not. allocated(dst%left)) allocate(dst%left)
		dst%left = src%left
	end if

	if (allocated(src%right)) then
		!if (debug > 1) print *, 'copy() right'
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	end if

end subroutine syntax_node_copy

!===============================================================================

function new_token(kind, pos, text, val) result(token)

	integer, optional :: val
	integer :: kind, pos

	character(len = *) :: text

	type(syntax_token_t) :: token

	token%kind = kind
	token%pos  = pos
	token%text = text

	if (present(val)) token%val  = val

end function new_token

!===============================================================================

character function current_char(lexer)

	class(lexer_t) :: lexer

	if (lexer%pos > len(lexer%text)) then
		current_char = null_char
		return
	end if

	current_char = lexer%text( lexer%pos: lexer%pos )

end function current_char

!===============================================================================

function lex(lexer) result(token)

	class(lexer_t) :: lexer

	type(syntax_token_t) :: token

	!********

	integer :: val, kind
	integer :: start, io

	character(len = :), allocatable :: text

	if (lexer%pos > len(lexer%text)) then
		token = new_token(eof_token, lexer%pos, null_char)
		return
	end if

	if (is_digit(lexer%current())) then

		start = lexer%pos

		do while (is_digit(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		read(text, *, iostat = io) val
		if (io /= 0) then
			! TODO: Refactor w/ underline fn, centralized style
			call lexer%diagnostics%push( &
					repeat(' ', start-1)//fg_bright_red &
					//repeat('^', len(text))//color_reset//line_feed &
					//fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': invalid int32 '//text &
					//color_reset &
					)
		end if

		token = new_token(num_token, start, text, val)
		return

	end if

	if (is_whitespace(lexer%current())) then

		start = lexer%pos

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		token = new_token(whitespace_token, start, text)
		return

	end if

	if (is_letter(lexer%current()) .or. lexer%current() == '_') then

		start = lexer%pos

		do while (is_alphanum(lexer%current()) .or. lexer%current() == '_')
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		kind = get_keyword_kind(text)
		token = new_token(kind, start, text)
		return

	end if

	select case (lexer%current())

		case ("+")
			token = new_token(plus_token  , lexer%pos, lexer%current())
			! TODO: prefix/postfix inc/dec operators (++, --)
		case ("-")
			token = new_token(minus_token , lexer%pos, lexer%current())
		case ("*")
			token = new_token(star_token  , lexer%pos, lexer%current())
		case ("/")
			token = new_token(slash_token , lexer%pos, lexer%current())
		case ("(")
			token = new_token(lparen_token, lexer%pos, lexer%current())
		case (")")
			token = new_token(rparen_token, lexer%pos, lexer%current())

		case default

			token = new_token(bad_token, lexer%pos, lexer%current())

			call lexer%diagnostics%push( &
					repeat(' ', lexer%pos-1)//fg_bright_red &
					//repeat('^', 1)//color_reset//line_feed &
					//fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//": unexpected character '"//lexer%current() &
					//"'" &
					//color_reset &
					)

	end select
	lexer%pos = lexer%pos + 1

	! TODO: arrow keys create bad tokens in bash on Windows.  Fix that (better
	! yet, override up arrow to do what it does in bash.  c.f. rubik-js)

end function lex

!===============================================================================

integer function get_keyword_kind(text) result(kind)

	character(len = *), intent(in) :: text

	! Here we start to depart from Fortran syntax (true, not .true.)
	select case (text)
		case ("true")
			kind = true_keyword
		case ("false")
			kind = false_keyword
		case default
			kind = identifier_token
	end select

	print *, 'get_keyword_kind = ', kind

end function get_keyword_kind

!===============================================================================

function new_lexer(text) result(lexer)

	character(len = *) :: text

	type(lexer_t) :: lexer

	lexer%text = text
	lexer%pos = 1

	lexer%diagnostics = new_string_vector()

end function new_lexer

!===============================================================================

function new_parser(str) result(parser)

	character(len = *) :: str

	type(parser_t) :: parser

	!********

	type(syntax_token_t)        :: token
	type(syntax_token_vector_t) :: tokens

	type(lexer_t) :: lexer

	! Get an array of tokens
	tokens = new_syntax_token_vector()
	lexer = new_lexer(str)
	do
		token = lexer%lex()

		if (token%kind /= whitespace_token .and. &
		    token%kind /= bad_token) then

			!print *, 'token = <', token%text, '> ', &
			!		'<'//kind_name(token%kind)//'>'

			call tokens%push(token)

		end if

		if (token%kind == eof_token) exit
	end do

	! Convert to standard array (and class member)
	parser%tokens = tokens%v( 1: tokens%len )

	parser%pos = 1

	parser%diagnostics = lexer%diagnostics

	if (debug > 1) print *, parser%tokens_str()

end function new_parser

!===============================================================================

function tokens_str(parser) result(str)

	class(parser_t) :: parser

	character(len = :), allocatable :: str

	!********

	integer :: i

	! This will crash for very long token lists, but it should suffice for basic
	! debugging

	str = 'tokens = '//line_feed//'['//line_feed
	do i = 1, size(parser%tokens)
		str = str//tab &
				//'<'//           parser%tokens(i)%text  //'> ' &
				//'<'//kind_name( parser%tokens(i)%kind )//'>'  &
				//line_feed
	end do
	str = str//']'//line_feed

end function tokens_str

!===============================================================================

function syntax_parse(str) result(tree)

	character(len = *) :: str

	type(syntax_node_t) :: tree

	!********

	type(parser_t) :: parser

	type(syntax_token_t) :: token

	if (debug > 0) print *, 'syntax_parse'
	if (debug > 0) print *, 'str = ', str

	parser = new_parser(str)

	! Parse the tokens. Should this accept empty strings?  It says unexpected
	! token trying to match number in parse_primary_expr(), so currently the
	! interpreter driver skips empty lines
	tree = parser%parse_expr()

	if (debug > 1) print *, 'tree = ', tree%str()

	if (debug > 1) print *, 'matching eof'
	token  = parser%match(eof_token)

	tree%diagnostics = parser%diagnostics

end function syntax_parse

!===============================================================================

recursive function parse_expr(parser, parent_prec) result(expr)

	class(parser_t) :: parser

	integer, optional, intent(in) :: parent_prec

	type(syntax_node_t) :: expr

	!********

	integer :: parent_precl, prec

	type(syntax_node_t) :: right
	type(syntax_token_t) :: op

	if (debug > 1) print *, 'parse_expr'

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_unary_expr(op, right)

	else
		expr = parser%parse_primary_expr()
	end if

	do
		prec = get_binary_op_prec(parser%current_kind())
		if (prec == 0 .or. prec <= parent_precl) exit

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_binary_expr(expr, op, right)

	end do

end function parse_expr

!===============================================================================

! You can't chain together member fn calls and their children like
! parser%current()%kind in Fortran, so use this helper fn instead

integer function current_kind(parser)
	class(parser_t) :: parser
	type(syntax_token_t) :: current
	current = parser%current()
	current_kind = current%kind
end function current_kind

!===============================================================================

integer function get_binary_op_prec(kind) result(prec)

	! Get binary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		case (star_token, slash_token)
			prec = 2

		case (plus_token, minus_token)
			prec = 1

		case default
			prec = 0

	end select

end function get_binary_op_prec

!===============================================================================

integer function get_unary_op_prec(kind) result(prec)

	! Get unary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		case (plus_token, minus_token)
			prec = 3

		case default
			prec = 0

	end select

end function get_unary_op_prec

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	logical :: bool

	type(syntax_token_t) :: num, left, right, keyword

	if (debug > 1) print *, 'parse_primary_expr'

	select case (parser%current_kind())

		case (lparen_token)

			left  = parser%next()
			expr  = parser%parse_expr()
			right = parser%match(rparen_token)

			return

		case (true_keyword, false_keyword)

			keyword = parser%next()
			bool = keyword%kind == true_keyword
			expr = new_bool_expr(bool)

		case default

			num = parser%match(num_token)
			expr = new_num_expr(num)

			if (debug > 1) print *, 'num = ', expr%num%val

	end select

end function parse_primary_expr

!===============================================================================

function new_bool_expr(bool) result(expr)

	logical :: bool

	type(syntax_node_t) :: expr

	!********

	type(value_t) :: val

	print *, 'new_bool_expr'

	!val%kind = bool_expr
	val%bval = bool

	expr%kind = bool_expr
	expr%val  = val
	!expr%num  = num

end function new_bool_expr

! TODO: combine new_bool_expr() and new_num_expr() into a general
! new_literal_exp() fn

function new_num_expr(num) result(expr)

	type(syntax_token_t) :: num

	type(syntax_node_t) :: expr

	!********

	expr%kind = num_expr
	expr%num  = num

end function new_num_expr

!===============================================================================

function new_binary_expr(left, op, right) result(expr)

	type(syntax_node_t) , intent(in) :: left, right
	type(syntax_token_t), intent(in) :: op

	type(syntax_node_t) :: expr

	!********

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'left  = ', left %str()
	if (debug > 1) print *, 'right = ', right%str()

	expr%kind = binary_expr

	allocate(expr%left)
	allocate(expr%right)

	expr%left  = left
	expr%op    = op
	expr%right = right

	if (debug > 1) print *, 'new_binary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_binary_expr'

end function new_binary_expr

!===============================================================================

function new_unary_expr(op, right) result(expr)

	type(syntax_node_t) , intent(in) :: right
	type(syntax_token_t), intent(in) :: op

	type(syntax_node_t) :: expr

	!********

	if (debug > 1) print *, 'new_unary_expr'

	expr%kind = unary_expr

	allocate(expr%right)

	expr%op    = op
	expr%right = right

	if (debug > 1) print *, 'new_unary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_unary_expr'

end function new_unary_expr

!===============================================================================

function match(parser, kind) result(token)

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t) :: token

	!********

	integer :: len_text

	type(syntax_token_t) :: current

	! If current_text() and current_pos() helper fns are added, this local var
	! current can be eliminated
	current = parser%current()

	if (parser%current_kind() == kind) then
		token = parser%next()
		return
	end if

	len_text = max(len(current%text), 1)

	call parser%diagnostics%push( &
			repeat(' ', current%pos-1)//fg_bright_red &
			//repeat('^', len_text)//color_reset//line_feed &
			//fg_bold_bright_red//'Error'//color_reset &
			//fg_bold//': unexpected token "'//current%text//'"' &
			//' (kind <'//kind_name(parser%current_kind())//'>)' &
			//', expected <'//kind_name(kind)//'>' &
			//color_reset &
			)

	token = new_token(kind, current%pos, null_char)

end function match

!===============================================================================

function current_token(parser)

	! Refactor in terms of peek(0) ?
	class(parser_t) :: parser
	type(syntax_token_t) :: current_token

	if (debug > 2) print *, 'current_token pos ', parser%pos

	if (parser%pos > size(parser%tokens)) then
		current_token = parser%tokens( size(parser%tokens) )
		return
	end if

	current_token = parser%tokens( parser%pos )

end function current_token

!===============================================================================

function next_parser_token(parser) result(next)
	class(parser_t) :: parser
	type(syntax_token_t) :: next
	next = parser%current()
	parser%pos = parser%pos + 1
end function next_parser_token

!===============================================================================

recursive function syntax_eval(node) result(res)

	type(syntax_node_t) :: node

	integer :: res

	!********

	! TODO: polymorphic value types for general num/bool/etc.
	integer :: left, right

	if (node%kind == num_expr) then
		res = node%num%val
		return
	end if

	if (node%kind == unary_expr) then

		right = syntax_eval(node%right)
		!print *, 'right = ', right

		if      (node%op%kind == plus_token) then
			res =  right
		else if (node%op%kind == minus_token) then
			res = -right
		else

			! Anything here should have been caught by a parser diagnostic
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected unary operator "' &
					//node%op%text//'"'//color_reset

			res = 0

		end if

		return

	end if

	if (node%kind == binary_expr) then

		left  = syntax_eval(node%left )
		right = syntax_eval(node%right)

		!print *, 'left  = ', left
		!print *, 'right = ', right

		if      (node%op%kind == plus_token) then
			res = left + right
		else if (node%op%kind == minus_token) then
			res = left - right
		else if (node%op%kind == star_token) then
			res = left * right
		else if (node%op%kind == slash_token) then
			res = left / right
		else

			! Anything here should have been caught by a parser diagnostic
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected binary operator "' &
					//node%op%text//'"'//color_reset

			!! This is catastrophic, but it kills the unit tests
			!stop

			res = 0

		end if

		return

	end if

	res = 0
	write(*,*) fg_bold_bright_red//'Error'//color_reset &
			//fg_bold//': unexpected node "'//kind_name(node%kind) &
			//'"'//color_reset

	!stop

end function syntax_eval

!===============================================================================

subroutine interpret()

	! This is the interpreter shell
	!
	! TODO: arg for iu as stdin vs another file:
	!   - enable input echo for file input (not for stdin)
	!   - write file name and line num for diagnostics

	character(len = :), allocatable :: line
	character(len = *), parameter :: prompt = lang_name//'$ '

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io, res

	type(syntax_node_t) :: tree

	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	! Read-eval-print-loop
	do
		write(ou, '(a)', advance = 'no') prompt
		line = read_line(iu, iostat = io)

		!print *, 'line = <', line, '>'
		!print *, 'io = ', io

		!! Echo input?
		!write(ou, '(a)') line

		if (io == iostat_end) exit

		! Skip empty lines
		if (len(line) == 0) cycle

		tree = syntax_parse(line)

		! I'm skipping the the binder that Immo implemented at this point in
		! episode 2.  I guess I'll find out later if that's a stupid decision on
		! my end.  I think I can just do type checking in the parser

		if (debug > 0) print *, 'tree = ', tree%str()

		call tree%log_diagnostics(line, ou)

		! Don't try to evaluate with errors
		if (tree%diagnostics%len > 0) cycle

		res  = syntax_eval(tree)

		! Consider MATLAB-style "ans = " log?
		write(ou, '(i0)') res

	end do

end subroutine interpret

!===============================================================================

subroutine log_diagnostics(node, line, ou)

	class(syntax_node_t), intent(in) :: node
	character(len = *)  , intent(in) :: line
	integer, optional   , intent(in) :: ou

	!********

	integer :: i, oul

	oul = output_unit
	if (present(ou)) oul = ou

	do i = 1, node%diagnostics%len
		! Check rustc conventions for style of line numbers in diagnostics
		write(oul,*)
		write(oul, '(a)') line
		write(oul, '(a)') node%diagnostics%v(i)%s
		write(oul,*)
	end do

end subroutine log_diagnostics

!===============================================================================

integer function eval(str)

	character(len = *), intent(in) :: str

	type(syntax_node_t) :: tree

	!! One-liner, but no error handling.  This can crash unit tests without
	!! reporting failures
	!eval = syntax_eval(syntax_parse(str))

	tree = syntax_parse(str)
	call tree%log_diagnostics(str)

	if (tree%diagnostics%len > 0) then
		eval = 0
		return
	end if

	eval = syntax_eval(tree)

end function eval

!===============================================================================

end module core_m

!===============================================================================

