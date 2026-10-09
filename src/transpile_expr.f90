
!===============================================================================

submodule (syntran__transpile_m) syntran__transpile_expr

	! Fortran backend: expressions.
	!
	! Every expression is emitted fully parenthesized because the tree has no
	! paren nodes, so precedence is not recoverable (nor does it need to be,
	! since Fortran's precedence differs from syntran's anyway)
	!
	! Types are made explicit.  Wherever syntran implicitly widens an operand
	! (e.g. i32 + f64), the emitted code converts it to the type of the whole
	! expression with convert() instead of leaving it to Fortran's own mixed-kind
	! rules
	!
	! Arrays are Fortran allocatable arrays of the same rank.  syntran indexes
	! from 0 and Fortran from 1, so every subscript gets a `+ 1`.  A syntran
	! range's upper bound is exclusive, which is the same number as an inclusive
	! 1-based Fortran bound, so `a[l:u]` is `a(l+1:u)`
	!
	! An array of strings is an array of rt_str_t, which wraps a string

	implicit none

!===============================================================================

contains

!===============================================================================

function avoid_runtime_prefix(base) result(r)

	! Every name in the runtime starts with `rt_`, and some of them end like a
	! generated fn's, e.g. rt_str_f32.  A user's own name that starts the same
	! way gets another prefix, so the two can never be the same.  Fortran isn't
	! case sensitive

	character(len = *), intent(in) :: base
	character(len = :), allocatable :: r

	r = base
	if (len(base) < 3) return

	if ((base(1:1) == 'r' .or. base(1:1) == 'R') .and. &
	    (base(2:2) == 't' .or. base(2:2) == 'T') .and. base(3:3) == '_') then
		r = 'u'//base
	end if

end function avoid_runtime_prefix

!===============================================================================

module function slot_name(name, is_loc, id) result(s)

	! The name is made unique by its slot id, which is already unique among a
	! fn's bindings (or the globals) even when a block re-declares the same name
	! with a different type.  That also means that Fortran's case insensitivity
	! and its keywords can never clash.  None of the runtime's names (rt_*) nor
	! temporaries' (*_t<n>) match the `_g<n>`/`_l<n>` suffix

	character(len = *), intent(in) :: name
	logical, intent(in) :: is_loc
	integer, intent(in) :: id
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: base

	integer :: i

	base = ''
	do i = 1, len(name)
		if (is_name_char(name(i:i))) then
			base = base//name(i:i)
		else
			base = base//'_'
		end if
	end do

	! Fortran names start with a letter and are at most 63 chars long
	if (len(base) == 0) then
		base = 'v'
	else if (.not. ((base(1:1) >= 'a' .and. base(1:1) <= 'z') .or. &
	                (base(1:1) >= 'A' .and. base(1:1) <= 'Z'))) then
		base = 'u'//base
	end if
	base = avoid_runtime_prefix(base)
	if (len(base) > 40) base = base(1: 40)

	s = base//merge('_l', '_g', is_loc)//str(id)

end function slot_name

!===============================================================================

module function var_name(node) result(s)

	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	s = slot_name(node%identifier%text, node%is_loc, node%id_index)

end function var_name

!===============================================================================

module function fn_name(name, id) result(s)

	character(len = *), intent(in) :: name
	integer, intent(in) :: id
	character(len = :), allocatable :: s

	character(len = :), allocatable :: base

	integer :: i

	base = ''
	do i = 1, len(name)
		if (is_name_char(name(i:i))) then
			base = base//name(i:i)
		else
			base = base//'_'
		end if
	end do

	if (len(base) == 0) then
		base = 'fn'
	else if (.not. ((base(1:1) >= 'a' .and. base(1:1) <= 'z') .or. &
	                (base(1:1) >= 'A' .and. base(1:1) <= 'Z'))) then
		base = 'u'//base
	end if
	base = avoid_runtime_prefix(base)
	if (len(base) > 40) base = base(1: 40)

	s = base//'_f'//str(id)

end function fn_name

!===============================================================================



!===============================================================================



!===============================================================================

module function decl_line(val, name, ok) result(s)

	type(value_t), intent(in) :: val
	character(len = *), intent(in) :: name
	logical, intent(out) :: ok
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: dims, spec

	integer :: i, t

	ok = .true.
	dims = ''

	t = val%type
	if (val%type == array_type) then

		if (.not. allocated(val%array)) then
			ok = .false.
			s = ''
			return
		end if

		if (val%array%rank < 1) then
			ok = .false.
			s = ''
			return
		end if

		t = val%array%type
		dims = '('
		do i = 1, val%array%rank
			if (i > 1) dims = dims//','
			dims = dims//':'
		end do
		dims = dims//')'

	end if

	select case (t)
	case (i32_type)
		spec = 'integer(int32)'
	case (i64_type)
		spec = 'integer(int64)'
	case (f32_type)
		spec = 'real(real32)'
	case (f64_type)
		spec = 'real(real64)'
	case (bool_type)
		spec = 'logical'
	case (str_type)
		if (val%type == array_type) then
			spec = 'type(rt_str_t)'
		else
			spec = 'character(len = :)'
		end if
	case default
		ok = .false.
		s = ''
		return
	end select

	if (val%type == array_type .or. t == str_type) then
		s = spec//', allocatable :: '//name//dims
	else
		s = spec//' :: '//name
	end if

end function decl_line

!===============================================================================

module subroutine declare_var(em, node, val)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(value_t), intent(in) :: val

	!********

	character(len = :), allocatable :: line

	integer :: i, key

	logical :: ok

	key = 2 * node%id_index
	if (node%is_loc) key = key + 1

	if (node%is_loc) then
		do i = 1, em%seen_local%len_
			if (em%seen_local%v(i) == key) return
		end do
		call em%seen_local%push(key)
	else
		do i = 1, em%seen_global%len_
			if (em%seen_global%v(i) == key) return
		end do
		call em%seen_global%push(key)
	end if

	call record_slot(em, node%is_loc, node%id_index, val)

	line = decl_line(val, var_name(node), ok)
	if (.not. ok) then
		call em_unsupported(em, 'a variable of type `'//kind_name(val%type)//'`')
		return
	end if

	if (node%is_loc) then
		call em%decls%push(line)
	else
		call em%gdecls%push(line)
	end if

end subroutine declare_var

!===============================================================================

module function new_tmp(em, type_str, prefix, dims) result(name)

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: type_str, prefix
	character(len = *), intent(in), optional :: dims
	character(len = :), allocatable :: name

	em%tmp_count = em%tmp_count + 1
	name = prefix//'_t'//str(em%tmp_count)

	if (present(dims)) then
		call em%decls%push(type_str//' :: '//name//dims)
	else
		call em%decls%push(type_str//' :: '//name)
	end if

end function new_tmp

!===============================================================================

module function convert(s, from, to) result(r)

	! Works elementwise on arrays too, in which case `from` and `to` are the
	! element types

	character(len = *), intent(in) :: s
	integer, intent(in) :: from, to
	character(len = :), allocatable :: r

	r = s
	if (from == to) return

	select case (to)
	case (i32_type)
		r = 'int('//s//', int32)'
	case (i64_type)
		r = 'int('//s//', int64)'
	case (f32_type)
		r = 'real('//s//', real32)'
	case (f64_type)
		r = 'real('//s//', real64)'
	end select

end function convert

!===============================================================================



!===============================================================================



!===============================================================================



!===============================================================================

function str_literal(s) result(r)

	! A Fortran character expression for the string s.  Quotes are doubled and
	! anything not printable, like a newline, is spliced in with char().  Long
	! strings are split up so that no source line gets too long

	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r

	!********

	character(len = :), allocatable :: seg

	integer :: i, n

	r = ''
	seg = ''

	do i = 1, len(s)
		n = iachar(s(i:i))

		if (n >= 32 .and. n < 127) then
			if (s(i:i) == "'") then
				seg = seg//"''"
			else
				seg = seg//s(i:i)
			end if

			if (len(seg) >= 48) then
				if (len(r) > 0) r = r//' // '
				r = r//"'"//seg//"'"
				seg = ''
			end if

		else
			if (len(seg) > 0) then
				if (len(r) > 0) r = r//' // '
				r = r//"'"//seg//"'"
				seg = ''
			end if
			if (len(r) > 0) r = r//' // '
			r = r//'char('//str(n)//')'

		end if
	end do

	if (len(seg) > 0) then
		if (len(r) > 0) r = r//' // '
		r = r//"'"//seg//"'"
	end if

	if (len(r) == 0) r = "''"

end function str_literal

!===============================================================================

function emit_literal(em, node) result(s)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	!********

	character(len = 32) :: buf

	associate (val => node%val)
		select case (val%type)

		case (i32_type)
			if (val%sca%i32 == -huge(val%sca%i32) - 1) then
				! The magnitude of the smallest value doesn't fit in the type, so a
				! literal for it is out of range in Fortran
				s = '(-'//str(huge(val%sca%i32))//'_int32 - 1_int32)'
			else
				s = str(val%sca%i32)//'_int32'
			end if

		case (i64_type)
			if (val%sca%i64 == -huge(val%sca%i64) - 1) then
				s = '(-'//str(huge(val%sca%i64))//'_int64 - 1_int64)'
			else
				s = str(val%sca%i64)//'_int64'
			end if

		case (f32_type)
			! Round-trip precision.  Three exponent digits so that the same
			! format would also work for f64
			write(buf, '(es17.8e3)') val%sca%f32
			s = trim(adjustl(buf))//'_real32'

		case (f64_type)
			write(buf, '(es26.16e3)') val%sca%f64
			s = trim(adjustl(buf))//'_real64'

		case (bool_type)
			if (val%sca%bool) then
				s = '.true.'
			else
				s = '.false.'
			end if

		case (str_type)
			s = str_literal(val%str%s)

		case default
			call em_unsupported(em, 'a literal of type `'//kind_name(val%type)//'`')
			s = '0'

		end select
	end associate

	! A negative literal can't be a bare operand of some Fortran operators
	if (len(s) > 0) then
		if (s(1:1) == '-') s = '('//s//')'
	end if

end function emit_literal

!===============================================================================

function as_rt_str(s, is_array) result(r)

	! A string operand of an elementwise operation on arrays of strings, which
	! are arrays of rt_str_t.  A lone string needs to be wrapped to match

	character(len = *), intent(in) :: s
	logical, intent(in) :: is_array
	character(len = :), allocatable :: r

	if (is_array) then
		r = s
	else
		r = 'rt_str_t_of('//s//')'
	end if

end function as_rt_str

!===============================================================================

function emit_str_binary(em, node, l, r) result(s)

	! A binary operator where both operands are strings, or arrays of strings

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = *), intent(in) :: l, r
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: la, ra

	logical :: elem

	elem = is_arr(node%left%val) .or. is_arr(node%right%val)

	if (elem) then
		la = as_rt_str(l, is_arr(node%left%val))
		ra = as_rt_str(r, is_arr(node%right%val))

		select case (node%op%kind)
		case (plus_token)
			s = 'rt_cat('//la//', '//ra//')'
		case (eequals_token)
			s = 'rt_eq('//la//', '//ra//')'
		case (bang_equals_token)
			s = '(.not. rt_eq('//la//', '//ra//'))'
		case (less_token)
			s = 'rt_lt('//la//', '//ra//')'
		case (less_equals_token)
			s = '(.not. rt_lt('//ra//', '//la//'))'
		case (greater_token)
			s = 'rt_lt('//ra//', '//la//')'
		case (greater_equals_token)
			s = '(.not. rt_lt('//la//', '//ra//'))'
		case default
			call em_unsupported(em, 'the `'//node%op%text//'` operator on strings')
			s = '0'
		end select

	else
		select case (node%op%kind)
		case (plus_token)
			s = '('//l//' // '//r//')'
		case (eequals_token)
			s = 'rt_str_eq('//l//', '//r//')'
		case (bang_equals_token)
			s = '(.not. rt_str_eq('//l//', '//r//'))'
		case (less_token)
			s = 'rt_str_lt('//l//', '//r//')'
		case (less_equals_token)
			s = '(.not. rt_str_lt('//r//', '//l//'))'
		case (greater_token)
			s = 'rt_str_lt('//r//', '//l//')'
		case (greater_equals_token)
			s = '(.not. rt_str_lt('//l//', '//r//'))'
		case default
			call em_unsupported(em, 'the `'//node%op%text//'` operator on strings')
			s = '0'
		end select
	end if

end function emit_str_binary

!===============================================================================

function emit_pow(l, r, lt, rt, restype) result(s)

	! `l ** r`.  The operands are not both converted to the result type, because
	! a real to an integer power is not the same operation as a real to a real
	! power.  This is what the interpreter does, which relies on Fortran's own
	! mixed-type rules, with one exception: an i64 next to a real is first
	! converted to that real's kind

	character(len = *), intent(in) :: l, r
	integer, intent(in) :: lt, rt, restype
	character(len = :), allocatable :: s

	character(len = :), allocatable :: l2, r2

	integer :: nt

	l2 = l
	r2 = r

	if (rt == i64_type .and. (lt == f32_type .or. lt == f64_type)) then
		r2 = convert(r, i64_type, lt)
	end if
	if (lt == i64_type .and. (rt == f32_type .or. rt == f64_type)) then
		l2 = convert(l, i64_type, rt)
	end if

	! The type that Fortran gives the power
	if (lt == f64_type .or. rt == f64_type) then
		nt = f64_type
	else if (lt == f32_type .or. rt == f32_type) then
		nt = f32_type
	else
		nt = wider_type(lt, rt)
	end if

	s = convert('('//l2//' ** '//r2//')', nt, restype)

end function emit_pow

!===============================================================================

recursive function emit_concat(em, node) result(s)

	! A chain of string concatenations `a + b + c + ...`, as one Fortran
	! concatenation instead of a nest of parentheses.  The caller wraps it

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	if (is_str_concat(node)) then
		s = emit_concat(em, node%left)//' // '//emit_concat(em, node%right)
	else
		s = emit_expr(em, node)
	end if

end function emit_concat

!===============================================================================

function is_str_concat(node) result(is_cat)

	! Is this a `+` of two single strings?

	type(syntax_node_t), intent(in) :: node
	logical :: is_cat

	is_cat = .false.
	if (node%kind /= binary_expr) return
	if (node%op%kind /= plus_token) return
	if (node%val%type /= str_type) return
	if (node%left %val%type /= str_type) return
	if (node%right%val%type /= str_type) return
	is_cat = .true.

end function is_str_concat

!===============================================================================

recursive function emit_binary(em, node) result(s)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: l, r

	integer :: lt, rt, ct, restype

	if (is_str_concat(node)) then
		s = '('//emit_concat(em, node)//')'
		return
	end if

	lt = elem_type(node%left %val)
	rt = elem_type(node%right%val)
	restype = elem_type(node%val)

	l = emit_expr(em, node%left)
	r = emit_expr(em, node%right)

	if (node%op%kind == matmul_token) then
		ct = wider_type(lt, rt)
		if (.not. is_numeric_type(ct)) then
			call em_unsupported(em, 'a matrix product of type `'//kind_name(ct)//'`')
			s = '0'
			return
		end if
		if (is_arr(node%left%val) .and. is_arr(node%right%val)) then
			if (node%left%val%array%rank == 1 .and. node%right%val%array%rank == 1) then
				! Two vectors are a dot product, which is a scalar
				s = 'dot_product('//convert(l, lt, ct)//', '//convert(r, rt, ct)//')'
				return
			end if
		end if
		s = 'matmul('//convert(l, lt, ct)//', '//convert(r, rt, ct)//')'
		return
	end if

	if (lt == str_type .and. rt == str_type) then
		s = emit_str_binary(em, node, l, r)
		return
	end if

	select case (node%op%kind)

	case (plus_token, minus_token, star_token, slash_token, sstar_token, &
			percent_token)

		if (.not. is_numeric_type(restype)) then
			call em_unsupported(em, 'a `'//node%op%text//'` operation on type `'// &
				kind_name(restype)//'`')
			s = '0'
			return
		end if

		if (node%op%kind == sstar_token) then
			s = emit_pow(l, r, lt, rt, restype)
			return
		end if

		l = convert(l, lt, restype)
		r = convert(r, rt, restype)

		select case (node%op%kind)
		case (plus_token)
			s = '('//l//' + '//r//')'
		case (minus_token)
			s = '('//l//' - '//r//')'
		case (star_token)
			s = '('//l//' * '//r//')'
		case (slash_token)
			s = '('//l//' / '//r//')'
		case (percent_token)
			! Fortran's mod() is consistent with C's `%`, like syntran's
			s = 'mod('//l//', '//r//')'
		end select

	case (eequals_token, bang_equals_token, less_token, less_equals_token, &
			greater_token, greater_equals_token)

		if (lt == bool_type .and. rt == bool_type) then
			select case (node%op%kind)
			case (eequals_token)
				s = '('//l//' .eqv. '//r//')'
			case (bang_equals_token)
				s = '('//l//' .neqv. '//r//')'
			case default
				call em_unsupported(em, 'ordering comparison of booleans')
				s = '.false.'
			end select
			return
		end if

		if (.not. (is_numeric_type(lt) .and. is_numeric_type(rt))) then
			call em_unsupported(em, 'a comparison of type `'//kind_name(lt)//'`')
			s = '.false.'
			return
		end if

		ct = wider_type(lt, rt)
		l = convert(l, lt, ct)
		r = convert(r, rt, ct)

		select case (node%op%kind)
		case (eequals_token)
			s = '('//l//' == '//r//')'
		case (bang_equals_token)
			s = '('//l//' /= '//r//')'
		case (less_token)
			s = '('//l//' < '//r//')'
		case (less_equals_token)
			s = '('//l//' <= '//r//')'
		case (greater_token)
			s = '('//l//' > '//r//')'
		case (greater_equals_token)
			s = '('//l//' >= '//r//')'
		end select

	case (and_keyword)
		s = '('//l//' .and. '//r//')'

	case (or_keyword)
		s = '('//l//' .or. '//r//')'

	case (amp_token, pipe_token, caret_token)
		if (.not. (lt == i32_type .or. lt == i64_type)) then
			call em_unsupported(em, 'a bitwise operation on type `'//kind_name(lt)//'`')
			s = '0'
			return
		end if

		ct = wider_type(lt, rt)
		l = convert(l, lt, ct)
		r = convert(r, rt, ct)

		select case (node%op%kind)
		case (amp_token)
			s = 'iand('//l//', '//r//')'
		case (pipe_token)
			s = 'ior('//l//', '//r//')'
		case (caret_token)
			s = 'ieor('//l//', '//r//')'
		end select

	case (lless_token, ggreater_token)
		if (.not. (lt == i32_type .or. lt == i64_type)) then
			call em_unsupported(em, 'a bitwise shift of type `'//kind_name(lt)//'`')
			s = '0'
			return
		end if

		! The shift is done in the width of the left operand, then widened if
		! the right operand is an i64.  Both are logical shifts, so a `>>` doesn't
		! extend the sign bit
		if (node%op%kind == lless_token) then
			s = convert('shiftl('//l//', '//r//')', lt, restype)
		else
			s = convert('shiftr('//l//', '//r//')', lt, restype)
		end if

	case default
		call em_unsupported(em, 'the `'//node%op%text//'` operator')
		s = '0'

	end select

end function emit_binary

!===============================================================================

recursive function emit_unary(em, node) result(s)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: r

	r = emit_expr(em, node%right)

	select case (node%op%kind)
	case (minus_token)
		s = '(-'//r//')'
	case (plus_token)
		s = '(+'//r//')'
	case (not_keyword)
		s = '(.not. '//r//')'
	case (bang_token)
		s = 'not('//r//')'
	case default
		call em_unsupported(em, 'the `'//node%op%text//'` operator')
		s = '0'
	end select

end function emit_unary

!===============================================================================



!===============================================================================

recursive function int_literal(node, val) result(is_lit)

	! Is this an integer literal, possibly with a sign?  The parser keeps the
	! sign of `-1` as a unary operator.  If so, its value is `val`

	type(syntax_node_t), intent(in) :: node
	integer(kind = 8), intent(out) :: val
	logical :: is_lit

	integer(kind = 8) :: inner

	is_lit = .false.
	val = 0

	if (node%kind == literal_expr) then
		if (node%val%type == i32_type) then
			val = node%val%sca%i32
			is_lit = .true.
		else if (node%val%type == i64_type) then
			val = node%val%sca%i64
			is_lit = .true.
		end if

	else if (node%kind == unary_expr) then
		if (node%op%kind == minus_token .or. node%op%kind == plus_token) then
			if (int_literal(node%right, inner)) then
				is_lit = .true.
				val = inner
				if (node%op%kind == minus_token) val = -inner
			end if
		end if
	end if

end function int_literal

!===============================================================================

recursive function plus_one(em, node) result(s)

	! The 1-based Fortran index for a 0-based syntran index expression

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	s = ''

	if (node%kind == literal_expr) then
		if (node%val%type == i32_type) then
			s = str(node%val%sca%i32 + 1)//'_int32'
		else if (node%val%type == i64_type) then
			s = str(node%val%sca%i64 + 1_8)//'_int64'
		end if
		if (len(s) > 0) then
			if (s(1:1) == '-') s = '('//s//')'
			return
		end if
	end if

	s = '('//emit_expr(em, node)//' + 1)'

end function plus_one

!===============================================================================

recursive function emit_dim_sub(em, node, i, base) result(s)

	! The Fortran subscript for dimension i of a subscripted array
	!
	! `base` is the array's own designator, which is only used to find the size
	! of the dimension for a slice with an omitted bound

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: i
	character(len = *), intent(in) :: base
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: lo, hi, st, size_str, lb, ub
	integer(kind = 8) :: step_lit
	logical :: has_lit

	associate (sub => node%lsubscripts(i))

		select case (sub%sub_kind)

		case (scalar_sub)
			s = plus_one(em, sub)

		case (all_sub)
			s = ':'

		case (range_sub)
			lo = ''
			hi = ''
			if (.not. sub%lsub_omit) lo = plus_one(em, sub)
			if (.not. sub%usub_omit) hi = emit_expr(em, node%usubscripts(i))
			s = lo//':'//hi

		case (step_sub)

			associate (step => node%ssubscripts(i))

				has_lit = int_literal(step, step_lit)

				if (has_lit .and. step_lit > 0) then
					! Same as Fortran's own triplet
					lo = ''
					hi = ''
					if (.not. sub%lsub_omit) lo = plus_one(em, sub)
					if (.not. sub%usub_omit) hi = emit_expr(em, node%usubscripts(i))
					s = lo//':'//hi//':'//str(step_lit)
					return
				end if

				! Any other step, which may be negative at run time.  The
				! subscript is the vector of indices [lb: step: ub], which is also
				! how the omitted bounds depend on the sign of the step
				if (.not. is_simple(step) .and. .not. has_lit) then
					call em_unsupported(em, 'a slice with a step that is not a variable or literal')
					s = ':'
					return
				end if

				st = convert(emit_expr(em, step), elem_type(step%val), i32_type)

				size_str = 'size('//base//', '//str(i)//', kind = int32)'

				if (sub%lsub_omit) then
					lb = 'merge('//size_str//' - 1_int32, 0_int32, '//st//' < 0_int32)'
				else
					lb = convert(emit_expr(em, sub), elem_type(sub%val), i32_type)
				end if

				if (sub%usub_omit) then
					ub = 'merge(-1_int32, '//size_str//', '//st//' < 0_int32)'
				else
					ub = convert(emit_expr(em, node%usubscripts(i)), &
						elem_type(node%usubscripts(i)%val), i32_type)
				end if

				s = '(rt_step_i32('//lb//', '//st//', '//ub//') + 1_int32)'

			end associate

		case (arr_sub)
			! A vector subscript: the elements of an index array
			s = '('//emit_expr(em, sub)//' + 1)'

		case default
			call em_unsupported(em, 'this kind of subscript')
			s = ':'

		end select

	end associate

end function emit_dim_sub

!===============================================================================

recursive function emit_char_sub(em, node, i, do_hoist) result(s)

	! The substring bounds `lo:hi` for subscript i of a string (without the
	! parens).  syntran strings are subscripted by a 0-based index or range

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: i
	logical, intent(in) :: do_hoist
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: lo, hi, tmp

	associate (sub => node%lsubscripts(i))

		select case (sub%sub_kind)

		case (scalar_sub)
			if (.not. is_simple(sub)) then
				if (.not. do_hoist) then
					! The index would be evaluated twice, since both ends of the
					! substring are the index
					call em_unsupported(em, &
						'a string index that is not a variable or literal')
					s = ':'
					return
				end if

				tmp = new_tmp(em, 'integer(int64)', 'idx')
				call em_line(em, tmp//' = '//emit_expr(em, sub))
				lo = '('//tmp//' + 1)'
			else
				lo = plus_one(em, sub)
			end if

			s = lo//':'//lo

		case (range_sub)
			lo = ''
			hi = ''
			if (.not. sub%lsub_omit) lo = plus_one(em, sub)
			if (.not. sub%usub_omit) hi = emit_expr(em, node%usubscripts(i))
			s = lo//':'//hi

		case (all_sub)
			s = ':'

		case default
			call em_unsupported(em, 'this kind of string subscript')
			s = ':'

		end select

	end associate

end function emit_char_sub

!===============================================================================

recursive module function emit_name_ref(em, node, hoist, target) result(s)

	! The Fortran designator of a reference to a variable: a name_expr, or the
	! target of an assignment_expr, which has the same fields.  `hoist` is for
	! a compound assignment, which uses its target twice: its subscripts are
	! first evaluated into temporaries if they have side effects.  `target` says
	! that the reference is assigned to, so it must be a variable and not a fn
	! result like rt_char_at()

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in), optional :: hoist, target
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: base, subs, tmp

	integer :: i, nsub, rank_
	logical :: do_hoist, str_hoist, all_scalar

	type(slot_info_t) :: info

	base = var_name(node)
	s = base

	if (.not. allocated(node%lsubscripts)) return

	do_hoist = .false.
	if (present(hoist)) do_hoist = hoist

	! A string's index is used twice, as both ends of its substring.  So for
	! an assignment target, it's also evaluated up front
	str_hoist = do_hoist
	if (present(target)) str_hoist = str_hoist .or. target

	nsub = size(node%lsubscripts)

	info = lookup_slot(em, node%is_loc, node%id_index)
	if (.not. info%known) then
		call em_unsupported(em, 'a subscript of a variable whose type is unknown')
		return
	end if

	if (info%type == str_type) then
		! A character or substring
		if (nsub /= 1) then
			call em_unsupported(em, 'this subscript of a string')
			return
		end if

		! A character at an index which is an expression is a fn, so that the
		! index isn't evaluated twice.  That's not possible for an assignment
		! target
		if (node%lsubscripts(1)%sub_kind == scalar_sub .and. .not. str_hoist .and. &
				.not. is_simple(node%lsubscripts(1))) then
			s = 'rt_char_at('//base//', '//convert(emit_expr(em, node%lsubscripts(1)), &
				elem_type(node%lsubscripts(1)%val), i64_type)//')'
			return
		end if

		s = base//'('//emit_char_sub(em, node, 1, str_hoist)//')'
		return
	end if

	if (info%type /= array_type) then
		call em_unsupported(em, 'a subscript of a variable of type `'// &
			kind_name(info%type)//'`')
		return
	end if

	rank_ = info%rank

	! A string array has one more subscript, which indexes a character of the
	! element
	if (info%elem == str_type .and. nsub == rank_ + 1) then

		subs = ''
		do i = 1, rank_
			if (node%lsubscripts(i)%sub_kind /= scalar_sub) then
				call em_unsupported(em, 'a substring of a slice of strings')
				return
			end if
			if (i > 1) subs = subs//', '
			subs = subs//emit_dim_sub(em, node, i, base)
		end do

		if (node%lsubscripts(nsub)%sub_kind == scalar_sub .and. .not. str_hoist .and. &
				.not. is_simple(node%lsubscripts(nsub))) then
			s = 'rt_char_at('//base//'('//subs//')%s, '// &
				convert(emit_expr(em, node%lsubscripts(nsub)), &
				elem_type(node%lsubscripts(nsub)%val), i64_type)//')'
			return
		end if

		s = base//'('//subs//')%s('//emit_char_sub(em, node, nsub, str_hoist)//')'
		return
	end if

	if (nsub /= rank_) then
		call em_unsupported(em, 'a subscript with the wrong number of dimensions')
		return
	end if

	all_scalar = .true.
	subs = ''
	do i = 1, nsub
		if (node%lsubscripts(i)%sub_kind /= scalar_sub) all_scalar = .false.
		if (i > 1) subs = subs//', '

		if (do_hoist .and. node%lsubscripts(i)%sub_kind == scalar_sub .and. &
				.not. is_simple(node%lsubscripts(i))) then
			tmp = new_tmp(em, 'integer(int64)', 'idx')
			call em_line(em, tmp//' = '//emit_expr(em, node%lsubscripts(i)))
			subs = subs//'('//tmp//' + 1)'
		else
			subs = subs//emit_dim_sub(em, node, i, base)
		end if
	end do

	s = base//'('//subs//')'

	! An element of a string array is the string inside of the wrapper
	if (info%elem == str_type .and. all_scalar) s = s//'%s'

end function emit_name_ref

!===============================================================================

function reshape_str(em, a, rank_, shape_str) result(s)

	! Fortran's reshape() can't be used on an array of strings, so it's done by
	! a runtime fn for each rank

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: a, shape_str
	integer, intent(in) :: rank_
	character(len = :), allocatable :: s

	if (rank_ < 2 .or. rank_ > 4) then
		call em_unsupported(em, 'an array of strings of rank '//str(rank_))
		s = a
		return
	end if

	s = 'rt_reshape_str_'//str(rank_)//'('//a//', '//shape_str//')'

end function reshape_str

!===============================================================================

recursive function emit_array_expr(em, node) result(s)

	! An array literal, one of the forms that array_expr nodes have

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: v, n_str, shape_str, elems, spec
	character(len = :), allocatable :: lb, ub, st, sfx

	integer :: i, t, kind_

	t = node%val%array%type
	kind_ = node%val%array%kind

	select case (kind_)

	case (bound_array)
		if (t /= i32_type .and. t /= i64_type) then
			call em_unsupported(em, 'a range of type `'//kind_name(t)//'`')
			s = '[0]'
			return
		end if
		sfx = merge('i32', 'i64', t == i32_type)
		lb = convert(emit_expr(em, node%lbound_), elem_type(node%lbound_%val), t)
		ub = convert(emit_expr(em, node%ubound_), elem_type(node%ubound_%val), t)
		s = 'rt_range_'//sfx//'('//lb//', '//ub//')'

	case (step_array)
		select case (t)
		case (i32_type)
			sfx = 'i32'
		case (i64_type)
			sfx = 'i64'
		case (f32_type)
			sfx = 'f32'
		case (f64_type)
			sfx = 'f64'
		case default
			call em_unsupported(em, 'a range of type `'//kind_name(t)//'`')
			s = '[0]'
			return
		end select
		lb = convert(emit_expr(em, node%lbound_), elem_type(node%lbound_%val), t)
		st = convert(emit_expr(em, node%step  ), elem_type(node%step  %val), t)
		ub = convert(emit_expr(em, node%ubound_), elem_type(node%ubound_%val), t)
		s = 'rt_step_'//sfx//'('//lb//', '//st//', '//ub//')'

	case (len_array)
		if (t /= f32_type .and. t /= f64_type) then
			call em_unsupported(em, 'a range of type `'//kind_name(t)//'`')
			s = '[0]'
			return
		end if
		sfx = merge('f32', 'f64', t == f32_type)
		lb = convert(emit_expr(em, node%lbound_), elem_type(node%lbound_%val), t)
		ub = convert(emit_expr(em, node%ubound_), elem_type(node%ubound_%val), t)
		n_str = convert(emit_expr(em, node%len_), elem_type(node%len_%val), i64_type)
		s = 'rt_linspace_'//sfx//'('//lb//', '//ub//', '//n_str//')'

	case (unif_array)
		! [v; n, m] is v repeated n * m times, in the shape [n, m]
		v = emit_expr(em, node%lbound_)
		if (t /= str_type) v = convert(v, elem_type(node%lbound_%val), t)

		n_str = ''
		shape_str = ''
		do i = 1, size(node%size_)
			if (i > 1) n_str = n_str//' * '
			n_str = n_str//convert(emit_expr(em, node%size_(i)), &
				elem_type(node%size_(i)%val), i64_type)
		end do
		do i = 1, size(node%size_)
			if (i > 1) shape_str = shape_str//', '
			shape_str = shape_str//convert(emit_expr(em, node%size_(i)), &
				elem_type(node%size_(i)%val), i64_type)
		end do

		if (t == str_type) then
			! spread() and reshape() don't copy strings properly
			s = 'rt_fill_str('//v//', '//n_str//')'
			if (size(node%size_) > 1) s = reshape_str(em, s, size(node%size_), shape_str)
		else if (size(node%size_) == 1) then
			s = 'spread('//v//', 1, '//n_str//')'
		else
			s = 'reshape(spread('//v//', 1, '//n_str//'), [ '//shape_str//' ])'
		end if

	case (expl_array, size_array)
		! A typed array constructor converts each element, and splices in the
		! elements of any element that is itself an array
		select case (t)
		case (i32_type)
			spec = 'integer(int32)'
		case (i64_type)
			spec = 'integer(int64)'
		case (f32_type)
			spec = 'real(real32)'
		case (f64_type)
			spec = 'real(real64)'
		case (bool_type)
			spec = 'logical'
		case (str_type)
			! A derived type in a type-spec is just its name
			spec = 'rt_str_t'
		case default
			call em_unsupported(em, 'an array of type `'//kind_name(t)//'`')
			s = '[0]'
			return
		end select

		elems = ''
		do i = 1, size(node%elems)
			if (i > 1) elems = elems//', '
			if (t == str_type) then
				elems = elems//as_rt_str(emit_expr(em, node%elems(i)), &
					is_arr(node%elems(i)%val))
			else
				elems = elems//emit_expr(em, node%elems(i))
			end if
		end do

		s = '['//spec//' :: '//elems//']'

		if (kind_ == size_array) then
			shape_str = ''
			do i = 1, size(node%size_)
				if (i > 1) shape_str = shape_str//', '
				shape_str = shape_str//convert(emit_expr(em, node%size_(i)), &
					elem_type(node%size_(i)%val), i64_type)
			end do
			if (t == str_type) then
				s = reshape_str(em, s, size(node%size_), shape_str)
			else
				s = 'reshape('//s//', [ '//shape_str//' ])'
			end if
		end if

	case default
		call em_unsupported(em, 'this kind of array literal')
		s = '[0]'

	end select

end function emit_array_expr

!===============================================================================

recursive function emit_user_call(em, node) result(s)

	! Call of a user fn.  By-value arguments are converted to the parameter's
	! type, and by-reference arguments must be plain variables so that Fortran
	! can pass them as an actual argument which the callee may define

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: arg

	integer :: i, ptype

	s = fn_name(node%identifier%text, node%id_index)//'('

	if (allocated(node%args)) then
		do i = 1, size(node%args)

			if (node_is_ref(node, i) .and. .not. param_is_const_ref(em, node%id_index, i)) then
				if (node%args(i)%kind /= name_expr .or. &
						allocated(node%args(i)%lsubscripts)) then
					call em_unsupported(em, 'a by-reference argument that is not a plain variable')
					arg = '0'
				else
					arg = var_name(node%args(i))
				end if

			else
				arg = emit_expr(em, node%args(i))

				! Convert by-value args to the declared parameter type
				if (associated(em%fns)) then
					ptype = elem_type(em%fns%fns(node%id_index)%params(i))
					if (is_numeric_type(ptype) .and. &
							is_numeric_type(elem_type(node%args(i)%val))) then
						arg = convert(arg, elem_type(node%args(i)%val), ptype)
					end if
				end if
			end if

			if (i > 1) s = s//', '
			s = s//arg
		end do
	end if

	s = s//')'

end function emit_user_call

!===============================================================================

function param_is_const_ref(em, fn_id, i) result(is_const)

	! Is parameter i of a user fn declared `&const`?  That is passed by
	! reference, but can't be assigned to, so any expression is an acceptable
	! argument

	type(emitter_t), intent(in) :: em
	integer, intent(in) :: fn_id, i
	logical :: is_const

	is_const = .false.
	if (.not. associated(em%fns)) return
	if (fn_id < 1 .or. fn_id > size(em%fns%fns)) return
	if (.not. allocated(em%fns%fns(fn_id)%node)) return
	if (.not. allocated(em%fns%fns(fn_id)%node%is_const_ref)) return
	if (i > size(em%fns%fns(fn_id)%node%is_const_ref)) return
	is_const = em%fns%fns(fn_id)%node%is_const_ref(i)

end function param_is_const_ref

!===============================================================================

function node_is_ref(node, i) result(is_ref)
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: i
	logical :: is_ref
	is_ref = .false.
	if (allocated(node%is_ref)) is_ref = node%is_ref(i)
end function node_is_ref

!===============================================================================

recursive function emit_intr_call(em, node) result(s)

	! Call of an intrinsic fn which returns a value.  Overloaded intrinsics were
	! given mangled names like `0sum_i32_dim` by the parser, which pick out the
	! overload

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: name, base, suffix, a1, a2, args, kind_str, &
		arg_i

	integer :: i, n, iu, t

	name = node%identifier%text
	n = 0
	if (allocated(node%args)) n = size(node%args)

	! Split a mangled name like `0sum_i32_dim` into `sum` and `i32_dim`
	base = name
	suffix = ''
	if (len(name) > 1) then
		if (name(1:1) == '0') then
			base = name(2:)
			iu = index(base, '_')
			if (iu > 0) then
				suffix = base(iu+1:)
				base = base(1: iu-1)
			end if
		end if
	end if

	a1 = ''
	a2 = ''
	if (n >= 1) a1 = emit_expr(em, node%args(1))
	if (n >= 2) a2 = emit_expr(em, node%args(2))

	select case (base)

	case ('str')
		! Concatenation of each argument's string.  rt_str() is generic over
		! every scalar type
		s = ''
		do i = 1, n
			if (i > 1) s = s//' // '
			if (i == 1) then
				arg_i = a1
			else if (i == 2) then
				arg_i = a2
			else
				arg_i = emit_expr(em, node%args(i))
			end if

			if (is_arr(node%args(i)%val)) then
				s = s//'rt_str_a('//arg_i//')'
			else if (node%args(i)%val%type == str_type) then
				s = s//arg_i
			else
				s = s//'rt_str('//arg_i//')'
			end if
		end do
		if (len(s) == 0) s = "''"
		s = '('//s//')'

	case ('len')
		s = convert('len('//a1//', kind = int64)', i64_type, elem_type(node%val))

	case ('repeat')
		s = 'repeat('//a1//', '//convert(a2, elem_type(node%args(2)%val), i32_type)//')'

	case ('char')
		s = 'achar('//convert(a1, elem_type(node%args(1)%val), i32_type)//')'

	case ('parse_i32', 'parse_i64', 'parse_f32', 'parse_f64')
		s = 'rt_'//base//'('//a1//')'

	case ('size')
		if (n == 2) then
			s = convert('size('//a1//', dim = '//convert(a2, elem_type(node%args(2)%val), &
				i32_type)//' + 1, kind = int64)', i64_type, elem_type(node%val))
		else
			s = convert('size('//a1//', kind = int64)', i64_type, elem_type(node%val))
		end if

	case ('count')
		if (suffix == 'dim') then
			s = 'count('//a1//', dim = '//convert(a2, elem_type(node%args(2)%val), &
				i32_type)//' + 1, kind = int64)'
		else
			s = convert('count('//a1//', kind = int64)', i64_type, elem_type(node%val))
		end if

	case ('all', 'any')
		if (suffix == 'dim') then
			s = base//'('//a1//', dim = '//convert(a2, elem_type(node%args(2)%val), &
				i32_type)//' + 1)'
		else
			s = base//'('//a1//')'
		end if

	case ('sum', 'product', 'minval', 'maxval')
		! suffix is like `i32`, `i32_dim`, `i32_mask`, or `i32_dim_mask`
		args = a1
		select case (suffix(index(suffix, '_') + 1:))
		case ('dim')
			args = args//', dim = '//convert(a2, elem_type(node%args(2)%val), &
				i32_type)//' + 1'
		case ('mask')
			args = args//', mask = '//a2
		case ('dim_mask')
			args = args//', dim = '//convert(a2, elem_type(node%args(2)%val), &
				i32_type)//' + 1, mask = '//emit_expr(em, node%args(3))
		end select
		s = base//'('//args//')'

	case ('norm2')
		s = 'norm2('//a1//')'

	case ('dot')
		s = 'dot_product('//a1//', '//a2//')'

	case ('exp', 'log', 'log10', 'sqrt', 'abs', 'cos', 'sin', 'tan', 'acos', 'asin', &
			'atan', 'cosd', 'sind', 'tand', 'acosd', 'asind', 'atand')
		s = base//'('//a1//')'

	case ('log2')
		! The interpreter's log2 is log(x) / log(2), in the argument's own
		! precision
		t = elem_type(node%args(1)%val)
		kind_str = type_kind_suffix(t)
		s = '(log('//a1//') / log(2.0_'//kind_str//'))'

	case ('min', 'max')
		s = base//'('
		do i = 1, n
			if (i > 1) s = s//', '
			if (i == 1) then
				s = s//a1
			else if (i == 2) then
				s = s//a2
			else
				s = s//emit_expr(em, node%args(i))
			end if
		end do
		s = s//')'

	case ('i32', 'i64')
		kind_str = merge('int32', 'int64', base == 'i32')
		if (elem_type(node%args(1)%val) == str_type) then
			! The code of a single character
			s = 'int(iachar('//a1//'), '//kind_str//')'
		else
			s = 'int('//a1//', '//kind_str//')'
		end if

	case default
		call em_unsupported(em, 'the intrinsic fn `'// &
			node%identifier%text//'`', node%identifier%pos)
		s = '0'

	end select

end function emit_intr_call

!===============================================================================

recursive module function emit_expr(em, node) result(s)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	select case (node%kind)

	case (literal_expr)
		s = emit_literal(em, node)

	case (name_expr)
		s = emit_name_ref(em, node)

	case (array_expr)
		if (node%is_enum_name .or. allocated(node%val%enum_name)) then
			call em_unsupported(em, 'an enum')
			s = '0'
		else
			s = emit_array_expr(em, node)
		end if

	case (binary_expr)
		s = emit_binary(em, node)

	case (unary_expr)
		s = emit_unary(em, node)

	case (fn_call_expr)
		if (allocated(node%lsubscripts)) then
			call em_unsupported(em, 'a subscripted fn call')
			s = '0'
		else
			s = emit_user_call(em, node)
		end if

	case (fn_call_intr_expr)
		s = emit_intr_call(em, node)

	case (assignment_expr)
		s = emit_assign_expr(em, node)

	case default
		call em_unsupported(em, 'a `'//kind_name(node%kind)//'`')
		s = '0'

	end select

end function emit_expr

!===============================================================================

recursive function emit_assign_expr(em, node) result(s)

	! An assignment which is used as a value, like `a = b = 1` or `f(x += 1)`.
	! Fortran has no such thing, so the assignment is hoisted ahead of the
	! statement that it's in, which then reads the target back.  That's the same
	! thing unless another part of the statement depends on the order of
	! evaluation, which is also fragile in syntran

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable :: s

	integer :: i

	s = '0'

	if (em%in_cond) then
		call em_unsupported(em, 'an assignment in a loop condition or `else if`')
		return
	end if

	if (allocated(node%member)) then
		call em_unsupported(em, 'an assignment to a struct member as a value')
		return
	end if

	! Reading the target back would evaluate its subscripts again
	if (allocated(node%lsubscripts)) then
		do i = 1, size(node%lsubscripts)
			if (node%lsubscripts(i)%sub_kind == scalar_sub) then
				if (is_simple(node%lsubscripts(i))) cycle
			end if
			call em_unsupported(em, 'an assignment to a complex subscript as a value')
			return
		end do
	end if

	call emit_assign(em, node)
	s = emit_name_ref(em, node)

end function emit_assign_expr

!===============================================================================

end submodule syntran__transpile_expr

!===============================================================================

