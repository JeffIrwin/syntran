
!===============================================================================

submodule (syntran__transpile_m) syntran__transpile_stmt

	! Fortran backend: statements, fns, and assembling the whole program.
	!
	! syntran's scopes are flattened.  Every binding has a unique slot (see
	! transpile.f90), so all of a procedure's variables are simply declared at
	! its top, and a `let` becomes a plain assignment where it stood.  Never emit
	! an initializer in a declaration: that would imply `save`, which breaks
	! recursion and re-entering a loop body

	implicit none

	type node_ptr_t
		type(syntax_node_t), pointer :: p => null()
	end type node_ptr_t

	! Longest line to emit before wrapping.  gfortran's default free-form limit
	! is 132
	integer, parameter :: MAX_LINE = 100

!===============================================================================

contains

!===============================================================================

function new_tmp_of(em, val, prefix) result(name)

	! A hidden temporary variable of the type of `val`, which may be an array

	type(emitter_t), intent(inout) :: em
	type(value_t), intent(in) :: val
	character(len = *), intent(in) :: prefix
	character(len = :), allocatable :: name

	logical :: ok

	em%tmp_count = em%tmp_count + 1
	name = prefix//'_t'//str(em%tmp_count)

	call em%decls%push(decl_line(em, val, name, ok))
	if (.not. ok) call em_unsupported(em, 'a temporary of type `'//kind_name(val%type)//'`')

end function new_tmp_of

!===============================================================================

subroutine emit_result_str(em, s)

	! Print the program's result, given a Fortran string expression for it

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: s

	if (.not. em%print_result) return

	if (em%trim_result) then
		call em_line(em, 'call rt_result(trim(adjustl('//s//')))')
	else
		call em_line(em, 'call rt_result('//s//')')
	end if

end subroutine emit_result_str

!===============================================================================

subroutine emit_result_invalid(em)

	! What the interpreter prints for a program whose last statement doesn't
	! have a value, like a loop

	type(emitter_t), intent(inout) :: em

	call emit_result_str(em, "'Error: <invalid_value>'")

end subroutine emit_result_invalid

!===============================================================================

recursive subroutine emit_print(em, node)

	! println(a, b, ...) writes each argument, without a separator, and then
	! ends the line

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	integer :: i

	if (allocated(node%args)) then
		do i = 1, size(node%args)
			call em_line(em, 'call rt_print('// &
				str_of(em, node%args(i)%val, emit_expr(em, node%args(i)))//')')
		end do
	end if

	call em_line(em, 'call rt_endl()')

end subroutine emit_print

!===============================================================================

recursive subroutine emit_writeln(em, node)

	! writeln(f, a, b, ...) writes each argument as println() does, without a
	! separator, to the file, and then ends the line

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: f, tmp

	integer :: i

	f = emit_expr(em, node%args(1))
	if (node%args(1)%kind /= name_expr .or. allocated(node%args(1)%lsubscripts)) then
		! Used for each of the values
		tmp = new_tmp(em, 'type(rt_file_t)', 'fh')
		call em_line(em, tmp//' = '//f)
		f = tmp
	end if

	do i = 2, size(node%args)
		call em_line(em, 'call rt_write('//f//', '// &
			str_of(em, node%args(i)%val, emit_expr(em, node%args(i)))//')')
	end do

	call em_line(em, 'call rt_write_end('//f//')')

end subroutine emit_writeln

!===============================================================================

recursive subroutine emit_call_stmt(em, node)

	! A call whose value, if any, is discarded

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: tmp, f

	if (node%kind == fn_call_intr_expr) then
		select case (node%identifier%text)
		case ('println')
			call emit_print(em, node)
			return

		case ('exit')
			call em_line(em, 'call rt_exit('// &
				convert(emit_expr(em, node%args(1)), elem_type(node%args(1)%val), &
				i32_type)//')')
			return

		case ('writeln')
			call emit_writeln(em, node)
			return

		case ('close')
			f = emit_expr(em, node%args(1))
			if (.not. is_file_var(node%args(1))) then
				! Not a variable to update, so there is nothing for the closed handle
				! to be seen by.  It is still closed
				tmp = new_tmp(em, 'type(rt_file_t)', 'fh')
				call em_line(em, tmp//' = '//f)
				f = tmp
			end if
			call em_line(em, 'call rt_close('//f//')')
			return

		end select
	end if

	if (node%val%type == void_type) then
		if (node%kind == fn_call_expr .or. node%kind == method_call_expr .or. &
				node%kind == fn_call_ptr_expr) then
			call em_line(em, 'call '//emit_expr(em, node))
		else
			call em_unsupported(em, 'the intrinsic fn `'// &
				node%identifier%text//'`', node%identifier%pos)
		end if

	else
		! Evaluate for its side effects
		tmp = new_tmp_of(em, node%val, 'discard')
		call em_line(em, tmp//' = '//emit_expr(em, node))

	end if

end subroutine emit_call_stmt

!===============================================================================

recursive subroutine emit_let(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: rhs

	rhs = emit_expr(em, node%right)
	call declare_var(em, node, node%val)
	call em_line(em, var_name(em, node)//' = '// &
		unparen(convert(rhs, elem_type(node%right%val), elem_type(node%val))))

end subroutine emit_let

!===============================================================================

recursive module subroutine emit_assign(em, node)

	! Plain and compound assignment, to a variable, an element or a slice of
	! one, or a character of a string

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: lhs, rhs, op_str, fn_str, tmp

	integer :: ct, lt, rt

	logical :: compound, lhs_arr, rhs_arr, done

	lt = elem_type(node%val)
	rt = elem_type(node%right%val)
	lhs_arr = is_arr(node%val)
	rhs_arr = is_arr(node%right%val)
	compound = node%op%kind /= equals_token

	! A character of each string of a slice of strings
	if (lt == str_type .and. .not. compound .and. .not. rhs_arr) then
		call emit_str_slice_assign(em, node, done)
		if (done) return
	end if

	! A compound assignment names its target twice, so anything with side
	! effects in a subscript is only evaluated once, up front
	if (allocated(em%str_step)) deallocate(em%str_step)
	lhs = emit_name_ref(em, node, hoist = compound, target = .true.)
	rhs = emit_expr(em, node%right)

	if (allocated(em%str_step)) then
		! A stepped slice of a string, which can't be a Fortran substring.  The
		! rhs is stored first, so that a call isn't aliased by an rhs which names
		! the target, like `s[:-1:] = s`
		tmp = new_tmp(em, 'character(len = :), allocatable', 'sr')
		call em_line(em, tmp//' = '//rhs)
		call em_line(em, 'call rt_str_step_set('//lhs//', '//tmp//em%str_step//')')
		deallocate(em%str_step)
		return
	end if

	if (lt == str_type) then
		! An array of strings is an array of wrappers, so a lone string needs
		! wrapping to be assigned to (a slice of) one
		if (lhs_arr .and. .not. rhs_arr) rhs = 'rt_str_t_of('//rhs//')'

		if (.not. compound) then
			call em_line(em, lhs//' = '//rhs)
		else if (node%op%kind == plus_equals_token) then
			if (lhs_arr) then
				call em_line(em, lhs//' = rt_cat('//lhs//', '//rhs//')')
			else
				call em_line(em, lhs//' = '//lhs//' // '//rhs)
			end if
		else
			call em_unsupported(em, 'the `'//node%op%text//'` operator')
		end if
		return
	end if

	if (.not. compound) then
		call em_line(em, lhs//' = '//unparen(convert(rhs, rt, lt)))
		return
	end if

	! Compound assignment.  Do the operation in the wider of the two types, like
	! the equivalent binary operation does, then store in the variable's type
	ct = lt
	if (is_numeric_type(lt) .and. is_numeric_type(rt)) ct = wider_type(lt, rt)

	op_str = ''
	fn_str = ''

	select case (node%op%kind)
	case (plus_equals_token)
		op_str = ' + '
	case (minus_equals_token)
		op_str = ' - '
	case (star_equals_token)
		op_str = ' * '
	case (slash_equals_token)
		op_str = ' / '
	case (sstar_equals_token)
		op_str = ' ** '
	case (percent_equals_token)
		fn_str = 'mod'
	case (amp_equals_token)
		fn_str = 'iand'
	case (pipe_equals_token)
		fn_str = 'ior'
	case (caret_equals_token)
		fn_str = 'ieor'
	case (lless_equals_token)
		fn_str = 'shiftl'
	case (ggreater_equals_token)
		fn_str = 'shiftr'
	case default
		call em_unsupported(em, 'the `'//node%op%text//'` operator')
		return
	end select

	if (node%op%kind == lless_equals_token .or. node%op%kind == ggreater_equals_token) then
		! Shifted in the width of the target.  The count can have any integer
		! type
		call em_line(em, lhs//' = '//fn_str//'('//lhs//', '//rhs//')')

	else if (len(fn_str) > 0) then
		call em_line(em, lhs//' = '//convert(fn_str//'('//convert(lhs, lt, ct)// &
			', '//convert(rhs, rt, ct)//')', ct, lt))

	else
		call em_line(em, lhs//' = '//unparen(convert('('//convert(lhs, lt, ct)// &
			op_str//convert(rhs, rt, ct)//')', ct, lt)))
	end if

end subroutine emit_assign

!===============================================================================

recursive function has_loopless_break(node) result(found)

	! Does this statement have a `break` that isn't inside of a loop?  Such a
	! break leaves the outermost block.  Loops are not searched, because their
	! breaks leave the loop instead

	type(syntax_node_t), intent(in) :: node
	logical :: found

	integer :: i

	found = .false.

	select case (node%kind)
	case (break_statement)
		found = .true.

	case (block_statement)
		if (allocated(node%members)) then
			do i = 1, size(node%members)
				if (has_loopless_break(node%members(i))) then
					found = .true.
					return
				end if
			end do
		end if

	case (if_statement)
		if (allocated(node%if_clause)) found = has_loopless_break(node%if_clause)
		if (found) return
		if (allocated(node%else_clause)) found = has_loopless_break(node%else_clause)

	case (switch_statement)
		! The arms of a switch behave like the clauses of an `if`
		if (allocated(node%members)) then
			do i = 1, size(node%members)
				if (.not. allocated(node%members(i)%body)) cycle
				if (has_loopless_break(node%members(i)%body)) then
					found = .true.
					return
				end if
			end do
		end if
		if (allocated(node%else_clause)) found = has_loopless_break(node%else_clause)

	end select

end function has_loopless_break

!===============================================================================

recursive subroutine emit_block(em, node, is_result)

	! The statements of a block.  If is_result, the last one's value is the
	! program's result.
	!
	! A `break` outside of a loop jumps to the end of the outermost block.  That
	! one is wrapped in a named Fortran `block` construct to `exit` from

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	character(len = :), allocatable :: label, saved_label

	integer :: i, n

	logical :: wrap

	wrap = .false.
	if (em%loop_depth == 0) then
		wrap = .true.
		if (allocated(em%break_label)) wrap = len(em%break_label) == 0
		if (wrap) wrap = has_loopless_break(node)
	end if

	if (wrap) then
		em%tmp_count = em%tmp_count + 1
		label = 'brk_t'//str(em%tmp_count)

		if (allocated(em%break_label)) then
			saved_label = em%break_label
		else
			saved_label = ''
		end if
		em%break_label = label

		call em_line(em, label//': block')
		em%indent = em%indent + 1
	end if

	n = 0
	if (allocated(node%members)) n = size(node%members)

	if (is_result .and. n == 0) then
		call emit_result_invalid(em)
	else
		do i = 1, n
			if (is_result .and. i == n) then
				call emit_result_stmt(em, node%members(i))
			else
				call emit_stmt(em, node%members(i))
			end if
		end do
	end if

	if (wrap) then
		em%indent = em%indent - 1
		call em_line(em, 'end block '//label)
		em%break_label = saved_label
	end if

end subroutine emit_block

!===============================================================================

recursive subroutine emit_if(em, node, is_result)

	! if / else if / else chain.  If is_result, the value of the taken branch's
	! last statement is the program's result

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	call em_line(em, 'if ('//unparen(emit_expr(em, node%condition))//') then')

	call emit_branch(node%if_clause)

	! Flatten `else if` chains instead of nesting each one
	if (allocated(node%else_clause)) then
		call emit_else(node%else_clause)
	else if (is_result) then
		call em_line(em, 'else')
		em%indent = em%indent + 1
		call emit_result_invalid(em)
		em%indent = em%indent - 1
	end if

	call em_line(em, 'end if')

contains

	recursive subroutine emit_branch(clause)
		type(syntax_node_t), intent(in) :: clause
		em%indent = em%indent + 1
		if (is_result) then
			call emit_result_stmt(em, clause)
		else
			call emit_stmt(em, clause)
		end if
		em%indent = em%indent - 1
	end subroutine emit_branch

	recursive subroutine emit_else(clause)
		type(syntax_node_t), intent(in) :: clause
		character(len = :), allocatable :: cond
		type(string_vector_t) :: hoisted
		if (clause%kind == if_statement) then
			! This condition isn't reached on every pass through the statement, so
			! nothing can be hoisted ahead of the whole `if`.  If it needs
			! statements first, it's a nested `if` in the `else` instead
			call emit_cond(em, clause%condition, cond, hoisted)

			if (hoisted%len_ == 0) then
				call em_line(em, 'else if ('//unparen(cond)//') then')
			else
				call em_line(em, 'else')
				em%indent = em%indent + 1
				call em%body%push_all(hoisted)
				call em_line(em, 'if ('//unparen(cond)//') then')
			end if

			call emit_branch(clause%if_clause)
			if (allocated(clause%else_clause)) then
				call emit_else(clause%else_clause)
			else if (is_result) then
				call em_line(em, 'else')
				em%indent = em%indent + 1
				call emit_result_invalid(em)
				em%indent = em%indent - 1
			end if

			if (hoisted%len_ > 0) then
				call em_line(em, 'end if')
				em%indent = em%indent - 1
			end if
		else
			call em_line(em, 'else')
			call emit_branch(clause)
		end if
	end subroutine emit_else

end subroutine emit_if

!===============================================================================

recursive function is_pure_case(node) result(pure_)

	! Can the match value (or range) of a `case` be evaluated whether or not an
	! earlier value of its arm matched, because it has no side effects and nothing
	! has to be hoisted out of it?  Otherwise the values have to be tested in
	! order, stopping at the first match, like the interpreter does

	type(syntax_node_t), intent(in) :: node
	logical :: pure_

	! An array has to be stored in a hidden variable first
	if (node%kind /= case_range) then
		if (is_arr(node%val)) then
			pure_ = .false.
			return
		end if
	end if

	select case (node%kind)
	case (literal_expr)
		pure_ = .true.
	case (name_expr)
		pure_ = .not. allocated(node%lsubscripts)
	case (unary_expr)
		pure_ = .false.
		if (allocated(node%right)) pure_ = is_pure_case(node%right)
	case (case_range)
		pure_ = is_pure_case(node%lbound_) .and. is_pure_case(node%ubound_)
	case default
		pure_ = .false.
	end select

end function is_pure_case

!===============================================================================

recursive function case_cmp(em, subj, sval, v, op) result(s)

	! Fortran expression for `<subject> == v` if `op` is eequals_token, or for
	! `<subject> < v` if it is less_token.  `subj` is the hidden variable that the
	! subject was stored in and `sval` is its value, for the type

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: subj
	type(value_t), intent(in) :: sval
	type(syntax_node_t), intent(in) :: v
	integer, intent(in) :: op
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: r, tmp, a, b

	integer :: st, vt, ct, k

	r  = emit_expr(em, v)
	st = elem_type(sval)
	vt = elem_type(v%val)

	if (is_arr(sval)) then
		! Whole-array equality.  Ranges of arrays aren't allowed by the parser.  The
		! value is stored in a hidden variable first, so that its shape and its
		! elements can both be read from it
		tmp = new_tmp_of(em, v%val, 'cv')
		call em_line(em, tmp//' = '//unparen(r))

		ct = st
		if (is_numeric_type(st) .and. is_numeric_type(vt)) ct = wider_type(st, vt)

		a = 'reshape('//subj//', [size('//subj//')])'
		b = 'reshape('//tmp//', [size('//tmp//')])'
		if (st == enum_type) then
			! Variants are equal if their backing values are
			k = enum_slot_of(em, sval)
			if (k > 0) then
				a = enum_fn(em, k, 'val')//'('//a//')'
				b = enum_fn(em, k, 'val')//'('//b//')'
			end if
		end if

		s = 'rt_arr_eq(shape('//subj//'), '//convert(a, st, ct)//', '// &
			'shape('//tmp//'), '//convert(b, vt, ct)//')'

	else if (st == str_type) then
		if (op == less_token) then
			s = 'rt_str_lt('//subj//', '//r//')'
		else
			s = 'rt_str_eq('//subj//', '//r//')'
		end if

	else if (st == bool_type) then
		s = '('//subj//' .eqv. '//r//')'

	else if (st == enum_type) then
		k = enum_slot_of(em, sval)
		if (k == 0) then
			call em_unsupported(em, 'an enum of unknown type')
			s = '.false.'
		else
			s = enum_cmp(em, k, op, subj, r)
		end if

	else if (is_numeric_type(st) .and. is_numeric_type(vt)) then
		ct = wider_type(st, vt)
		if (op == less_token) then
			s = '('//convert(subj, st, ct)//' < '//convert(r, vt, ct)//')'
		else
			s = '('//convert(subj, st, ct)//' == '//convert(r, vt, ct)//')'
		end if

	else
		call em_unsupported(em, 'a `switch` on type `'//kind_name(st)//'`')
		s = '.false.'

	end if

end function case_cmp

!===============================================================================

recursive function case_test(em, subj, sval, v) result(s)

	! Fortran condition for whether a case value or range matches the subject.
	! A range is half-open like the interpreter's: `lo <= subj < hi`, tested as
	! "not (subj < lo) and subj < hi"

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: subj
	type(value_t), intent(in) :: sval
	type(syntax_node_t), intent(in) :: v
	character(len = :), allocatable :: s

	if (v%kind == case_range) then
		s = '(.not. '//case_cmp(em, subj, sval, v%lbound_, less_token)//' .and. '// &
			case_cmp(em, subj, sval, v%ubound_, less_token)//')'
	else
		s = case_cmp(em, subj, sval, v, eequals_token)
	end if

end function case_test

!===============================================================================

recursive subroutine emit_switch(em, node, is_result)

	! A switch statement is a chain of arms inside a named Fortran `block`.  The
	! subject is stored once in a hidden variable.  An arm whose value (or one of
	! its values) matches and whose guard holds runs its body and then leaves
	! the block, so that a guard which is false falls through to the next arm, like
	! the interpreter's.  The `default` arm is last, and its position in the
	! source doesn't matter
	!
	!   sw_t3 = <subject>
	!   sw_t3_blk: block
	!       if (<match 1>) then
	!           if (<guard>) then
	!               <body>
	!               exit sw_t3_blk
	!           end if
	!       end if
	!       ...
	!       <default body>
	!   end block sw_t3_blk
	!
	! The values of an arm are tested in order, and only up to the first that
	! matches, because a value can be a call with side effects.  An arm whose
	! values can't have any is tested with a single condition
	!
	! If is_result, the value of the body that ran is the program's result, and a
	! switch that matched nothing has none

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	character(len = :), allocatable :: label, subj, matched, test

	integer :: i, j, nelems, narms, depth

	logical :: all_pure, has_guard

	em%tmp_count = em%tmp_count + 1
	label = 'sw_blk'//str(em%tmp_count)

	test = emit_expr(em, node%condition)
	subj = new_tmp_of(em, node%condition%val, 'sw')
	call em_line(em, subj//' = '//unparen(test))

	call em_line(em, label//': block')
	em%indent = em%indent + 1

	narms = 0
	if (allocated(node%members)) narms = size(node%members)

	do i = 1, narms

		depth = 0

		nelems = 0
		if (allocated(node%members(i)%elems)) nelems = size(node%members(i)%elems)

		if (nelems > 0) then

			all_pure = .true.
			do j = 1, nelems
				if (.not. is_pure_case(node%members(i)%elems(j))) all_pure = .false.
			end do

			if (all_pure) then
				test = ''
				do j = 1, nelems
					if (j > 1) test = test//' .or. '
					test = test//case_test(em, subj, node%condition%val, &
						node%members(i)%elems(j))
				end do
				call em_line(em, 'if ('//unparen(test)//') then')
				em%indent = em%indent + 1
				depth = depth + 1

			else
				! Stop at the first value that matches
				matched = new_tmp(em, 'logical', 'm')
				call em_line(em, matched//' = .false.')

				do j = 1, nelems
					call em_line(em, 'if (.not. '//matched//') then')
					em%indent = em%indent + 1

					associate (v => node%members(i)%elems(j))
						if (v%kind == case_range) then
							! The upper bound isn't evaluated for a subject below the
							! lower bound
							test = case_cmp(em, subj, node%condition%val, v%lbound_, &
								less_token)
							call em_line(em, 'if (.not. '//test//') then')
							em%indent = em%indent + 1
							test = case_cmp(em, subj, node%condition%val, v%ubound_, &
								less_token)
							call em_line(em, matched//' = '//unparen(test))
							em%indent = em%indent - 1
							call em_line(em, 'end if')
						else
							test = case_cmp(em, subj, node%condition%val, v, eequals_token)
							call em_line(em, matched//' = '//unparen(test))
						end if
					end associate

					em%indent = em%indent - 1
					call em_line(em, 'end if')
				end do

				call em_line(em, 'if ('//matched//') then')
				em%indent = em%indent + 1
				depth = depth + 1

			end if
		end if

		! The guard is only evaluated once a value has matched, or at once for an
		! arm that is nothing but a guard
		has_guard = allocated(node%members(i)%condition)
		if (has_guard) then
			test = emit_expr(em, node%members(i)%condition)
			call em_line(em, 'if ('//unparen(test)//') then')
			em%indent = em%indent + 1
			depth = depth + 1
		end if

		call emit_arm_body(node%members(i)%body)
		call em_line(em, 'exit '//label)

		do j = 1, depth
			em%indent = em%indent - 1
			call em_line(em, 'end if')
		end do

	end do

	if (allocated(node%else_clause)) then
		call emit_arm_body(node%else_clause)
	else if (is_result) then
		call emit_result_invalid(em)
	end if

	em%indent = em%indent - 1
	call em_line(em, 'end block '//label)

contains

	recursive subroutine emit_arm_body(body)
		type(syntax_node_t), intent(in) :: body
		if (is_result) then
			call emit_result_stmt(em, body)
		else
			call emit_stmt(em, body)
		end if
	end subroutine emit_arm_body

end subroutine emit_switch

!===============================================================================

recursive subroutine emit_cond(em, node, cond, hoisted)

	! The Fortran condition for the expression `node`, and the statements which
	! have to run before it, which the expression hoisted.  They are at one
	! level deeper than the current indentation, which is where they go

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	character(len = :), allocatable, intent(out) :: cond
	type(string_vector_t), intent(out) :: hoisted

	type(string_vector_t) :: saved

	saved = em%body
	em%body = new_string_vector()
	em%indent = em%indent + 1

	cond = emit_expr(em, node)

	hoisted = em%body
	em%body = saved
	em%indent = em%indent - 1

end subroutine emit_cond

!===============================================================================

recursive subroutine emit_while(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: cond

	type(string_vector_t) :: hoisted

	! The condition is evaluated on each pass, so what it needs done first, like
	! an assignment that is used as a value, goes in the loop
	call emit_cond(em, node%condition, cond, hoisted)

	if (hoisted%len_ == 0) then
		call em_line(em, 'do while ('//unparen(cond)//')')
		em%indent = em%indent + 1
	else
		call em_line(em, 'do')
		em%indent = em%indent + 1
		call em%body%push_all(hoisted)
		call em_line(em, 'if (.not. ('//unparen(cond)//')) exit')
	end if

	em%loop_depth = em%loop_depth + 1
	call emit_stmt(em, node%body)
	em%loop_depth = em%loop_depth - 1
	em%indent = em%indent - 1
	call em_line(em, 'end do')

end subroutine emit_while

!===============================================================================

recursive subroutine emit_for(em, node)

	! syntran evaluates the iterable once on entering the loop, and the body may
	! assign to the loop variable without affecting the iteration.  So the loop
	! is driven by a hidden counter, and the variable is assigned from it
	!
	! An integer range is a Fortran do loop of its own.  Anything else, i.e. an
	! array or a string, is first stored in a temporary which is then iterated

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: it, tmp, len_str, elem_str

	type(value_t) :: var_val

	if (node%array%kind == array_expr) then
		if (node%array%val%array%kind == bound_array .or. &
				node%array%val%array%kind == step_array) then
			if (any(node%array%val%array%type == [i32_type, i64_type])) then
				call emit_for_range(em, node)
				return
			end if
		end if
	end if

	!********
	! A string or array

	if (node%array%val%type == str_type) then

		tmp = new_tmp(em, 'character(len = :), allocatable', 'str')
		call em_line(em, tmp//' = '//emit_expr(em, node%array))
		var_val%type = str_type
		len_str = 'len('//tmp//')'
		elem_str = tmp//'(KK:KK)'

	else if (node%array%val%type == array_type) then

		if (node%array%val%array%rank /= 1) then
			call em_unsupported(em, 'a `for` loop over an array that is not rank 1')
			return
		end if

		tmp = new_tmp_of(em, node%array%val, 'arr')
		call em_line(em, tmp//' = '//emit_expr(em, node%array))

		var_val = node%array%val
		var_val%type = node%array%val%array%type
		if (allocated(var_val%array)) deallocate(var_val%array)
		len_str = 'size('//tmp//')'
		if (var_val%type == str_type) then
			elem_str = tmp//'(KK)%s'
		else
			elem_str = tmp//'(KK)'
		end if

	else
		call em_unsupported(em, 'a `for` loop over a value of type `'// &
			kind_name(node%array%val%type)//'`')
		return
	end if

	it = new_tmp(em, type_spec(i32_type), 'it')

	call declare_var(em, node, var_val)

	call em_line(em, 'do '//it//' = 1, '//len_str)

	em%indent = em%indent + 1
	em%loop_depth = em%loop_depth + 1

	! Substitute the counter into the element designator
	call em_line(em, var_name(em, node)//' = '//replace_kk(elem_str, it))
	call emit_stmt(em, node%body)

	em%loop_depth = em%loop_depth - 1
	em%indent = em%indent - 1

	call em_line(em, 'end do')

end subroutine emit_for

!===============================================================================

function replace_kk(s, it) result(r)

	! Replace each `KK` placeholder in s by the loop counter's name

	character(len = *), intent(in) :: s, it
	character(len = :), allocatable :: r

	integer :: i

	r = ''
	i = 1
	do while (i <= len(s))
		if (i < len(s)) then
			if (s(i:i+1) == 'KK') then
				r = r//it
				i = i + 2
				cycle
			end if
		end if
		r = r//s(i:i)
		i = i + 1
	end do

end function replace_kk

!===============================================================================

recursive subroutine emit_for_range(em, node)

	! A for loop over an integer range `[a: b]` or `[a: step: b]` is a Fortran
	! do loop.  Its bounds are evaluated once on entry, just like syntran's

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: it, lb, ub, step, tmp
	character(len = 2) :: kindstr

	integer :: elem

	type(value_t) :: var_val

	elem = node%array%val%array%type
	kindstr = merge('32', '64', elem == i32_type)

	associate (arr => node%array)

		lb = convert(emit_expr(em, arr%lbound_), elem_type(arr%lbound_%val), elem)
		ub = convert(emit_expr(em, arr%ubound_), elem_type(arr%ubound_%val), elem)

		it = new_tmp(em, type_spec(elem), 'it')

		if (arr%val%array%kind == step_array) then
			step = convert(emit_expr(em, arr%step), elem_type(arr%step%val), elem)
			if (.not. is_simple(arr%step)) then
				! Used twice below
				tmp = new_tmp(em, type_spec(elem), 'step')
				call em_line(em, tmp//' = '//step)
				step = tmp
			end if
			! The upper bound is exclusive, in either direction
			call em_line(em, 'do '//it//' = '//lb//', '//ub//' - sign(1_int'// &
				kindstr//', '//step//'), '//step)
		else
			call em_line(em, 'do '//it//' = '//lb//', '//ub//' - 1')
		end if

	end associate

	var_val%type = elem
	call declare_var(em, node, var_val)

	em%indent = em%indent + 1
	em%loop_depth = em%loop_depth + 1
	call em_line(em, var_name(em, node)//' = '//it)
	call emit_stmt(em, node%body)
	em%loop_depth = em%loop_depth - 1
	em%indent = em%indent - 1

	call em_line(em, 'end do')

end subroutine emit_for_range

!===============================================================================

recursive subroutine emit_return(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	logical :: is_void

	is_void = node%right%val%type == void_type

	if (em%top_level) then
		! Returning from the program prints the returned value, which is what the
		! interpreter does with it
		if (is_void) then
			call emit_result_invalid(em)
		else if (em%print_result) then
			call emit_result_str(em, str_of(em, node%right%val, emit_expr(em, node%right)))
		else
			call emit_discard(em, node%right)
		end if
		call em_line(em, 'return')

	else
		if (.not. is_void) then
			call em_line(em, 'res_ = '//unparen(emit_expr(em, node%right)))
		end if
		call em_line(em, 'return')

	end if

end subroutine emit_return

!===============================================================================

recursive subroutine emit_discard(em, node)

	! Evaluate an expression only for its side effects

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	select case (node%kind)
	case (fn_call_expr, fn_call_intr_expr, method_call_expr, fn_call_ptr_expr)
		call emit_call_stmt(em, node)
	case default
		! A pure expression has no side effects
	end select

end subroutine emit_discard

!===============================================================================

recursive subroutine emit_result_stmt(em, node)

	! Emit a statement such that the program's result is the value of the last
	! statement that gets executed.  This is what the interpreter does.  It
	! recurses into the branch of an `if` and into the end of a block.  A loop, a
	! bare `return`, an `if` that doesn't run, or a void call has no value

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
	end if

	select case (node%kind)

	case (block_statement)
		call emit_block(em, node, .true.)

	case (if_statement)
		call emit_if(em, node, .true.)

	case (while_statement, for_statement, break_statement, continue_statement)
		call emit_stmt(em, node)
		call emit_result_invalid(em)

	case (return_statement)
		call emit_stmt(em, node)

	case (switch_statement)
		call emit_switch(em, node, .true.)

	case (struct_declaration, enum_declaration)
		call emit_stmt(em, node)

	case (use_statement)
		! Its value is nothing
		call emit_stmt(em, node)
		call emit_result_invalid(em)

	case (let_expr)
		call emit_stmt(em, node)
		call emit_result_str(em, str_of(em, node%val, var_name(em, node)))

	case (assignment_expr)
		call emit_stmt(em, node)
		! The target, e.g. a whole array or just the element that was assigned
		call emit_result_str(em, str_of(em, node%val, emit_name_ref(em, node)))

	case (fn_call_expr, fn_call_intr_expr, method_call_expr, fn_call_ptr_expr)
		if (node%val%type == void_type) then
			call emit_call_stmt(em, node)
			! println() has an empty result while other void fns have none.  exit()
			! never gets to have one
			if (node%kind == fn_call_intr_expr) then
				if (node%identifier%text /= 'exit') call emit_result_str(em, "''")
			else
				call emit_result_invalid(em)
			end if
		else
			call emit_result_expr(em, node)
		end if

	case default
		call emit_result_expr(em, node)

	end select

end subroutine emit_result_stmt

!===============================================================================

recursive subroutine emit_result_expr(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	if (node%val%type == unknown_type .or. node%val%type == void_type) then
		call emit_result_invalid(em)
	else if (em%print_result) then
		call emit_result_str(em, str_of(em, node%val, emit_expr(em, node)))
	else
		call emit_discard(em, node)
	end if

end subroutine emit_result_expr

!===============================================================================

recursive subroutine emit_use(em, node)

	! A module's own top-level statements, like `let count = 0;`, run where it is
	! imported.  Its fns are emitted with the rest of them (c.f. emit_module_fns()).
	! The parser numbered the module's variables and fns together with the
	! importer's, so they are named like any other global and need no mapping

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	integer :: i

	if (.not. allocated(node%member)) return
	if (.not. allocated(node%member%members)) return

	do i = 1, size(node%member%members)
		if (node%member%members(i)%kind == fn_declaration) cycle
		call emit_stmt(em, node%member%members(i))
	end do

end subroutine emit_use

!===============================================================================

recursive subroutine emit_module_fns(em, unit, done)

	! Emit the fns of an imported module, and of the modules that it imports.
	! `done` has the ids of the fns that are already emitted, which a module that
	! is imported from two places isn't to do again

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: unit
	type(integer_vector_t), intent(inout) :: done

	integer :: i, j
	logical :: found

	if (.not. allocated(unit%members)) return

	do i = 1, size(unit%members)
		if (unit%members(i)%kind /= use_statement) cycle
		if (.not. allocated(unit%members(i)%member)) cycle
		call emit_module_fns(em, unit%members(i)%member, done)
	end do

	do i = 1, size(unit%members)
		if (unit%members(i)%kind == struct_declaration) then
			call emit_struct_methods(em, unit%members(i), done)
			cycle
		end if
		if (unit%members(i)%kind /= fn_declaration) cycle

		found = .false.
		do j = 1, done%len_
			if (done%v(j) == unit%members(i)%id_index) found = .true.
		end do
		if (found) cycle

		call done%push(unit%members(i)%id_index)
		call emit_fn(em, unit%members(i))
	end do

end subroutine emit_module_fns

!===============================================================================

recursive module subroutine emit_stmt(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
	end if

	if (node%is_empty) return

	select case (node%kind)

	case (block_statement)
		call emit_block(em, node, .false.)

	case (let_expr)
		call emit_let(em, node)

	case (assignment_expr)
		call emit_assign(em, node)

	case (if_statement)
		call emit_if(em, node, .false.)

	case (while_statement)
		call emit_while(em, node)

	case (for_statement)
		call emit_for(em, node)

	case (break_statement)
		if (em%loop_depth > 0) then
			call em_line(em, 'exit')
		else if (allocated(em%break_label)) then
			if (len(em%break_label) > 0) call em_line(em, 'exit '//em%break_label)
		end if
		! Otherwise there is nothing to break out of, which is a no-op

	case (continue_statement)
		if (em%loop_depth > 0) call em_line(em, 'cycle')

	case (return_statement)
		call emit_return(em, node)

	case (fn_call_expr, fn_call_intr_expr, method_call_expr, fn_call_ptr_expr)
		call emit_call_stmt(em, node)

	case (fn_declaration)
		! Emitted separately, since a fn is its own Fortran procedure

	case (struct_declaration)
		! The derived type is emitted with the program (c.f. emit_struct_types()).
		! A member that has no Fortran type yet is reported here
		call check_struct_members(em, node)

	case (enum_declaration)
		! Nothing runs.  The enum's helper fns are emitted with the program (c.f.
		! emit_enum_procs())

	case (switch_statement)
		call emit_switch(em, node, .false.)

	case (use_statement)
		call emit_use(em, node)

	case default
		! A bare expression statement, which at most has side effects from calls
		call emit_discard(em, node)

	end select

end subroutine emit_stmt

!===============================================================================

subroutine push_wrapped(v, line)

	! Push a source line onto `v`, wrapped with Fortran continuation markers if
	! it is too long.  A line is only broken at a blank outside of a string
	! literal, which is only ever next to an operator or a comma in generated
	! code, so a break there can't change the meaning

	type(string_vector_t), intent(inout) :: v
	character(len = *), intent(in) :: line

	!********

	character(len = :), allocatable :: rest, pad

	integer :: i, k, lead, rest_lead
	logical :: in_quote

	lead = verify(line, ' ') - 1
	if (lead < 0) lead = 0

	rest = line
	pad  = repeat(' ', lead + 8)

	do while (len(rest) > MAX_LINE)

		! Where the text starts, after the indentation of the first line or the
		! padding of a continuation
		rest_lead = verify(rest, ' ') - 1
		if (rest_lead < 0) exit

		! Don't touch comments
		if (rest(rest_lead+1: rest_lead+1) == '!') exit

		k = 0
		in_quote = .false.
		do i = 1, MAX_LINE - 2
			if (rest(i:i) == "'") in_quote = .not. in_quote
			if (i > rest_lead + 1 .and. rest(i:i) == ' ' .and. .not. in_quote) k = i
		end do

		! Nothing to break at within the limit, e.g. a deep nest of parentheses
		! before the first operator.  Take the first blank after the limit
		if (k <= len(pad) + 1) then
			in_quote = .false.
			do i = 1, len(rest)
				if (rest(i:i) == "'") in_quote = .not. in_quote
				if (i > MAX_LINE - 2 .and. rest(i:i) == ' ' .and. .not. in_quote) then
					k = i
					exit
				end if
			end do
		end if

		! Nowhere to break it, so leave it long.  A break has to be past the
		! padding that a continuation gets, otherwise it would never get shorter
		if (k <= len(pad) + 1) exit

		call v%push(rest(1: k-1)//' &')
		rest = pad//rest(k+1:)
	end do

	call v%push(rest)

end subroutine push_wrapped

!===============================================================================

subroutine push_proc(em, header, footer, ret_decls)

	! Move the body and declarations that have been accumulated for one
	! procedure into em%procs, wrapped in its header and footer

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: header, footer
	type(string_vector_t), intent(in) :: ret_decls

	integer :: i

	call push_wrapped(em%procs, '    '//header)
	do i = 1, ret_decls%len_
		call push_wrapped(em%procs, '        '//ret_decls%v(i)%s)
	end do
	do i = 1, em%decls%len_
		call push_wrapped(em%procs, '        '//em%decls%v(i)%s)
	end do
	do i = 1, em%body%len_
		call push_wrapped(em%procs, '    '//em%body%v(i)%s)
	end do
	call em%procs%push('    '//footer)
	call em%procs%push('')

end subroutine push_proc

!===============================================================================

subroutine get_children(node, kids, n)

	! Pointers to every child node of a node, whichever of its many optional
	! components they are in

	type(syntax_node_t), intent(in), target :: node
	type(node_ptr_t), allocatable, intent(out) :: kids(:)
	integer, intent(out) :: n

	integer :: i, cap

	cap = 12
	if (allocated(node%members)) cap = cap + size(node%members)
	if (allocated(node%elems)) cap = cap + size(node%elems)
	if (allocated(node%lsubscripts)) cap = cap + size(node%lsubscripts)
	if (allocated(node%size_)) cap = cap + size(node%size_)
	if (allocated(node%args)) cap = cap + size(node%args)
	if (allocated(node%usubscripts)) cap = cap + size(node%usubscripts)
	if (allocated(node%ssubscripts)) cap = cap + size(node%ssubscripts)
	allocate(kids(cap))
	n = 0

	if (allocated(node%left)) call add(node%left)
	if (allocated(node%right)) call add(node%right)
	if (allocated(node%condition)) call add(node%condition)
	if (allocated(node%if_clause)) call add(node%if_clause)
	if (allocated(node%else_clause)) call add(node%else_clause)
	if (allocated(node%body)) call add(node%body)
	if (allocated(node%array)) call add(node%array)
	if (allocated(node%member)) call add(node%member)
	if (allocated(node%lbound_)) call add(node%lbound_)
	if (allocated(node%step)) call add(node%step)
	if (allocated(node%ubound_)) call add(node%ubound_)
	if (allocated(node%len_)) call add(node%len_)

	if (allocated(node%members)) then
		do i = 1, size(node%members)
			call add(node%members(i))
		end do
	end if
	if (allocated(node%elems)) then
		do i = 1, size(node%elems)
			call add(node%elems(i))
		end do
	end if
	if (allocated(node%lsubscripts)) then
		do i = 1, size(node%lsubscripts)
			call add(node%lsubscripts(i))
		end do
	end if
	if (allocated(node%size_)) then
		do i = 1, size(node%size_)
			call add(node%size_(i))
		end do
	end if
	if (allocated(node%args)) then
		do i = 1, size(node%args)
			call add(node%args(i))
		end do
	end if
	if (allocated(node%usubscripts)) then
		do i = 1, size(node%usubscripts)
			call add(node%usubscripts(i))
		end do
	end if
	if (allocated(node%ssubscripts)) then
		do i = 1, size(node%ssubscripts)
			call add(node%ssubscripts(i))
		end do
	end if

contains

	subroutine add(child)
		type(syntax_node_t), intent(in), target :: child
		n = n + 1
		kids(n)%p => child
	end subroutine add

end subroutine get_children

!===============================================================================

recursive function modifies_slot(node, slot) result(found)

	! Is the local variable in `slot` assigned to anywhere in this tree, or
	! passed by reference to a fn which could assign to it?

	type(syntax_node_t), intent(in), target :: node
	integer, intent(in) :: slot
	logical :: found

	integer :: i, n
	type(node_ptr_t), allocatable :: kids(:)

	found = .false.

	select case (node%kind)

	case (assignment_expr)
		! The target is the node itself.  That includes assignment to an
		! element or slice of the variable
		if (node%is_loc .and. node%id_index == slot) then
			found = .true.
			return
		end if

	case (fn_call_expr, method_call_expr)
		! An argument passed by reference is another way for it to be assigned.
		! A `&const` parameter is only read, but we don't know that here, so
		! assume the worst
		if (allocated(node%is_ref) .and. allocated(node%args)) then
			do i = 1, size(node%args)
				if (.not. node%is_ref(i)) cycle
				if (node%args(i)%is_loc .and. node%args(i)%id_index == slot .and. &
						(node%args(i)%kind == name_expr .or. &
						node%args(i)%kind == dot_expr)) then
					found = .true.
					return
				end if
			end do
		end if

	end select

	call get_children(node, kids, n)
	do i = 1, n
		if (modifies_slot(kids(i)%p, slot)) then
			found = .true.
			return
		end if
	end do

end function modifies_slot

!===============================================================================

recursive function alias_hazard(node, fn_id, ref_slots) result(found)

	! Could something in this tree change a variable that a by-value argument of
	! the fn `fn_id` is aliasing, while it is running?
	!
	! Fortran passes arguments by reference, so a dummy argument which isn't
	! copied is the caller's own variable.  That's only the same as passing by
	! value if nothing else assigns to that variable during the call, which could
	! be through a global, a by-reference parameter, or any fn that does either.
	! So this is conservative: any assignment to a global or by-reference
	! parameter counts, as does calling any user fn other than the fn itself

	type(syntax_node_t), intent(in), target :: node
	integer, intent(in) :: fn_id
	integer, intent(in) :: ref_slots(:)
	logical :: found

	integer :: i, n
	type(node_ptr_t), allocatable :: kids(:)

	found = .false.

	select case (node%kind)

	case (assignment_expr)
		if (.not. node%is_loc) then
			found = .true.
			return
		end if
		if (any(ref_slots == node%id_index)) then
			found = .true.
			return
		end if

	case (fn_call_ptr_expr)
		! Which fn this runs isn't known, and it could assign to anything
		found = .true.
		return

	case (fn_call_expr, method_call_expr)
		if (node%id_index /= fn_id) then
			found = .true.
			return
		end if

		! The fn itself can also pass a global or by-reference parameter as
		! its own by-reference argument
		if (allocated(node%is_ref) .and. allocated(node%args)) then
			do i = 1, size(node%args)
				if (.not. node%is_ref(i)) cycle
				if (node%args(i)%kind /= name_expr .and. &
						node%args(i)%kind /= dot_expr) cycle
				if (.not. node%args(i)%is_loc .or. &
						any(ref_slots == node%args(i)%id_index)) then
					found = .true.
					return
				end if
			end do
		end if

	end select

	call get_children(node, kids, n)
	do i = 1, n
		if (alias_hazard(kids(i)%p, fn_id, ref_slots)) then
			found = .true.
			return
		end if
	end do

end function alias_hazard

!===============================================================================

function dims_of(rank_) result(s)

	! `(:,:)` for rank 2

	integer, intent(in) :: rank_
	character(len = :), allocatable :: s

	integer :: i

	s = '('
	do i = 1, rank_
		if (i > 1) s = s//','
		s = s//':'
	end do
	s = s//')'

end function dims_of

!===============================================================================

function elem_spec(em, val) result(s)

	! Type spec of a scalar's type, or of an array's elements, for a dummy
	! argument

	type(emitter_t), intent(inout) :: em
	type(value_t), intent(in) :: val
	character(len = :), allocatable :: s

	integer :: k, t

	t = elem_type(val)
	if (t == str_type) then
		if (is_arr(val)) then
			s = 'type(rt_str_t)'
		else
			s = 'character(len = *)'
		end if
	else if (t == struct_type) then
		k = struct_slot_of(em, val)
		if (k > 0) then
			s = 'type('//struct_tname(em, k)//')'
		else
			s = 'integer(int32)'
		end if
	else if (t == fn_type) then
		s = 'type(fnptr_t'//str(fptr_slot(em, val))//')'
	else
		s = type_spec(t)
	end if

end function elem_spec

!===============================================================================

subroutine add_cand(kind_, id, base, kinds, ids, names)

	! A variable (kind 0) or fn (kind 1) which might be named as is.  Only once
	! for each, since a module that is imported twice is found twice

	integer, intent(in) :: kind_, id
	character(len = *), intent(in) :: base
	type(integer_vector_t), intent(inout) :: kinds, ids
	type(string_vector_t), intent(inout) :: names

	integer :: j

	do j = 1, ids%len_
		if (kinds%v(j) == kind_ .and. ids%v(j) == id) return
	end do

	call kinds%push(kind_)
	call ids%push(id)
	call names%push(base)

end subroutine add_cand

!===============================================================================

recursive subroutine walk_binders(em, node, want_loc, kinds, ids, names)

	! The names of the variables bound in a tree, either of those which are
	! local to a fn or of those which are global, and of the fns declared in
	! it.  `names` has the starts of their Fortran names

	type(emitter_t), intent(in) :: em
	type(syntax_node_t), intent(in), target :: node
	logical, intent(in) :: want_loc
	type(integer_vector_t), intent(inout) :: kinds, ids
	type(string_vector_t), intent(inout) :: names

	!********

	integer :: i, n
	type(node_ptr_t), allocatable :: kids(:)

	select case (node%kind)

	case (let_expr, for_statement)
		if ((node%is_loc .eqv. want_loc) .and. node%id_index > 0 .and. &
				allocated(node%identifier%text)) then
			call add_cand(0, node%id_index, name_base(node%identifier%text, .false.), &
				kinds, ids, names)
		end if

	case (fn_declaration)
		if (.not. want_loc .and. node%id_index > 0 .and. &
				allocated(node%identifier%text)) then
			call add_cand(1, node%id_index, &
				fn_base(em, node%identifier%text, node%id_index), kinds, ids, names)
		end if

	end select

	call get_children(node, kids, n)
	do i = 1, n
		call walk_binders(em, kids(i)%p, want_loc, kinds, ids, names)
	end do

end subroutine walk_binders

!===============================================================================

function has_name(v, low) result(found)

	type(string_vector_t), intent(in) :: v
	character(len = *), intent(in) :: low
	logical :: found

	integer :: i

	found = .false.
	do i = 1, v%len_
		if (v%v(i)%s == low) then
			found = .true.
			return
		end if
	end do

end function has_name

!===============================================================================

subroutine collect_names(em, tree)

	! Choose the globals and fns that are named as they are in the syntran
	! source, with no suffix.  Fortran treats the case of a name as the same, so
	! `X` and `x` are one name, and a variable can't have the name of a fn,
	! because they're all in the one module.  The first of a name keeps it, with
	! the globals before the fns, and the rest have the suffix with their slot id
	! like they all had before.  So does everything which `bare_ok()` doesn't
	! allow, and anything that isn't found here, which is why a gap in this walk
	! can only make a name uglier and never a clash
	!
	! This also keeps a shadowing variable from clashing with the variable that
	! it shadows

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: tree

	!********

	character(len = :), allocatable :: low

	integer :: j, pass, ng, nf

	type(integer_vector_t) :: kinds, ids
	type(string_vector_t) :: names

	kinds = new_integer_vector()
	ids   = new_integer_vector()
	names = new_string_vector()
	em%bare_names = new_string_vector()

	call walk_binders(em, tree, .false., kinds, ids, names)

	ng = 0
	nf = 0
	do j = 1, ids%len_
		if (kinds%v(j) == 0) ng = max(ng, ids%v(j))
		if (kinds%v(j) == 1) nf = max(nf, ids%v(j))
	end do

	if (allocated(em%bare_global)) deallocate(em%bare_global)
	if (allocated(em%bare_fn)) deallocate(em%bare_fn)
	allocate(em%bare_global(ng), em%bare_fn(nf))
	em%bare_global = .false.
	em%bare_fn = .false.

	do pass = 0, 1
		do j = 1, ids%len_
			if (kinds%v(j) /= pass) cycle
			if (.not. bare_ok(names%v(j)%s, .true.)) cycle

			low = to_lower(names%v(j)%s)
			if (has_name(em%bare_names, low)) cycle
			call em%bare_names%push(low)

			if (pass == 0) then
				em%bare_global(ids%v(j)) = .true.
			else
				em%bare_fn(ids%v(j)) = .true.
			end if
		end do
	end do

end subroutine collect_names

!===============================================================================

subroutine collect_locals(em, decl, is_method)

	! The same as collect_names() for the locals of a fn, which are its
	! parameters and the variables in its body.  A local also can't have the
	! name of a global or a fn that is named as is, because that would hide it
	! for the whole of the fn, though syntran only does that after the `let`

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl
	logical, intent(in) :: is_method

	!********

	character(len = :), allocatable :: low, pname

	integer :: i, j, k, nl

	type(fn_t), pointer :: fn
	type(integer_vector_t) :: kinds, ids
	type(string_vector_t) :: names, taken

	kinds = new_integer_vector()
	ids   = new_integer_vector()
	names = new_string_vector()
	taken = new_string_vector()

	if (allocated(em%bare_local)) deallocate(em%bare_local)

	fn => em%fns%fns(decl%id_index)

	if (allocated(decl%params)) then
		do i = 1, size(decl%params)
			if (is_method .and. i == 1) then
				pname = '0self'
			else
				k = i
				if (is_method) k = i - 1
				pname = fn%param_names%v(k)%s
			end if
			call add_cand(0, decl%params(i), name_base(pname, .false.), kinds, ids, names)
		end do
	end if

	if (allocated(decl%body)) then
		call walk_binders(em, decl%body, .true., kinds, ids, names)
	end if

	nl = 0
	do j = 1, ids%len_
		nl = max(nl, ids%v(j))
	end do
	allocate(em%bare_local(nl))
	em%bare_local = .false.

	do j = 1, ids%len_
		if (.not. bare_ok(names%v(j)%s, .false.)) cycle

		low = to_lower(names%v(j)%s)
		if (has_name(em%bare_names, low)) cycle
		if (has_name(taken, low)) cycle
		call taken%push(low)

		em%bare_local(ids%v(j)) = .true.
	end do

end subroutine collect_locals

!===============================================================================

recursive subroutine emit_fn(em, decl, self_sk)

	! Emit a user fn as a Fortran procedure.  It is always `recursive`, which
	! also makes all of its local variables automatic (not `save`)
	!
	! By-value parameters are copied into a local at entry, so that the body is
	! free to assign to them.  By-reference parameters are `intent(inout)`, and
	! allocatable if they are arrays or strings so that they can be resized

	! A method has a struct as the first parameter, which is its `self`.  Pass
	! `self_sk`, the position of the struct in the table, for a method

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl
	integer, intent(in), optional :: self_sk

	!********

	character(len = :), allocatable :: name, header, dummy_names, pname, local, dims, line

	integer :: i, k, slot

	logical :: is_method

	type(value_t) :: pval

	integer, allocatable :: ref_slots(:)

	logical :: is_void, ok, is_const, use_direct

	type(fn_t), pointer :: fn
	type(string_vector_t) :: ret_decls, entry_lines
	type(syntax_node_t) :: pnode

	if (decl%src_id > 0) then
		em%cur_src_id  = decl%src_id
		em%cur_src_pos = decl%src_pos
	end if

	fn => em%fns%fns(decl%id_index)

	! Fresh procedure state
	em%body  = new_string_vector()
	em%decls = new_string_vector()
	em%seen_local = new_integer_vector()
	if (allocated(em%local_slots)) deallocate(em%local_slots)
	em%top_level = .false.
	em%loop_depth = 0
	em%break_label = ''
	em%indent = 1

	ret_decls   = new_string_vector()
	entry_lines = new_string_vector()

	is_void = fn%type%type == void_type

	is_method = present(self_sk)

	call collect_locals(em, decl, is_method)

	name = fn_name(em, decl%identifier%text, decl%id_index)

	! Slots of the parameters passed by reference, which the body can assign to
	allocate(ref_slots(0))
	if (allocated(decl%params) .and. allocated(decl%is_ref)) then
		do i = 1, size(decl%params)
			if (decl%is_ref(i)) ref_slots = [ref_slots, decl%params(i)]
		end do
	end if

	dummy_names = ''
	if (allocated(decl%params)) then
		do i = 1, size(decl%params)
			slot  = decl%params(i)

			if (is_method .and. i == 1) then
				pname = '0self'
				pval%type = struct_type
				pval%struct_cookie = em%structs%table(self_sk)%val%cookie
				pval%struct_name = em%structs%table(self_sk)%key
			else
				k = i
				if (is_method) k = i - 1
				pname = fn%param_names%v(k)%s
				pval = fn%params(k)
			end if

			! A node that stands for the parameter, to name and declare it
			pnode%is_loc = .true.
			pnode%id_index = slot
			pnode%identifier%text = pname

			local = var_name(em, pnode)

			dims = ''
			if (is_arr(pval)) dims = dims_of(pval%array%rank)

			if (len(dummy_names) > 0) dummy_names = dummy_names//', '

			is_const = .false.
			if (allocated(decl%is_const_ref)) is_const = decl%is_const_ref(i)

			if (is_const) then
				! `&const`: passed by reference but read-only.  The dummy argument
				! is the variable itself, with no copy of it needed
				dummy_names = dummy_names//local
				call ret_decls%push(elem_spec(em, pval)//', intent(in) :: '// &
					local//dims)
				call em%seen_local%push(2 * slot + 1)
				call record_slot(em, .true., slot, pval)
					if (is_method .and. i == 1) em%local_slots(slot)%fname = local
				cycle
			end if

			if (allocated(decl%is_ref)) then
				if (decl%is_ref(i)) then
					! Passed by reference: the dummy argument is the variable
					dummy_names = dummy_names//local
					line = decl_line(em, pval, local, ok)
					if (.not. ok) then
						call em_unsupported(em, 'a parameter of type `'// &
							kind_name(pval%type)//'`')
					else
						! Declare as an intent(inout) dummy: insert the intent
						! after the type spec
						line = insert_intent(line, 'intent(inout)')
						call ret_decls%push(line)
					end if
					call em%seen_local%push(2 * slot + 1)
					call record_slot(em, .true., slot, pval)
					if (is_method .and. i == 1) em%local_slots(slot)%fname = local
					cycle
				end if
			end if

			use_direct = .false.
			if (is_arr(pval) .or. pval%type == str_type) then
				! Copying a scalar is free.  An array or string is only worth not
				! copying if it can't be affected by anything else
				use_direct = .not. modifies_slot(decl%body, slot)
				if (use_direct) use_direct = .not. alias_hazard(decl%body, &
					decl%id_index, ref_slots)
			end if

			if (use_direct) then
				! Passed by value, and never assigned to, so the dummy argument can
				! be used directly instead of copying it
				dummy_names = dummy_names//local
				call ret_decls%push(elem_spec(em, pval)//', intent(in) :: '// &
					local//dims)
				call em%seen_local%push(2 * slot + 1)
				call record_slot(em, .true., slot, pval)
					if (is_method .and. i == 1) em%local_slots(slot)%fname = local
				cycle
			end if

			! Passed by value, and assigned to: a read-only dummy argument, copied
			! into the local
			dummy_names = dummy_names//local//'_a'
			call ret_decls%push(elem_spec(em, pval)//', intent(in) :: '// &
				local//'_a'//dims)
			call declare_var(em, pnode, pval)
			call entry_lines%push(local//' = '//local//'_a')
		end do
	end if

	do i = 1, entry_lines%len_
		call em_line(em, entry_lines%v(i)%s)
	end do

	if (is_void) then
		header = 'recursive subroutine '//name//'('//dummy_names//')'
	else
		header = 'recursive function '//name//'('//dummy_names//') result(res_)'

		line = decl_line(em, fn%type, 'res_', ok)
		if (.not. ok) then
			call em_unsupported(em, 'a fn that returns type `'//kind_name(fn%type%type)//'`')
		else
			call ret_decls%push(line)
		end if
	end if

	call emit_stmt(em, decl%body)

	if (is_void) then
		call push_proc(em, header, 'end subroutine '//name, ret_decls)
	else
		call push_proc(em, header, 'end function '//name, ret_decls)
	end if

	em%top_level = .true.
	if (allocated(em%bare_local)) deallocate(em%bare_local)

	call value_destroy(pval)

end subroutine emit_fn

!===============================================================================

function struct_slot_by_id(em, id) result(k)

	! Position in the struct table of the struct that the parser numbered `id`

	type(emitter_t), intent(in) :: em
	integer, intent(in) :: id
	integer :: k

	integer :: i

	k = 0
	if (.not. associated(em%structs)) return
	if (.not. allocated(em%structs%table)) return

	do i = 1, size(em%structs%table)
		if (.not. allocated(em%structs%table(i)%key)) cycle
		if (.not. allocated(em%structs%table(i)%val)) cycle
		if (em%structs%table(i)%id_index == id) then
			k = i
			return
		end if
	end do

end function struct_slot_by_id

!===============================================================================

subroutine check_struct_members(em, node)

	! Report a struct member whose type can't be a component of a derived type
	! yet, at the declaration of the struct

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: mname, line

	integer :: i, k

	logical :: ok

	type(value_t) :: mval

	k = struct_slot_by_id(em, node%id_index)
	if (k == 0) return

	do i = 1, em%structs%table(k)%val%num_vars
		call struct_member(em, k, i, mval, mname, ok)
		if (.not. ok) cycle

		line = decl_line(em, mval, 'm', ok)
		if (.not. ok) then
			call em_unsupported(em, 'a struct member of type `'//kind_name(mval%type)//'`')
			call value_destroy(mval)
			return
		end if
	end do

	call value_destroy(mval)

end subroutine check_struct_members

!===============================================================================

function member_str(em, v, x) result(r)

	! Fortran expression for the string of the member of a struct whose type is
	! `v` and designator is `x`.  A struct's members round trip, which is to say
	! that strings are quoted and the types of i64 and f32 are written

	type(emitter_t), intent(inout) :: em
	type(value_t), intent(in) :: v
	character(len = *), intent(in) :: x
	character(len = :), allocatable :: r

	integer :: k, t

	t = elem_type(v)

	if (is_arr(v)) then
		select case (t)
		case (i64_type, f32_type, str_type)
			r = 'rt_str_a_rt('//x//')'

		case (enum_type)
			k = enum_slot_of(em, v)
			if (k == 0) then
				call em_unsupported(em, 'an enum of unknown type')
				r = "''"
			else
				r = 'rt_str_a('//enum_fn(em, k, 'strs')//'('//x//'))'
			end if

		case (struct_type)
			r = str_of(em, v, x)

		case default
			r = 'rt_str_a('//x//')'
		end select
		return
	end if

	select case (t)
	case (i64_type)
		r = 'rt_str('//x//') // "''i64"'
	case (f32_type)
		r = 'trim(adjustl(rt_str('//x//'))) // "''f32"'
	case (f64_type)
		r = 'trim(adjustl(rt_str('//x//')))'
	case (str_type)
		r = 'rt_quote('//x//')'
	case (enum_type, struct_type, fn_type)
		r = str_of(em, v, x)
	case default
		r = 'rt_str('//x//')'
	end select

end function member_str

!===============================================================================

recursive function val_level(em, v) result(lv)

	! How far down the types that a type is made of go, which is the order that
	! the derived types have to be defined in: a struct comes after the types of
	! its members, and a fn pointer after the types in its signature

	type(emitter_t), intent(inout) :: em
	type(value_t), intent(in) :: v
	integer :: lv

	integer :: k

	lv = 0

	if (elem_type(v) == struct_type) then
		k = struct_slot_of(em, v)
		if (k > 0) lv = 1 + struct_level(em, k)

	else if (v%type == fn_type) then
		lv = 1 + sig_level(em, fptr_slot(em, v))

	end if

end function val_level

!===============================================================================

recursive function struct_level(em, k) result(lv)

	type(emitter_t), intent(inout) :: em
	integer, intent(in) :: k
	integer :: lv

	character(len = :), allocatable :: mname

	integer :: m

	logical :: ok

	type(value_t) :: mval

	lv = 0
	do m = 1, em%structs%table(k)%val%num_vars
		call struct_member(em, k, m, mval, mname, ok)
		if (.not. ok) cycle
		lv = max(lv, val_level(em, mval))
	end do

	call value_destroy(mval)

end function struct_level

!===============================================================================

recursive function sig_level(em, s) result(lv)

	type(emitter_t), intent(inout) :: em
	integer, intent(in) :: s
	integer :: lv

	integer :: i

	type(value_t) :: sig

	lv = 0

	! The signature may get more signatures added below, which moves the vector
	sig = em%sigs%v(s)

	if (allocated(sig%fn_params)) then
		do i = 1, size(sig%fn_params)
			lv = max(lv, val_level(em, sig%fn_params(i)))
		end do
	end if
	if (allocated(sig%fn_ret)) lv = max(lv, val_level(em, sig%fn_ret))

	! Explicitly, like the rest of the code does, since a signature nests
	call value_destroy(sig)

end function sig_level

!===============================================================================

subroutine emit_type_decls(em, order)

	! The derived types of the structs at positions `order` of the table, and of
	! the fn pointers.  A fn pointer type is an abstract interface of a fn with
	! its signature, and a derived type that has a procedure pointer of that
	! interface, so that a fn pointer is a value which can be a component, an
	! argument, or a result.  Each type comes after the types that it uses

	type(emitter_t), intent(inout) :: em
	integer, intent(in) :: order(:)

	!********

	character(len = :), allocatable :: line, mname, fname, id, tn, sn

	integer :: i, j, k1, k2, k3, m, n, s, nsig
	integer, allocatable :: kind_(:), idx(:), lev(:)

	logical :: ok

	type(value_t) :: mval, sig

	! Register the fn pointers in members of structs, and those that they have
	! in their own signatures
	do i = 1, size(order)
		m = struct_level(em, order(i))
	end do
	s = 1
	do while (s <= em%sigs%len_)
		m = sig_level(em, s)
		s = s + 1
	end do

	nsig = em%sigs%len_
	n = size(order) + nsig
	allocate(kind_(n), idx(n), lev(n))

	do i = 1, size(order)
		kind_(i) = 1
		idx(i) = order(i)
		lev(i) = struct_level(em, order(i))
	end do
	do s = 1, nsig
		kind_(size(order) + s) = 2
		idx(size(order) + s) = s
		lev(size(order) + s) = sig_level(em, s)
	end do

	! Insertion sort by level, which keeps the order of the structs, which is the
	! order that they were declared in, and then of the signatures
	do i = 2, n
		! Copies, since the elements that are shifted below include these
		k1 = kind_(i)
		k2 = idx(i)
		k3 = lev(i)
		call insert_sorted(i, k1, k2, k3)
	end do

	do i = 1, n

		if (kind_(i) == 1) then

			m = idx(i)
			tn = struct_tname(em, m)
			call em%tdecls%push('    type :: '//tn)
			do j = 1, em%structs%table(m)%val%num_vars
				call struct_member(em, m, j, mval, mname, ok)
				if (.not. ok) cycle
				fname = member_fname(em, m, j)
				line = decl_line(em, mval, fname, ok)
				if (.not. ok) line = 'integer(int32) :: '//fname
				if (fname == mname) then
					call em%tdecls%push('        '//line)
				else
					call em%tdecls%push('        '//line//'  ! '//mname)
				end if
			end do
			call em%tdecls%push('    end type '//tn)
			call em%tdecls%push('')

		else

			s = idx(i)
			id = str(s)
			sn = 'fnptr_i'//id
			sig = em%sigs%v(s)

			line = ''
			if (allocated(sig%fn_params)) then
				do j = 1, size(sig%fn_params)
					if (j > 1) line = line//', '
					line = line//'a'//str(j)
				end do
			end if

			if (allocated(sig%fn_ret)) then
				if (sig%fn_ret%type == void_type) then
					call em%tdecls%push('    abstract interface')
					call em%tdecls%push('        subroutine '//sn//'('//line//')')
				else
					call em%tdecls%push('    abstract interface')
					call em%tdecls%push('        function '//sn//'('//line//') result(r)')
				end if
			end if

			! The names of the module aren't known in an interface body otherwise
			call em%tdecls%push('            import')

			if (allocated(sig%fn_params)) then
				do j = 1, size(sig%fn_params)
					line = elem_spec(em, sig%fn_params(j))//', intent(in) :: a'//str(j)
					if (is_arr(sig%fn_params(j))) &
						line = line//dims_of(sig%fn_params(j)%array%rank)
					call em%tdecls%push('            '//line)
				end do
			end if

			if (allocated(sig%fn_ret)) then
				if (sig%fn_ret%type == void_type) then
					call em%tdecls%push('        end subroutine '//sn)
				else
					line = decl_line(em, sig%fn_ret, 'r', ok)
					if (.not. ok) line = 'integer(int32) :: r'
					call em%tdecls%push('            '//line)
					call em%tdecls%push('        end function '//sn)
				end if
			end if
			call em%tdecls%push('    end interface')
			call em%tdecls%push('    type :: fnptr_t'//id)
			call em%tdecls%push('        procedure('//sn//'), pointer, nopass :: p => null()')
			call em%tdecls%push('    end type fnptr_t'//id)
			call em%tdecls%push('')

		end if
	end do

	call value_destroy(mval)
	call value_destroy(sig)

contains

	subroutine insert_sorted(pos, k_, ix_, lv_)
		integer, intent(in) :: pos, k_, ix_, lv_
		integer :: p
		p = pos - 1
		do while (p >= 1)
			if (lev(p) <= lv_) exit
			kind_(p + 1) = kind_(p)
			idx(p + 1) = idx(p)
			lev(p + 1) = lev(p)
			p = p - 1
		end do
		kind_(p + 1) = k_
		idx(p + 1) = ix_
		lev(p + 1) = lv_
	end subroutine insert_sorted

end subroutine emit_type_decls

!===============================================================================

subroutine emit_struct_procs(em)

	! The derived types of the structs, and fns that map a struct to its string.
	! A struct is a derived type with a component `m<i>` for the member that the
	! parser numbered i.  The fns are
	!
	!   stN_str(x)      the string of a struct, like `Point{x = 1, y = 2}`
	!   stN_join(a)     the strings of a rank 1 array of structs, as a list
	!   stN_fill(v, n)  a rank 1 array of n copies of v
	!
	! Allocatable components aren't copied right by spread(), so stN_fill()
	! assigns each element instead
	!
	! The types are emitted in the order that the structs were declared in, which
	! is the order that they depend on each other in

	type(emitter_t), intent(inout) :: em

	!********

	character(len = :), allocatable :: id, tn, line, mname

	integer :: i, j, k, m, n
	integer, allocatable :: order(:)

	logical :: dup, ok

	type(value_t) :: mval

	if (.not. associated(em%structs)) return
	if (.not. allocated(em%structs%table)) then
		call emit_type_decls(em, [integer ::])
		return
	end if

	! Table positions of the structs, without duplicates by cookie, by id
	n = 0
	allocate(order(size(em%structs%table)))
	do k = 1, size(em%structs%table)
		if (.not. allocated(em%structs%table(k)%key)) cycle
		if (.not. allocated(em%structs%table(k)%val)) cycle
		if (.not. allocated(em%structs%table(k)%val%cookie)) cycle

		dup = .false.
		do j = 1, k - 1
			if (.not. allocated(em%structs%table(j)%val)) cycle
			if (.not. allocated(em%structs%table(j)%val%cookie)) cycle
			if (em%structs%table(j)%val%cookie == em%structs%table(k)%val%cookie) dup = .true.
		end do
		if (dup) cycle

		n = n + 1
		order(n) = k
	end do

	! Insertion sort by id
	do i = 2, n
		k = order(i)
		j = i - 1
		do while (j >= 1)
			if (em%structs%table(order(j))%id_index <= em%structs%table(k)%id_index) exit
			order(j + 1) = order(j)
			j = j - 1
		end do
		order(j + 1) = k
	end do

	call emit_type_decls(em, order(1: n))

	do i = 1, n
		k = order(i)
		id = str(em%structs%table(k)%id_index)
		tn = struct_tname(em, k)

		associate (st => em%structs%table(k)%val)

			! The string of one struct
			call em%procs%push('    function '//tn//'_str(x) result(s)')
			call em%procs%push('        type('//tn//'), intent(in) :: x')
			call em%procs%push('        character(len = :), allocatable :: s')
			call em%procs%push("        s = '"//em%structs%table(k)%key//"{'")
			do m = 1, st%num_vars
				call struct_member(em, k, m, mval, mname, ok)
				if (.not. ok) cycle
				if (m > 1) call em%procs%push("        s = s // ', '")
				call em%procs%push("        s = s // '"//mname//" = '")
				call em%procs%push('        s = s // '//member_str(em, mval, 'x%'//member_fname(em, k, m)))
			end do
			call em%procs%push("        s = s // '}'")
			call em%procs%push('    end function '//tn//'_str')
			call em%procs%push('')

			! The strings of an array
			call em%procs%push('    function '//tn//'_join(a) result(s)')
			call em%procs%push('        type('//tn//'), intent(in) :: a(:)')
			call em%procs%push('        character(len = :), allocatable :: s')
			call em%procs%push('        integer :: i')
			call em%procs%push("        s = ''")
			call em%procs%push('        do i = 1, size(a)')
			call em%procs%push("            if (i > 1) s = s // ', '")
			call em%procs%push('            s = s // '//tn//'_str(a(i))')
			call em%procs%push('        end do')
			call em%procs%push('    end function '//tn//'_join')
			call em%procs%push('')

			! n copies
			call em%procs%push('    function '//tn//'_fill(v, n) result(r)')
			call em%procs%push('        type('//tn//'), intent(in) :: v')
			call em%procs%push('        integer(int64), intent(in) :: n')
			call em%procs%push('        type('//tn//'), allocatable :: r(:)')
			call em%procs%push('        integer(int64) :: i')
			call em%procs%push('        allocate(r(max(n, 0_int64)))')
			call em%procs%push('        do i = 1, n')
			call em%procs%push('            r(i) = v')
			call em%procs%push('        end do')
			call em%procs%push('    end function '//tn//'_fill')
			call em%procs%push('')

		end associate
	end do

	call value_destroy(mval)

end subroutine emit_struct_procs

!===============================================================================

recursive subroutine emit_struct_methods(em, decl, done)

	! Emit the methods of a struct as procedures.  `done` is as for
	! emit_module_fns()

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl
	type(integer_vector_t), intent(inout) :: done

	integer :: j, k, m
	logical :: found

	if (.not. allocated(decl%members)) return

	k = struct_slot_by_id(em, decl%id_index)
	if (k == 0) return

	do j = 1, size(decl%members)
		if (decl%members(j)%kind /= fn_declaration) cycle

		found = .false.
		do m = 1, done%len_
			if (done%v(m) == decl%members(j)%id_index) found = .true.
		end do
		if (found) cycle

		call done%push(decl%members(j)%id_index)
		call emit_fn(em, decl%members(j), k)
	end do

end subroutine emit_struct_methods

!===============================================================================

subroutine emit_enum_procs(em)

	! Helper fns of each enum, as module procedures.  An enum value in the
	! generated program is the zero-based index of its variant, so that its name
	! is known when printing, even when variants are aliases with equal backing
	! values:
	!
	!   enumN_strs(i)  the name of variant i, like `Suit.Clubs`, as an rt_str_t
	!   enumN_str(i)   the same as a string, for a single variant
	!   enumN_val(i)   the backing value of variant i, `i32(Suit.Clubs)`
	!   enumN_of(v)    the first variant with backing value v, `Suit(v)`
	!
	! They're elemental so that they work on arrays of enums too

	type(emitter_t), intent(inout) :: em

	!********

	character(len = :), allocatable :: id, nm
	integer :: i, j, k
	logical :: dup, first

	if (.not. associated(em%enums)) return
	if (.not. allocated(em%enums%table)) return

	do k = 1, size(em%enums%table)
		if (.not. allocated(em%enums%table(k)%key)) cycle
		if (.not. allocated(em%enums%table(k)%val)) cycle
		if (.not. allocated(em%enums%table(k)%val%cookie)) cycle

		! An enum may be in the table under more than one name.  This is the one
		! that enum_slot_of() finds first
		dup = .false.
		do j = 1, k - 1
			if (.not. allocated(em%enums%table(j)%val)) cycle
			if (.not. allocated(em%enums%table(j)%val%cookie)) cycle
			if (em%enums%table(j)%val%cookie == em%enums%table(k)%val%cookie) dup = .true.
		end do
		if (dup) cycle

		id = enum_tname(em, k)
		nm = em%enums%table(k)%key

		associate (e => em%enums%table(k)%val)

			call em%procs%push('    elemental function '//id//'_strs(v) result(r)')
			call em%procs%push('        integer(int32), intent(in) :: v')
			call em%procs%push('        type(rt_str_t) :: r')
			call em%procs%push('        select case (v)')
			do i = 1, e%num_vars
				call em%procs%push('        case ('//str(i - 1)//')')
				call em%procs%push("            r%s = '"//nm//'.'//e%variant_names%v(i)%s//"'")
			end do
			call em%procs%push('        case default')
			call em%procs%push("            r%s = '"//nm//".<invalid>'")
			call em%procs%push('        end select')
			call em%procs%push('    end function '//id//'_strs')
			call em%procs%push('')

			call em%procs%push('    function '//id//'_str(v) result(r)')
			call em%procs%push('        integer(int32), intent(in) :: v')
			call em%procs%push('        character(len = :), allocatable :: r')
			call em%procs%push('        select case (v)')
			do i = 1, e%num_vars
				call em%procs%push('        case ('//str(i - 1)//')')
				call em%procs%push("            r = '"//nm//'.'//e%variant_names%v(i)%s//"'")
			end do
			call em%procs%push('        case default')
			call em%procs%push("            r = '"//nm//".<invalid>'")
			call em%procs%push('        end select')
			call em%procs%push('    end function '//id//'_str')
			call em%procs%push('')

			call em%procs%push('    elemental function '//id//'_val(v) result(r)')
			call em%procs%push('        integer(int32), intent(in) :: v')
			call em%procs%push('        integer(int32) :: r')
			call em%procs%push('        select case (v)')
			do i = 1, e%num_vars
				call em%procs%push('        case ('//str(i - 1)//')')
				call em%procs%push('            r = '//str(e%variant_values(i)))
			end do
			call em%procs%push('        case default')
			call em%procs%push('            r = -1')
			call em%procs%push('        end select')
			call em%procs%push('    end function '//id//'_val')
			call em%procs%push('')

			call em%procs%push('    elemental function '//id//'_of(v) result(r)')
			call em%procs%push('        integer(int32), intent(in) :: v')
			call em%procs%push('        integer(int32) :: r')
			call em%procs%push('        select case (v)')
			do i = 1, e%num_vars
				! Only the first variant with a value, since `case` can't repeat one
				first = .true.
				do j = 1, i - 1
					if (e%variant_values(j) == e%variant_values(i)) first = .false.
				end do
				if (.not. first) cycle
				call em%procs%push('        case ('//str(e%variant_values(i))//')')
				call em%procs%push('            r = '//str(i - 1))
			end do
			call em%procs%push('        case default')
			call em%procs%push('            r = -1')
			call em%procs%push('        end select')
			call em%procs%push('    end function '//id//'_of')
			call em%procs%push('')

		end associate

	end do

end subroutine emit_enum_procs

!===============================================================================

function insert_intent(line, intent_str) result(r)

	! Add an attribute to a declaration like `real(real32) :: x` or
	! `integer(int32), allocatable :: x(:)`, before its `::`

	character(len = *), intent(in) :: line, intent_str
	character(len = :), allocatable :: r

	integer :: i

	i = index(line, ' :: ')
	if (i == 0) then
		r = line
	else
		r = line(1: i-1)//', '//intent_str//line(i:)
	end if

end function insert_intent

!===============================================================================

module subroutine transpile_tree(tree, state, t, diags)

	type(syntax_node_t), intent(in) :: tree
	type(state_t), intent(in), target :: state
	type(transpile_t), intent(inout) :: t
	type(string_vector_t), intent(out) :: diags

	!********

	integer :: i, last
	logical :: no_diags

	type(emitter_t) :: em
	type(integer_vector_t) :: done
	type(string_vector_t) :: rt, main_decls, src

	em%fns => state%fns
	em%enums => state%enums
	em%structs => state%structs
	em%tdecls = new_string_vector()
	em%sigs = new_value_vector()
	em%sig_keys = new_string_vector()
	em%print_result = t%print_result
	em%trim_result  = t%trim_result

	em%body  = new_string_vector()
	em%decls = new_string_vector()
	em%gdecls = new_string_vector()
	em%procs = new_string_vector()
	em%diags = new_string_vector()
	em%seen_local  = new_integer_vector()
	em%seen_global = new_integer_vector()
	em%break_label = ''

	if (tree%kind /= translation_unit .or. .not. allocated(tree%members)) then
		call em_unsupported(em, 'this kind of input')
	end if

	call collect_names(em, tree)

	! Top-level statements go in the main procedure.  The value of the last one is
	! the program's result.  This comes before the fns so that the types of the
	! global variables, which fns can use, are known by then
	em%top_level = .true.
	em%indent = 1

	last = 0
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			! Declarations aren't statements that run, so they have no value
			select case (tree%members(i)%kind)
			case (fn_declaration, struct_declaration, enum_declaration)
			case default
				last = i
			end select
		end do
	end if

	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind == fn_declaration) cycle
			if (i == last) then
				call emit_result_stmt(em, tree%members(i))
			else
				call emit_stmt(em, tree%members(i))
			end if
		end do
	end if
	if (last == 0) call emit_result_invalid(em)

	main_decls = new_string_vector()
	call push_proc(em, 'subroutine syntran_main()', 'end subroutine syntran_main', main_decls)

	! Fns have their own state, so they don't interleave with the above.  Those of
	! the modules come with their own diagnostics, in the module's source
	done = new_integer_vector()
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind == struct_declaration) then
				call emit_struct_methods(em, tree%members(i), done)
				cycle
			end if
			if (tree%members(i)%kind /= fn_declaration) cycle
			call done%push(tree%members(i)%id_index)
			call emit_fn(em, tree%members(i))
		end do
		do i = 1, size(tree%members)
			if (tree%members(i)%kind /= use_statement) cycle
			if (.not. allocated(tree%members(i)%member)) cycle
			call emit_module_fns(em, tree%members(i)%member, done)
		end do
	end if

	call emit_enum_procs(em)
	call emit_struct_procs(em)

	!********
	! Assemble the program

	diags = em%diags
	no_diags = diags%len_ == 0

	src = new_string_vector()

	rt = transpile_rt_src()
	call src%push_all(rt)

	call src%push('')
	call src%push('!'//repeat('=', 79))
	call src%push('')
	call src%push('! Generated by syntran --transpile')
	call src%push('')
	call src%push('module syntran_prog')
	call src%push('')
	call src%push('    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64')
	call src%push('    use syntran_rt')
	call src%push('    implicit none')
	call src%push('')
	do i = 1, em%tdecls%len_
		call push_wrapped(src, em%tdecls%v(i)%s)
	end do
	do i = 1, em%gdecls%len_
		call push_wrapped(src, '    '//em%gdecls%v(i)%s)
	end do
	if (em%gdecls%len_ > 0) call src%push('')
	call src%push('contains')
	call src%push('')
	call src%push_all(em%procs)
	call src%push('end module syntran_prog')
	call src%push('')
	call src%push('program syntran_program')
	call src%push('    use syntran_prog, only: syntran_main')
	call src%push('    implicit none')
	call src%push('    call syntran_main()')
	call src%push('end program syntran_program')
	call src%push('')

	if (no_diags) t%src = src

	! Explicitly, since fn pointer types nest
	if (allocated(em%sigs%v)) call value_array_destroy(em%sigs%v)

end subroutine transpile_tree

!===============================================================================

end submodule syntran__transpile_stmt

!===============================================================================

