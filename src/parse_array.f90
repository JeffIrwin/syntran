
!===============================================================================

submodule (syntran__parse_m) syntran__parse_array

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

recursive module subroutine parse_array_expr(parser, expr)

	! These are the possible kinds of array literals:
	!
	!     unif_array :  [0; 6] or [0; 2, 3]           uniform (constant) value, size after `;`
	!     bound_array:  [0: 6]                        bounded range with default step=1
	!     step_array :  [0: 2: 6] or [0.0: 2.0: 6.0]  range with given step
	!     len_array  :  [0.0: 4.0; 3]                 range with len (inclusive of upper bound!)
	!     expl_array :  [0, 1, 2, 3, 4, 5]            explicit csv vector
	!     size_array :  [0,1,2,3,4,5 ; 2,3]           explicit csv multi-rank array

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: expr

	!********

	integer :: span_beg, span_end, pos0, lb_beg, lb_end, ub_beg, ub_end, rank_, i

	logical :: enum_mismatch, range_non_num

	type(syntax_node_t)  :: lbound_, step, ubound_, len_, elem
	type(syntax_node_vector_t) :: elems, size_
	type(syntax_token_t) :: lbracket, rbracket, colon, semicolon, comma, dummy
	type(text_span_t) :: span

	!print *, 'starting parse_array_expr()'

	! This function parses arrays of the following forms:
	!
	!     // i32
	!     let a = [imin:        imax];      // current loop syntax
	!     let a = [imin: istep: imax];
	!     let a = [iconst           ; len]; // this one is like Rust
	!
	!     // f32
	!     let a = [fmin: fstep: fmax];      // consistent with i32
	!     let a = [fmin:        fmax; len]; // no default unit step like i32
	!     let a = [fconst           ; len];
	!
	!     // Rank-2, rank-3, etc.  No range variations, only all elements
	!     // the same value
	!     let a = [fconst           ; rows, cols];  // row-major like Fortran
	!     let a = [fconst           ; rows, cols, sheets];
	!
	!     // Explicit list for any rank-1 type
	!     [elem_0, elem_1, elem_2, ... ]
	!
	!     // Explicit list and size for higher ranks
	!     [elem_0, elem_1, elem_2, ... ; size_0, size_1, ... ]
	!
	! A note on the term "rank-1":  Maybe there's an argument to be made that
	! for a language with 0-based arrays, we should call vectors "rank-0" and
	! matrices "rank-1".  However, I'm calling them "rank-1" and "rank-2"
	! respectively, as that's what Fortran calls them and I hadn't thought that
	! far ahead :).  Anyway, a "3D" vector is always like [x, y, z] -- C doesn't
	! call that a 4D vector despite being 0-based.
	!
	! NumPy uses the same convention for "rank-1" as us.  In fact, NumPy has
	! something below a vector called a "rank-0" array :exploding-head:

	call parser%match(lbracket_token, lbracket)

	span_beg = parser%peek_pos(0)
	lb_beg   = span_beg
	call parser%parse_expr(expr=lbound_)
	call parser%check_enum_name_value(lbound_)
	span_end = parser%peek_pos(0) - 1
	lb_end   = span_end

	!print *, 'lbound_ = ', parser%text(span_beg, span_end)

	! Arrays of fn pointers are not supported (v1): eval_array.f90's per-type
	! storage/copy paths have no fn_type case, so letting this through would
	! either hit the IC_ALLOC_ARRAY_TYPE internal error (uniform-value form) or
	! segfault outright (explicit-list form, since a het-array check alone
	! can't catch same-signature fn-pointer elements).  Caught here for
	! lbound_, which every array-literal form is seeded from, so this single
	! check covers all of them
	if (lbound_%val%type == fn_type) then
		span = new_span(lb_beg, lb_end - lb_beg + 1)
		call parser%diagnostics%push(err_fn_ptr_array( &
			parser%context(), span, parser%text(lb_beg, lb_end)))
	end if

	if (parser%current_kind() == semicolon_token) then

		! Implicit constant-value array form [lbound; len]

		call parser%match(semicolon_token, semicolon)

		! rank-2+ arrays:
		!
		! [lbound; rows, cols]
		! [lbound; rows, cols, sheets, ...]

		call parser%parse_size(size_)

		call parser%match(rbracket_token, rbracket)

		allocate(expr%val%array)

		expr%kind           = array_expr
		expr%val%type        = array_type
		if (allocated(lbound_%val%struct_name)) then
			expr%val%struct_name = lbound_%val%struct_name
		else if (allocated(lbound_%val%enum_name)) then
			expr%val%enum_name = lbound_%val%enum_name
			if (allocated(lbound_%val%enum_cookie)) &
				expr%val%enum_cookie = lbound_%val%enum_cookie
		end if

		if (lbound_%val%type == array_type) then
			! Only push in the final pass: in pass 0, a forward-referenced fn
			! call's type may still be unresolved, which would falsely trip
			! this check.  parse_unit() (parse_misc.f90) reports pass 1's
			! diagnostics but falls back to pass 0's list when pass 1 comes
			! back clean -- an ungated push here would leave a bogus pass-0
			! diagnostic for that fallback to wrongly resurrect, rejecting a
			! valid program
			if (parser%ipass /= 0) then
				span = new_span(lb_beg, lb_end - lb_beg + 1)
				call parser%diagnostics%push(err_non_sca_val( &
					parser%context(), span, parser%text(lb_beg, lb_end), &
					"uniform"))
			end if
		end if

		expr%val%array%type = lbound_%val%type
		expr%val%array%kind = unif_array
		expr%val%array%rank = size_%len_

		! Move children (avoids deep copies)
		call syntax_node_move(lbound_, expr%lbound)
		allocate(expr%size(size_%len_))
		do i = 1, size_%len_
			call syntax_node_move_into(size_%v(i), expr%size(i))
		end do

		return

	end if

	if (parser%current_kind() == colon_token) then

		! Implicit array form unit step [lbound: ubound] or [lbound: step: ubound]
		call parser%match(colon_token, colon)

		span_beg = parser%peek_pos(0)
		ub_beg   = span_beg
		call parser%parse_expr(expr=ubound_)
		span_end = parser%peek_pos(0) - 1
		ub_end   = span_end

		!print *, 'lbound_ type = ', kind_name(lbound_%val%type)
		!print *, 'ubound_ type = ', kind_name(ubound_%val%type)

		range_non_num = .not. all(is_num_type([lbound_%val%type, ubound_%val%type]))
		if (range_non_num) then

			! Only push in the final pass: a forward-referenced fn call's type
			! may still be unresolved in pass 0 (see err_non_sca_val above)
			if (parser%ipass /= 0) then
				span = new_span(lb_beg, ub_end - lb_beg + 1)
				call parser%diagnostics%push(err_non_num_range( &
					parser%context(), span, parser%text(lb_beg, ub_end)))
			end if

		end if

		if (parser%current_kind() == colon_token) then

			! Implicit form [lbound: step: ubound]

			! Step has just been parsed as ubound above
			step = ubound_

			call parser%match(colon_token, colon)

			span_beg = parser%peek_pos(0)
			call parser%parse_expr(expr=ubound_)
			span_end = parser%peek_pos(0) - 1
			ub_end   = span_end

			if (.not. is_num_type(ubound_%val%type)) then
				range_non_num = .true.
				! Only push in the final pass (see err_non_sca_val above)
				if (parser%ipass /= 0) then
					span = new_span(span_beg, span_end - span_beg + 1)
					call parser%diagnostics%push(err_non_num_range( &
						parser%context(), span, &
						parser%text(span_beg, span_end)))
				end if
			end if

			! If [lbound_: step: ubound] are all specified, then specifying the
			! len would be overconstrained!  Next token must be rbracket

			call parser%match(rbracket_token, rbracket)

			allocate(expr%val%array)

			expr%kind           = array_expr
			expr%val%type       = array_type

			if (all(i32_type == &
				[lbound_%val%type, step%val%type, ubound_%val%type]) .or. &
				all(f32_type == &
				[lbound_%val%type, step%val%type, ubound_%val%type]) .or. &
				all(f64_type == &
				[lbound_%val%type, step%val%type, ubound_%val%type])) then

				expr%val%array%type = lbound_%val%type

			else if (all(is_int_type( &
				[lbound_%val%type, step%val%type, ubound_%val%type]))) then

				expr%val%array%type = i64_type

			else
				! Not a uniform i32/f32/f64 triple (caught above) and not all
				! int types (caught above) -- the only way to land here with
				! numeric operands is a type mismatch between lbound/step/ubound,
				! e.g. [1: 2.0: 5] or [1: 2: 5.0].  Explicitly set unknown_type
				! (instead of leaving the just-allocated array%type
				! uninitialized) so downstream consumers (e.g.
				! is_binary_op_allowed()) hit their existing unknown_type
				! cascade-suppression instead of comparing against garbage
				expr%val%array%type = unknown_type
				! Only push in the final pass (see err_non_sca_val above), and
				! only if a non-numeric operand hasn't already been reported
				! above (avoid a redundant cascade)
				if (parser%ipass /= 0 .and. .not. range_non_num) then
					span = new_span(lb_beg, ub_end - lb_beg + 1)
					call parser%diagnostics%push(err_bound_type_mismatch( &
						parser%context(), span))
				end if
			end if

			expr%val%array%kind = step_array
			expr%val%array%rank = 1

			call syntax_node_move(lbound_, expr%lbound)
			call syntax_node_move(step,    expr%step)
			call syntax_node_move(ubound_, expr%ubound)

			return

		end if

		if (parser%current_kind() == semicolon_token) then

			! Implicit form [lbound: ubound_; len]

			call parser%match(semicolon_token, semicolon)

			span_beg = parser%peek_pos(0)
			call parser%parse_expr(expr=len_)
			span_end = parser%peek_pos(0) - 1

			!print *, 'len_ = ', parser%text(span_beg, span_end)

			if (.not. any(len_%val%type == [i32_type, i64_type])) then
				! Length is not an integer type
				! Only push in the final pass (see err_non_sca_val above)
				if (parser%ipass /= 0) then
					span = new_span(span_beg, span_end - span_beg + 1)
					call parser%diagnostics%push(err_non_int_len( &
						parser%context(), span, &
						parser%text(span_beg, span_end)))
				end if
			end if

			! This used to be checked further up before i64 arrays
			if (ubound_%val%type /= lbound_%val%type) then
				! lbound_ type and ubound_ type do not match for length-based array
				! Only push in the final pass (see err_non_sca_val above)
				if (parser%ipass /= 0) then
					span = new_span(lb_beg, ub_end - lb_beg + 1)
					call parser%diagnostics%push(err_bound_type_mismatch( &
						parser%context(), span))
				end if
			end if

			if (.not. any(lbound_%val%type == [f32_type, f64_type])) then
				! Only push in the final pass (see err_non_sca_val above)
				if (parser%ipass /= 0) then
					span = new_span(lb_beg, lb_end - lb_beg + 1)
					call parser%diagnostics%push(err_non_float_len_range( &
						parser%context(), span, &
						parser%text(lb_beg, lb_end)))
				end if
			end if

			call parser%match(rbracket_token, rbracket)

			allocate(expr%val%array)

			expr%kind           = array_expr
			expr%val%type       = array_type
			expr%val%array%type = lbound_%val%type
			expr%val%array%kind = len_array
			expr%val%array%rank = 1

			call syntax_node_move(lbound_, expr%lbound)
			call syntax_node_move(ubound_, expr%ubound)
			call syntax_node_move(len_,    expr%len_)

			return

		end if

		! Implicit array form unit step [lbound: ubound]

		call parser%match(rbracket_token, rbracket)

		!print *, 'lbound_ = ', lbound_%str()
		!print *, 'ubound_ = ', ubound_%str()

		allocate(expr%val%array)

		expr%kind = array_expr
		expr%val%type = array_type
		expr%val%array%kind = bound_array
		expr%val%array%rank = 1

		call syntax_node_move(lbound_, expr%lbound)
		call syntax_node_move(ubound_, expr%ubound)

		! Read type info from moved children (not from consumed locals)
		if (all(i32_type == &
			[expr%lbound%val%type, expr%ubound%val%type])) then

			expr%val%array%type = expr%lbound%val%type

		else if (all(is_int_type( &
			[expr%lbound%val%type, expr%ubound%val%type]))) then

			expr%val%array%type = i64_type

		else
			! Explicitly set unknown_type (instead of leaving the
			! just-allocated array%type uninitialized) so downstream
			! consumers (e.g. is_binary_op_allowed()) hit their existing
			! unknown_type cascade-suppression instead of comparing against
			! garbage
			expr%val%array%type = unknown_type
			! Only push in the final pass (see err_non_sca_val above), and
			! only if a non-numeric operand hasn't already been reported
			! above (avoid a redundant cascade)
			if (parser%ipass /= 0 .and. .not. range_non_num) then
				if (expr%lbound%val%type == expr%ubound%val%type) then
					! Same (numeric) type on both sides, e.g. [1.0: 5.0] --
					! an implicit unit-step range specifically needs integer
					! bounds
					span = new_span(span_beg, span_end - span_beg + 1)
					call parser%diagnostics%push(err_non_int_range( &
						parser%context(), span, &
						parser%text(span_beg, span_end)))
				else
					! Numeric but mismatched types, e.g. [1: 2.0]
					span = new_span(lb_beg, ub_end - lb_beg + 1)
					call parser%diagnostics%push(err_bound_type_mismatch( &
						parser%context(), span))
				end if
			end if
		end if

		return

	end if

	! Explicit array form [elem_0, elem_1, elem_2, ... ].  elem_0 has already been
	! parsed as lbound above

	!print *, 'elem ', lbound_%val%str()
	if (lbound_%val%type == array_type) then

		! Fortran actually allows concatenating multi-rank arrays.  It just
		! reshapes them to a vector and then concatenates.  I don't think I like
		! that
		rank_ = lbound_%val%array%rank
		!print *, "rank = ", lbound_%val%array%rank
		if (rank_ /= 1) then
			span = new_span(lb_beg, lb_end - lb_beg + 1)
			call parser%diagnostics%push( &
				err_bad_cat_rank(parser%context(), span, rank_, &
				parser%text(lb_beg, lb_end)) &
			)
		end if

	end if

	elems = new_syntax_node_vector()
	call elems%push(lbound_)
	do while (&
		parser%current_kind() /= rbracket_token  .and. &
		parser%current_kind() /= semicolon_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		call parser%match(comma_token, comma)

		! Allow a trailing comma before `]` or `;`, e.g. [10, 20, 30, ]
		if (parser%current_kind() == rbracket_token .or. &
			parser%current_kind() == semicolon_token) exit

		span_beg = parser%peek_pos(0)
		call parser%parse_expr(expr=elem)
		call parser%check_enum_name_value(elem)
		span_end = parser%peek_pos(0) - 1

		!print *, 'elem ', elem%val%str()

		if (elem%val%type /= lbound_%val%type) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_het_array( &
				parser%context(), span, parser%text(span_beg, span_end)))
		else if (elem%val%type == enum_type) then
			! Matching `type == enum_type` isn't enough -- two different
			! enums (e.g. Suit vs Card) must not be mixed in one array
			! literal.  Mirrors the cross-enum operand check for binary ops
			if (allocated(elem%val%enum_cookie) .and. &
				allocated(lbound_%val%enum_cookie)) then
				enum_mismatch = elem%val%enum_cookie /= lbound_%val%enum_cookie
			else
				enum_mismatch = elem%val%enum_name /= lbound_%val%enum_name
			end if
			if (enum_mismatch) then
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push(err_het_array( &
					parser%context(), span, parser%text(span_beg, span_end)))
			end if
		end if

		if (elem%val%type == array_type) then
			! This check could be DRY'd up but it only happens twice
			rank_ = elem%val%array%rank
			!print *, "rank = ", elem%val%array%rank
			if (rank_ /= 1) then
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push( &
					err_bad_cat_rank(parser%context(), span, rank_, &
					parser%text(span_beg, span_end)) &
				)
			end if
		end if

		call elems%push(elem)

		! break infinite loop
		if (parser%pos == pos0) call parser%next(dummy)

	end do

	if (parser%current_kind() == semicolon_token) then

		! Explicit rank-2+ size_array: [elem_0, elem_1, elem_2, ... ; size_0, size_1, ... ];
		call parser%match(semicolon_token, semicolon)

		if (lbound_%val%type == array_type) then
			span = new_span(lb_beg, lb_end - lb_beg + 1)
			call parser%diagnostics%push(err_non_sca_val( &
				parser%context(), span, parser%text(lb_beg, lb_end), &
				"sized"))
		end if

		span_beg = parser%peek_pos(0)
		call parser%parse_size(size_)
		span_end = parser%peek_pos(0) - 1

		call parser%match(rbracket_token, rbracket)

		! If every declared size is a literal constant, the element count can
		! be validated right now (E102) instead of waiting for the runtime
		! check (R21) in eval_array_expr()/vm_exec.f90.  A non-literal size
		! (e.g. from a variable) still falls through to R21
		if (parser%ipass /= 0) then
			block
				logical :: all_lit
				integer(kind = 8) :: total
				character(len = :), allocatable :: dims

				all_lit = .true.
				total   = 1
				dims    = ''
				do i = 1, size_%len_
					if (size_%v(i)%kind /= literal_expr .or. &
						.not. is_int_type(size_%v(i)%val%type)) then
						all_lit = .false.
						exit
					end if
					total = total * size_%v(i)%val%to_i64()
					if (i > 1) dims = dims//' x '
					dims = dims//str(size_%v(i)%val%to_i64())
				end do

				if (all_lit .and. int(elems%len_, 8) /= total) then
					span = new_span(span_beg, span_end - span_beg + 1)
					call parser%diagnostics%push(err_expl_array_size( &
						parser%context(), span, elems%len_, dims, total))
				end if
			end block
		end if

		allocate(expr%val%array)

		expr%kind           = array_expr
		expr%val%type       = array_type
		expr%val%array%type = lbound_%val%type
		expr%val%array%kind = size_array
		expr%val%array%rank = size_%len_

		allocate(expr%size(size_%len_))
		do i = 1, size_%len_
			call syntax_node_move_into(size_%v(i), expr%size(i))
		end do
		allocate(expr%elems(elems%len_))
		do i = 1, elems%len_
			call syntax_node_move_into(elems%v(i), expr%elems(i))
		end do

		return

	end if

	! Explicit rank-1 array (size is implicitly defined by number of elements)

	call parser%match(rbracket_token, rbracket)

	allocate(expr%val%array)
	expr%kind            = array_expr
	expr%val%type        = array_type
	if (allocated(lbound_%val%struct_name)) then
		expr%val%struct_name = lbound_%val%struct_name
	else if (allocated(lbound_%val%enum_name)) then
		expr%val%enum_name = lbound_%val%enum_name
		if (allocated(lbound_%val%enum_cookie)) &
			expr%val%enum_cookie = lbound_%val%enum_cookie
	end if

	expr%val%array%type = lbound_%val%type
	if (lbound_%val%type == array_type) then
		expr%val%array%type = lbound_%val%array%type
	end if

	expr%val%array%kind = expl_array
	expr%val%array%rank = 1
	expr%val%array%len_ = elems%len_

	allocate(expr%elems(elems%len_))
	do i = 1, elems%len_
		call syntax_node_move_into(elems%v(i), expr%elems(i))
	end do

end subroutine parse_array_expr

!===============================================================================

recursive module subroutine parse_subscripts(parser, expr)

	! Parse array subscripts, if present

	class(parser_t) :: parser
	type(syntax_node_t), intent(inout) :: expr

	!********

	integer :: pos0, span0, span1, expect_rank, rank_, ls_beg, ls_end, &
		us_beg, us_end, nelem_subs

	logical :: has_char_sub

	type(syntax_node_t) :: lsubscript, usubscript, ssubscript
	type(syntax_node_vector_t) :: lsubscripts_vec, usubscripts_vec, &
		ssubscripts_vec
	type(syntax_token_t) :: lbracket, rbracket, comma, &
		dummy, colon, dcolon

	type(text_span_t) :: span

	if (parser%current_kind() /= lbracket_token) return

	!print *, 'parsing subscripts'

	lsubscripts_vec = new_syntax_node_vector()  ! lower-bounds
	usubscripts_vec = new_syntax_node_vector()  ! upper-bounds
	ssubscripts_vec = new_syntax_node_vector()  ! steps

	call parser%match(lbracket_token, lbracket)

	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		pos0  = parser%pos
		span0 = parser%current_pos()

		! Reset omit flags for this dimension (lsubscript is reused across
		! loop iterations, so stale flags from prior dims must be cleared)
		lsubscript%lsub_omit = .false.
		lsubscript%usub_omit = .false.

		if (parser%current_kind() == double_colon_token) then
			! [::...] at start — empty step between two colons: error
			span = new_span(span0, parser%current_pos() - span0)
			call parser%diagnostics%push( &
				err_empty_step(parser%context(), span))
			call parser%match(double_colon_token, dcolon)  ! consume :: for recovery
			lsubscript%sub_kind = range_sub
			lsubscript%lsub_omit = .true.
			if (parser%current_kind() == rbracket_token .or. &
				parser%current_kind() == comma_token .or. &
				parser%current_kind() == eof_token) then
				lsubscript%usub_omit = .true.
			else
				us_beg = parser%current_pos()
				call parser%parse_expr(expr=usubscript)
				us_end = parser%current_pos()
			end if

		else if (parser%current_kind() == colon_token) then
			! Lower bound is absent (or bare all_sub)
			call parser%match(colon_token, colon)
			lsubscript%lsub_omit = .true.

			if (parser%current_kind() == rbracket_token .or. &
				parser%current_kind() == comma_token) then
				! Bare [:] — whole dimension, keep existing all_sub semantics
				lsubscript%sub_kind = all_sub
				lsubscript%lsub_omit = .false.

			else

				! Parse the first expr after the leading colon.  For one-colon
				! form this is the upper bound; for two-colon (step) form it
				! turns out to be the step (same rearrangement as below).
				us_beg = parser%current_pos()
				call parser%parse_expr(expr=usubscript)
				us_end = parser%current_pos()

				if (.not. any(usubscript%val%type == [i32_type, i64_type, unknown_type])) then
					span = new_span(us_beg, us_end - us_beg + 1)
					call parser%diagnostics%push( &
						err_non_int_subscript(parser%context(), span, &
						parser%text(span0, parser%current_pos()-1)))
				end if

				if (parser%current_kind() == colon_token) then
					! Two colons: step_sub.  The expr we parsed was the step.
					call parser%match(colon_token, colon)
					lsubscript%sub_kind = step_sub
					ssubscript = usubscript

					if (parser%current_kind() == rbracket_token .or. &
						parser%current_kind() == comma_token) then
						! [:step:] — upper omitted
						lsubscript%usub_omit = .true.
					else
						us_beg = parser%current_pos()
						call parser%parse_expr(expr=usubscript)
						us_end = parser%current_pos()
						if (.not. any(usubscript%val%type == [i32_type, i64_type, unknown_type])) then
							span = new_span(us_beg, us_end - us_beg + 1)
							call parser%diagnostics%push( &
								err_non_int_subscript(parser%context(), span, &
								parser%text(span0, parser%current_pos()-1)))
						end if
					end if
				else
					! One colon, lower omitted: [:upper]
					lsubscript%sub_kind = range_sub
				end if
			end if

		else

			ls_beg = parser%current_pos()
			call parser%parse_expr(expr=lsubscript)
			ls_end = parser%current_pos()

			!print *, 'lsubscript = ', lsubscript%str()
			!print *, 'lsubscript = ', parser%text(span0, parser%current_pos()-1)
			!print *, "sub type = ", kind_name(lsubscript%val%type)

			if (lsubscript%val%type == array_type) then
				lsubscript%sub_kind = arr_sub

				rank_ = lsubscript%val%array%rank
				span = new_span(ls_beg, ls_end - ls_beg + 1)
				!print *, "rank_ = ", rank_
				if (rank_ /= 1) then
					call parser%diagnostics%push( &
						err_bad_sub_rank(parser%context(), span, rank_) &
					)
				end if
				if (.not. any(lsubscript%val%array%type == [i32_type, i64_type, unknown_type])) then
					call parser%diagnostics%push( &
						err_non_int_subscript(parser%context(), span, &
						parser%text(span0, parser%current_pos()-1)))
				end if

			else

				if (.not. any(lsubscript%val%type == [i32_type, i64_type, unknown_type])) then
					span = new_span(span0, parser%current_pos() - span0)
					call parser%diagnostics%push( &
						err_non_int_subscript(parser%context(), span, &
						parser%text(span0, parser%current_pos()-1)))
				end if

				if (parser%current_kind() == colon_token) then
					call parser%match(colon_token, colon)
					lsubscript%sub_kind = range_sub

					if (parser%current_kind() == colon_token) then
						! [lower: :...] (space-separated) — empty step: error
						span = new_span(span0, parser%current_pos() - span0)
						call parser%diagnostics%push( &
							err_empty_step(parser%context(), span))
						! best-effort recovery: treat upper as omitted
						lsubscript%usub_omit = .true.

					else if (parser%current_kind() == rbracket_token .or. &
						parser%current_kind() == comma_token) then
						! [lower:] — upper omitted
						lsubscript%usub_omit = .true.

					else
						us_beg = parser%current_pos()
						call parser%parse_expr(expr=usubscript)
						us_end = parser%current_pos()

						if (.not. any(usubscript%val%type == [i32_type, i64_type, unknown_type])) then
							span = new_span(us_beg, us_end - us_beg + 1)
							call parser%diagnostics%push( &
								err_non_int_subscript(parser%context(), span, &
								parser%text(span0, parser%current_pos()-1)))
						end if

						if (parser%current_kind() == colon_token) then
							call parser%match(colon_token, colon)
							lsubscript%sub_kind = step_sub

							! The last expr was step, not upper (same swap as before)
							ssubscript = usubscript

							if (parser%current_kind() == rbracket_token .or. &
								parser%current_kind() == comma_token) then
								! [lower:step:] — upper omitted
								lsubscript%usub_omit = .true.
							else
								us_beg = parser%current_pos()
								call parser%parse_expr(expr=usubscript)
								us_end = parser%current_pos()
								if (.not. any(usubscript%val%type == [i32_type, i64_type, unknown_type])) then
									span = new_span(us_beg, us_end - us_beg + 1)
									call parser%diagnostics%push( &
										err_non_int_subscript(parser%context(), span, &
										parser%text(span0, parser%current_pos()-1)))
								end if
							end if

						end if
					end if

				else if (parser%current_kind() == double_colon_token) then
					! [lower::upper] — empty step (no space, lexed as ::): error
					span = new_span(span0, parser%current_pos() - span0)
					call parser%diagnostics%push( &
						err_empty_step(parser%context(), span))
					call parser%match(double_colon_token, dcolon)  ! consume :: for recovery
					lsubscript%sub_kind = range_sub
					if (parser%current_kind() == rbracket_token .or. &
						parser%current_kind() == comma_token .or. &
						parser%current_kind() == eof_token) then
						lsubscript%usub_omit = .true.
					else
						us_beg = parser%current_pos()
						call parser%parse_expr(expr=usubscript)
						us_end = parser%current_pos()
					end if

				else
					lsubscript%sub_kind = scalar_sub
				end if
				!print *, kind_name(subscript%sub_kind)
			end if

		end if

		! Parallel arrays subscripts and usubscripts should be same size? Not
		! sure if this is ideal for multi-rank ranges
		call lsubscripts_vec%push(lsubscript)
		call ssubscripts_vec%push(ssubscript)
		call usubscripts_vec%push(usubscript)

		! Break infinite loop
		if (parser%pos == pos0) call parser%next(dummy)

		if (parser%current_kind() /= rbracket_token) then
			call parser%match(comma_token, comma)
		end if

	end do

	!print *, 'parsing rbracket'
	call parser%match(rbracket_token, rbracket)
	!print *, 'done'

	call syntax_nodes_copy(expr%lsubscripts, &
		lsubscripts_vec%v( 1: lsubscripts_vec%len_ ))

	call syntax_nodes_copy(expr%ssubscripts, &
		ssubscripts_vec%v( 1: ssubscripts_vec%len_ ))

	call syntax_nodes_copy(expr%usubscripts, &
		usubscripts_vec%v( 1: usubscripts_vec%len_ ))

	! Do some type juggling which the caller used to do

	span1 = parser%current_pos() - 1
	if (expr%val%type == array_type) then

		!print *, 'sub kind = ', kind_name(expr%lsubscripts(1)%sub_kind)

		! Capture element rank before we overwrite it.  For string arrays we
		! allow an optional extra (rank+1-th) subscript that indexes into the
		! characters of each selected element.
		nelem_subs = expr%val%array%rank
		has_char_sub = (expr%val%array%type == str_type) .and. &
			(size(expr%lsubscripts) == nelem_subs + 1)

		if (all(expr%lsubscripts(1:nelem_subs)%sub_kind == scalar_sub)) then
			! this is not necessarily true for strings
			expr%val%type = expr%val%array%type
		else if (expr%val%array%type == struct_type) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push(err_array_struct_slice( &
				parser%context(), &
				span, &
				expr%identifier%text))
		end if

		! Allow rank or (for str arrays) rank+1 subscripts
		if (size(expr%lsubscripts) /= nelem_subs .and. .not. has_char_sub) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push( &
				err_bad_sub_count(parser%context(), span, &
				expr%identifier%text, &
				expr%val%array%rank, size(expr%lsubscripts)))
		end if

		! A slice operation can change the result rank.  The char sub (if any)
		! is NOT counted — it never adds array rank.
		!print *, 'rank in  = ', expr%val%array%rank
		expr%val%array%rank = count(expr%lsubscripts(1:nelem_subs)%sub_kind /= scalar_sub)
		!print *, 'rank out = ', expr%val%array%rank

	else if (expr%val%type == str_type) then
		!print *, 'string type'

		expect_rank = 1
		if (size(expr%lsubscripts) /= expect_rank) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push( &
				err_bad_sub_count(parser%context(), span, &
				expr%identifier%text, &
				expect_rank, size(expr%lsubscripts)))
		end if

	else if (expr%val%type /= unknown_type) then
		span = new_span(span0, span1 - span0 + 1)
		!print *, "err_scalar_subscript 1"
		call parser%diagnostics%push( &
			err_scalar_subscript(parser%context(), &
			span, expr%identifier%text))
	end if

end subroutine parse_subscripts

!===============================================================================

module subroutine parse_size(parser, size)

	class(parser_t) :: parser

	type(syntax_node_vector_t), intent(out) :: size

	!********

	integer :: span_beg, span_end, pos0

	type(syntax_node_t)  :: len
	type(syntax_token_t) :: comma, dummy
	type(text_span_t) :: span

	size = new_syntax_node_vector()
	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos

		span_beg = parser%peek_pos(0)
		call parser%parse_expr(expr=len)
		span_end = parser%peek_pos(0) - 1

		!print *, 'len = ', parser%text(span_beg, span_end)

		if (.not. any(len%val%type == [i32_type, i64_type])) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_non_int_size( &
				parser%context(), span, &
				parser%text(span_beg, span_end)))
		end if

		call size%push(len)

		! break infinite loop?
		if (parser%pos == pos0) call parser%next(dummy)

		if (parser%current_kind() /= rbracket_token) then
			call parser%match(comma_token, comma)
		end if

	end do

end subroutine parse_size

!===============================================================================

end submodule syntran__parse_array

!===============================================================================

