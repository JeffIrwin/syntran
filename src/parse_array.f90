
!===============================================================================

submodule (syntran__parse_m) syntran__parse_array

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

recursive module function parse_array_expr(parser) result(expr)

	! These are the possible kinds of array literals:
	!
	!     unif_array :  [0; 6] or [0; 2, 3]           uniform (constant) value, size after `;`
	!     bound_array:  [0: 6]                        bounded range with default step=1
	!     step_array :  [0: 2: 6] or [0.0: 2.0: 6.0]  range with given step
	!     len_array  :  [0.0: 4.0; 3]                 range with len (inclusive of upper bound!)
	!     expl_array :  [0, 1, 2, 3, 4, 5]            explicit csv vector
	!     size_array :  [0,1,2,3,4,5 ; 2,3]           explicit csv multi-rank array

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: span_beg, span_end, pos0, lb_beg, lb_end, ub_beg, ub_end

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

	lbracket = parser%match(lbracket_token)

	span_beg = parser%peek_pos(0)
	lb_beg   = span_beg
	lbound_  = parser%parse_expr()
	span_end = parser%peek_pos(0) - 1
	lb_end   = span_end

	!print *, 'lbound_ = ', parser%text(span_beg, span_end)

	! TODO: should type checking be done by caller, or should we pass an
	! expected type arg for the RHS of this check?

	! TODO: check if lbound_%val is allocated, e.g. for assigning one array to
	! a cat of another?  How would this work for rank-2+?
	!
	!     let a = [0: 3];
	!     let b = [a, 5, 6];
	!              ^ segfault

	!! TODO: there should still be *some* type checking.  At least, implicit
	!! ranges cannot use bool
	!if (lbound_%val%type /= i32_type) then
	!	span = new_span(span_beg, span_end - span_beg + 1)
	!	call parser%diagnostics%push(err_non_int_range( &
	!		parser%context, span, parser%text(span_beg, span_end)))
	!end if

	if (parser%current_kind() == semicolon_token) then

		! Implicit constant-value array form [lbound; len]

		semicolon    = parser%match(semicolon_token)

		! rank-2+ arrays:
		!
		! [lbound; rows, cols]
		! [lbound; rows, cols, sheets, ...]

		size_ = parser%parse_size()

		rbracket = parser%match(rbracket_token)

		allocate(expr%val%array)
		allocate(expr%lbound)

		call syntax_nodes_copy(expr%size, size_%v( 1: size_%len_ ))

		expr%kind           = array_expr

		expr%val%type        = array_type
		expr%val%struct_name = lbound_%val%struct_name

		expr%val%array%type = lbound_%val%type
		expr%val%array%kind = unif_array
		expr%val%array%rank = size_%len_

		!print *, 'expr%val%type       = ', expr%val%type
		!print *, 'expr%val%array%type = ', expr%val%array%type

		! Does this syntax node need to own these members, or can we just save
		! them in the array_t?  I think they do need to be duplicated, as they
		! may be an expression and not just a literal.  So, sizes have to be
		! allocated dynamically during evaluation, not during parsing

		expr%lbound = lbound_

		return

	end if

	if (parser%current_kind() == colon_token) then

		! Implicit array form unit step [lbound: ubound] or [lbound: step: ubound]
		colon    = parser%match(colon_token)

		span_beg = parser%peek_pos(0)
		ub_beg   = span_beg
		ubound_  = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1
		ub_end   = span_end

		!print *, 'lbound_ type = ', kind_name(lbound_%val%type)
		!print *, 'ubound_ type = ', kind_name(ubound_%val%type)

		if (.not. all([ &
			is_num_type(lbound_%val%type), &
			is_num_type(ubound_%val%type)])) then

			span = new_span(lb_beg, ub_end - lb_beg + 1)
			call parser%diagnostics%push(err_non_num_range( &
				parser%context(), span, parser%text(lb_beg, ub_end)))

		end if

		if (parser%current_kind() == colon_token) then

			! Implicit form [lbound: step: ubound]

			! Step has just been parsed as ubound above
			step = ubound_

			colon    = parser%match(colon_token)

			span_beg = parser%peek_pos(0)
			ubound_  = parser%parse_expr()
			span_end = parser%peek_pos(0) - 1

			if (.not. is_num_type(ubound_%val%type)) then
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push(err_non_num_range( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			! If [lbound_: step: ubound] are all specified, then specifying the
			! len would be overconstrained!  Next token must be rbracket

			rbracket = parser%match(rbracket_token)

			allocate(expr%val%array)
			allocate(expr%lbound)
			allocate(expr%step)
			allocate(expr%ubound)

			expr%kind           = array_expr
			expr%val%type       = array_type

			if (all(i32_type == &
				[lbound_%val%type, step%val%type, ubound_%val%type]) .or. &
				all(f32_type == &
				[lbound_%val%type, step%val%type, ubound_%val%type]) .or. &
				all(f64_type == &
				[lbound_%val%type, step%val%type, ubound_%val%type])) then

				expr%val%array%type = lbound_%val%type

			! TODO: make is_int_type() elemental, then we can sugar up this syntax
			else if (all([ &
				is_int_type(lbound_%val%type), &
				is_int_type(step  %val%type), &
				is_int_type(ubound_%val%type)])) then

				expr%val%array%type = i64_type

			else
				! TODO: different message
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push(err_non_int_range( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			expr%val%array%kind = step_array
			expr%val%array%rank = 1

			expr%lbound = lbound_
			expr%step   = step
			expr%ubound = ubound_

			return

		end if

		if (parser%current_kind() == semicolon_token) then

			! Implicit form [lbound: ubound_; len]

			semicolon    = parser%match(semicolon_token)

			span_beg = parser%peek_pos(0)
			len_     = parser%parse_expr()
			span_end = parser%peek_pos(0) - 1

			!print *, 'len_ = ', parser%text(span_beg, span_end)

			if (.not. any(len_%val%type == [i32_type, i64_type])) then
				! Length is not an integer type
				span = new_span(span_beg, span_end - span_beg + 1)
				! TODO: different diag for each (or at least some) case
				call parser%diagnostics%push(err_non_int_len( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			! This used to be checked further up before i64 arrays
			if (ubound_%val%type /= lbound_%val%type) then
				! lbound_ type and ubound_ type do not match for length-based array
				span = new_span(lb_beg, ub_end - lb_beg + 1)
				call parser%diagnostics%push(err_bound_type_mismatch( &
					parser%context(), span))
			end if

			if (.not. any(lbound_%val%type == [f32_type, f64_type])) then
				span = new_span(lb_beg, lb_end - lb_beg + 1)
				call parser%diagnostics%push(err_non_float_len_range( &
					parser%context(), span, &
					parser%text(lb_beg, lb_end)))
			end if

			rbracket = parser%match(rbracket_token)

			allocate(expr%val%array)
			allocate(expr%lbound)
			allocate(expr%ubound)
			allocate(expr%len_)

			expr%kind           = array_expr
			expr%val%type       = array_type
			expr%val%array%type = lbound_%val%type
			expr%val%array%kind = len_array
			expr%val%array%rank = 1

			expr%lbound = lbound_
			expr%ubound = ubound_
			expr%len_   = len_

			return

		end if

		! Implicit array form unit step [lbound: ubound]

		rbracket = parser%match(rbracket_token)

		!print *, 'lbound_ = ', lbound_%str()
		!print *, 'ubound_ = ', ubound_%str()

		allocate(expr%val%array)
		allocate(expr%lbound)
		allocate(expr%ubound)

		expr%kind = array_expr

		expr%val%type = array_type

		expr%val%array%kind = bound_array
		expr%val%array%rank = 1

		expr%lbound = lbound_
		expr%ubound = ubound_

		if (all(i32_type == &
			[lbound_%val%type, ubound_%val%type]) &! .or. &
			) then

			!print *, 'setting lbound_ type'
			expr%val%array%type = lbound_%val%type

		! TODO: make is_int_type() elemental, then we can sugar up this syntax
		else if (all([ &
			is_int_type(lbound_%val%type), &
			is_int_type(ubound_%val%type)])) then

			!print *, 'setting i64_type'
			expr%val%array%type = i64_type

		else
			! TODO: different message
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_non_int_range( &
				parser%context(), span, &
				parser%text(span_beg, span_end)))
		end if

		return

	end if

	! Explicit array form [elem_0, elem_1, elem_2, ... ].  elem_0 has already been
	! parsed as lbound above

	!print *, 'elem ', lbound_%val%str()

	elems = new_syntax_node_vector()
	call elems%push(lbound_)
	do while (&
		parser%current_kind() /= rbracket_token  .and. &
		parser%current_kind() /= semicolon_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		comma    = parser%match(comma_token)

		span_beg = parser%peek_pos(0)
		elem     = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'elem ', elem%val%str()

		if (elem%val%type /= lbound_%val%type) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_het_array( &
				parser%context(), span, parser%text(span_beg, span_end)))
		end if

		call elems%push(elem)

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	if (parser%current_kind() == semicolon_token) then

		! Explicit rank-2+ size_array: [elem_0, elem_1, elem_2, ... ; size_0, size_1, ... ];
		semicolon = parser%match(semicolon_token)

		size_ = parser%parse_size()

		rbracket = parser%match(rbracket_token)

		allocate(expr%val%array)

		call syntax_nodes_copy(expr%size, size_%v( 1: size_%len_ ))

		expr%kind           = array_expr

		expr%val%type       = array_type

		expr%val%array%type = lbound_%val%type
		expr%val%array%kind = size_array
		expr%val%array%rank = size_%len_

		call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len_ ))

		return

	end if

	! Explicit rank-1 array (size is implicitly defined by number of elements)

	rbracket = parser%match(rbracket_token)

	allocate(expr%val%array)
	expr%kind           = array_expr

	!expr%val%type       = lbound_%val%type
	expr%val%type        = array_type
	expr%val%struct_name = lbound_%val%struct_name

	!print *, "expr struct_name = ", expr%val%struct_name

	expr%val%array%type = lbound_%val%type
	if (lbound_%val%type == array_type) then
		! If lbound is another array, get its subtype here instead
		!
		! TODO: only allow array concatenation for rank-1 arrays.  Or check
		! Fortran et al. to see how they handle multi-rank catting

		expr%val%array%type = lbound_%val%array%type
	end if

	expr%val%array%kind = expl_array
	expr%val%array%rank = 1
	expr%val%array%len_ = elems%len_
	!print*, "expl_array"

	call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len_ ))

end function parse_array_expr

!===============================================================================

recursive module subroutine parse_subscripts(parser, expr)

	! Parse array subscripts, if present

	class(parser_t) :: parser
	type(syntax_node_t), intent(inout) :: expr

	!********

	integer :: pos0, span0, span1, expect_rank

	type(syntax_node_t) :: lsubscript, usubscript, ssubscript
	type(syntax_node_vector_t) :: lsubscripts_vec, usubscripts_vec, &
		ssubscripts_vec
	type(syntax_token_t) :: lbracket, rbracket, comma, &
		dummy, colon

	type(text_span_t) :: span

	if (parser%current_kind() /= lbracket_token) return

	!print *, 'parsing subscripts'

	lsubscripts_vec = new_syntax_node_vector()  ! lower-bounds
	usubscripts_vec = new_syntax_node_vector()  ! upper-bounds
	ssubscripts_vec = new_syntax_node_vector()  ! steps

	lbracket  = parser%match(lbracket_token)

	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		pos0  = parser%pos
		span0 = parser%current_pos()

		if (parser%current_kind() == colon_token) then
			lsubscript%sub_kind = all_sub
		else

			lsubscript = parser%parse_expr()

			!print *, 'lsubscript = ', lsubscript%str()
			!print *, 'lsubscript = ', parser%text(span0, parser%current_pos()-1)
			!print *, "sub type = ", kind_name(lsubscript%val%type)

			if (lsubscript%val%type == array_type) then
				lsubscript%sub_kind = arr_sub
				! TODO: check rank-1, check i32 or i64 type
			else

				! TODO: this is some nasty nested logic.  Can we refactor as a
				! fn, invert conditions, never nest, and return early?  I can't
				! cycle here bc there's important stuff at at end of loop.  Goto
				! could work but fn might be better

				if (.not. any(lsubscript%val%type == [i32_type, i64_type])) then
					span = new_span(span0, parser%current_pos() - span0)
					call parser%diagnostics%push( &
						err_non_int_subscript(parser%context(), span, &
						parser%text(span0, parser%current_pos()-1)))
				end if

				if (parser%current_kind() == colon_token) then
					colon = parser%match(colon_token)
					lsubscript%sub_kind = range_sub

					usubscript = parser%parse_expr()
					! TODO: type check i32 usubscript

					if (parser%current_kind() == colon_token) then
						colon = parser%match(colon_token)
						lsubscript%sub_kind = step_sub

						! The last one that we parsed above was actually step, not ubound
						ssubscript = usubscript

						usubscript = parser%parse_expr()
						! TODO: type check i32 usubscript

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
		if (parser%pos == pos0) dummy = parser%next()

		if (parser%current_kind() /= rbracket_token) then
			comma = parser%match(comma_token)
		end if

	end do

	!print *, 'parsing rbracket'
	rbracket  = parser%match(rbracket_token)
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

		if (all(expr%lsubscripts%sub_kind == scalar_sub)) then
			! this is not necessarily true for strings
			expr%val%type = expr%val%array%type
		else if (expr%val%array%type == struct_type) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push(err_array_struct_slice( &
				parser%context(), &
				span, &
				expr%identifier%text))
		end if

		! TODO: allow rank+1 for str arrays
		if (expr%val%array%rank /= size(expr%lsubscripts)) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push( &
				err_bad_sub_count(parser%context(), span, &
				expr%identifier%text, &
				expr%val%array%rank, size(expr%lsubscripts)))
		end if

		! A slice operation can change the result rank

		!print *, 'rank in  = ', expr%val%array%rank
		expr%val%array%rank = count(expr%lsubscripts%sub_kind /= scalar_sub)
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

	else
		span = new_span(span0, span1 - span0 + 1)
		!print *, "err_scalar_subscript 1"
		call parser%diagnostics%push( &
			err_scalar_subscript(parser%context(), &
			span, expr%identifier%text))
	end if

end subroutine parse_subscripts

!===============================================================================

module function parse_size(parser) result(size)

	class(parser_t) :: parser

	type(syntax_node_vector_t) :: size

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
		len      = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'len = ', parser%text(span_beg, span_end)

		if (.not. any(len%val%type == [i32_type, i64_type])) then
			span = new_span(span_beg, span_end - span_beg + 1)
			! TODO: different diag for each (or at least some) case
			call parser%diagnostics%push(err_non_int_len( &
				parser%context(), span, &
				parser%text(span_beg, span_end)))
		end if

		call size%push(len)

		! break infinite loop?
		if (parser%pos == pos0) dummy = parser%next()

		if (parser%current_kind() /= rbracket_token) then
			comma = parser%match(comma_token)
		end if

	end do

end function parse_size

!===============================================================================

end submodule syntran__parse_array

!===============================================================================

