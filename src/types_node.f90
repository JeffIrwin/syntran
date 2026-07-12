
!===============================================================================

submodule (syntran__types_m) syntran__types_node

	implicit none

!===============================================================================

contains

!===============================================================================

module subroutine new_token(token, kind, pos, text, val)

	type(syntax_token_t), intent(out) :: token

	integer :: kind, pos

	character(len = *) :: text

	type(value_t), optional :: val

	token%kind = kind
	token%pos  = pos
	token%text = text

	! Not `token%val = val` -- same class of gfortran/mingw defined-
	! assignment bug as push_value() in value.f90
	if (present(val)) call value_copy(token%val, val)

end subroutine new_token

!===============================================================================

module function new_syntax_node_vector() result(vector)

	type(syntax_node_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_node_vector

!===============================================================================

module function new_syntax_token_vector() result(vector)

	type(syntax_token_vector_t) :: vector

	! cap=0 / no pre-allocation: ifort (following the standard) calls value_copy
	! via defined assignment when copying the vector result, passing uninitialized
	! v(:)%val as class(value_t) src — which ifort rejects.  gfortran uses a
	! bitwise copy and never invokes the defined assignment for array components,
	! so the bug is ifort-only.  Starting unallocated means there is nothing to
	! copy and value_copy is never called with uninitialized data.
	vector%len_ = 0
	vector%cap = 0

end function new_syntax_token_vector

!===============================================================================

module function new_literal_value(type, bool, i32, i64, f32, f64, str_) result(val)

	integer, intent(in) :: type

	integer(kind = 4), intent(in), optional :: i32
	integer(kind = 8), intent(in), optional :: i64
	real   (kind = 4), intent(in), optional :: f32
	real   (kind = 8), intent(in), optional :: f64
	logical          , intent(in), optional :: bool
	character(len=*) , intent(in), optional :: str_

	type(value_t) :: val

	val%type = type
	if (present(bool)) val%sca%bool  = bool
	if (present(f32 )) val%sca%f32   = f32
	if (present(f64 )) val%sca%f64   = f64
	if (present(i32 )) val%sca%i32   = i32
	if (present(i64 )) val%sca%i64   = i64
	if (present(str_)) then
		allocate(val%str)
		val%str%s = str_
	end if

end function new_literal_value

!===============================================================================

module subroutine new_declaration_expr(identifier, op, right, expr)

	! TODO: IMO this fn is overly abstracted.  It's only used once, so
	! just paste it their and delete the fn.  That will make it easier to
	! refactor and consolidate declaration_expr and assignment_expr parsing

	type(syntax_token_t), intent(in) :: identifier, op
	type(syntax_node_t) , intent(in) :: right
	type(syntax_node_t) , intent(out) :: expr

	!********

	expr%kind = let_expr

	allocate(expr%right)

	! Not `expr%X = Y` below for identifier/op (syntax_token_t), right
	! (syntax_node_t), or val (value_t) -- same class of gfortran/mingw
	! defined-assignment bug as push_value() in value.f90
	call syntax_token_copy(expr%identifier, identifier)
	call syntax_token_copy(expr%op, op)
	call syntax_node_copy(expr%right, right)

	! Pass the result value type up the tree for type checking in parent
	call value_copy(expr%val, right%val)

end subroutine new_declaration_expr

!===============================================================================

module subroutine new_name_expr(identifier, val, expr)

	type(syntax_token_t), intent(in)  :: identifier
	type(value_t)                     :: val
	type(syntax_node_t), intent(out)  :: expr

	expr%kind = name_expr
	! Not `expr%X = Y` -- same class of gfortran/mingw defined-assignment
	! bug as push_value() in value.f90
	call syntax_token_copy(expr%identifier, identifier)
	call value_copy(expr%val, val)

end subroutine new_name_expr

!===============================================================================

module subroutine new_binary_expr(left, op, right, expr)

	type(syntax_node_t) , intent(inout) :: left, right  ! consumed by move
	type(syntax_token_t), intent(in)    :: op
	type(syntax_node_t) , intent(out)   :: expr

	!********

	integer :: larrtype, rarrtype, type_, ltype, rtype, lrank, rrank, out_rank, elem_type

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'left  = ', left %str()
	if (debug > 1) print *, 'op    = ', op%text
	if (debug > 1) print *, 'right = ', right%str()

	! Read type info before moves (left/right val%array may be moved)
	larrtype = unknown_type
	rarrtype = unknown_type
	lrank    = -1
	rrank    = -1
	if (left %val%type == array_type) then
		larrtype = left %val%array%type
		lrank    = left %val%array%rank
	end if
	if (right%val%type == array_type) then
		rarrtype = right%val%array%type
		rrank    = right%val%array%rank
	end if
	ltype = left%val%type
	rtype = right%val%type

	expr%kind = binary_expr
	! Not `expr%op = op` -- same class of gfortran/mingw defined-assignment
	! bug as push_value() in value.f90
	call syntax_token_copy(expr%op, op)

	call syntax_node_move(left,  expr%left)
	call syntax_node_move(right, expr%right)

	! Special handling for matmul: result rank depends on operand ranks
	if (op%kind == matmul_token) then

		! Promote element type using the same rules as *
		elem_type = get_binary_op_kind(larrtype, star_token, rarrtype, &
			unknown_type, unknown_type)

		out_rank = matmul_out_rank(lrank, rrank)

		if (out_rank == 0) then
			! vector @ vector -> scalar
			expr%val%type = elem_type
		else
			allocate(expr%val%array)
			expr%val%array%type = elem_type
			expr%val%array%rank = out_rank
			expr%val%type = array_type
		end if

		if (debug > 1) print *, 'new_binary_expr = ', expr%str()
		if (debug > 1) print *, 'done new_binary_expr'
		return

	end if

	! Pass the result value type up the tree for type checking in parent
	type_ = get_binary_op_kind(ltype, op%kind, rtype, &
		larrtype, rarrtype)
	!print *, 'type_ = ', kind_name(type_)

	if (any(type_ == [bool_array_type, f32_array_type, f64_array_type, &
		i32_array_type, i64_array_type, str_array_type])) then

		allocate(expr%val%array)

		expr%val%array%type = array_to_scalar_type(type_)
		if (ltype == array_type) then
			expr%val%array%rank = expr%left%val%array%rank
		else
			expr%val%array%rank = expr%right%val%array%rank
		end if

		expr%val%type = array_type

	! TODO: other array sub types.  Maybe make a mold_val() helper fn similar to
	! mold() (for arrays)

	else
		expr%val%type = type_

	end if

	! TODO: array subtype if subscripted?  I think parse_primary_expr should
	! already set the subtype when subscripts are present

	if (debug > 1) print *, 'new_binary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_binary_expr'

end subroutine new_binary_expr

!===============================================================================

module subroutine new_unary_expr(op, right, expr)

	type(syntax_node_t) , intent(inout) :: right   ! consumed by move
	type(syntax_token_t), intent(in)    :: op
	type(syntax_node_t) , intent(out)   :: expr

	!********

	if (debug > 1) print *, 'new_unary_expr'

	expr%kind = unary_expr
	! Not `expr%op = op` / `expr%val = expr%right%val` -- same class of
	! gfortran/mingw defined-assignment bug as push_value() in value.f90
	call syntax_token_copy(expr%op, op)

	call syntax_node_move(right, expr%right)

	! Pass the result value type up the tree for type checking in parent.  IIRC
	! all unary operators result in the same type as their operand, hence there
	! is a get_binary_op_kind() fn but no get_unary_op_kind() fn

	call value_copy(expr%val, expr%right%val)

	if (debug > 1) print *, 'new_unary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_unary_expr'

end subroutine new_unary_expr

!===============================================================================

module subroutine new_bool(bool, expr)

	logical            , intent(in)  :: bool
	type(syntax_node_t), intent(out) :: expr

	expr%kind = literal_expr
	! Not `expr%val = new_literal_value(...)` -- same class of gfortran/
	! mingw defined-assignment bug as push_value() in value.f90
	call value_copy(expr%val, new_literal_value(bool_type, bool = bool))

end subroutine new_bool

!********

module subroutine new_f32(f32, expr)

	real(kind = 4)     , intent(in)  :: f32
	type(syntax_node_t), intent(out) :: expr

	expr%kind = literal_expr
	! Not `expr%val = new_literal_value(...)` -- see new_bool()
	call value_copy(expr%val, new_literal_value(f32_type, f32 = f32))

end subroutine new_f32

!********

module subroutine new_f64(f64, expr)

	real(kind = 8)     , intent(in)  :: f64
	type(syntax_node_t), intent(out) :: expr

	expr%kind = literal_expr
	! Not `expr%val = new_literal_value(...)` -- see new_bool()
	call value_copy(expr%val, new_literal_value(f64_type, f64 = f64))

end subroutine new_f64

!********

module subroutine new_i32(i32, expr)

	integer(kind = 4)  , intent(in)  :: i32
	type(syntax_node_t), intent(out) :: expr

	expr%kind = literal_expr
	! Not `expr%val = new_literal_value(...)` -- see new_bool()
	call value_copy(expr%val, new_literal_value(i32_type, i32 = i32))

end subroutine new_i32

!********

module subroutine new_i64(i64, expr)

	integer(kind = 8)  , intent(in)  :: i64
	type(syntax_node_t), intent(out) :: expr

	expr%kind = literal_expr
	! Not `expr%val = new_literal_value(...)` -- see new_bool()
	call value_copy(expr%val, new_literal_value(i64_type, i64 = i64))

end subroutine new_i64

!********

module subroutine new_str(str_, expr)

	character(len = *) , intent(in)  :: str_
	type(syntax_node_t), intent(out) :: expr

	expr%kind = literal_expr
	! Not `expr%val = new_literal_value(...)` -- see new_bool().  str_type
	! literals are especially likely to hit this: new_literal_value()
	! allocates val%str internally, a populated nested allocatable
	call value_copy(expr%val, new_literal_value(str_type, str_ = str_))

end subroutine new_str

!===============================================================================

end submodule syntran__types_node

!===============================================================================

