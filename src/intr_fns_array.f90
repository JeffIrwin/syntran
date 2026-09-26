
!===============================================================================

module syntran__intr_fns_array_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

function new_reduce_fn(array_name, elem_type, ret_type, has_dim, has_mask) result(fn)

	! Build the signature of one reduction overload that takes extra `dim`
	! and/or `mask` args:
	!
	!     f(array)                 -- not built here, see the plain declarations
	!     f(array, dim)            -- has_dim
	!     f(array, mask)           -- has_mask
	!     f(array, dim, mask)      -- has_dim and has_mask
	!
	! With `dim` the result is an array of `ret_type` whose rank is resolved
	! per-call in resolve_overload(), otherwise it is a scalar `ret_type`

	character(len = *), intent(in) :: array_name
	integer, intent(in) :: elem_type, ret_type
	logical, intent(in) :: has_dim, has_mask

	type(fn_t) :: fn

	!********

	integer :: np, ip

	if (has_dim) then
		fn%type%type = array_type
		allocate(fn%type%array)
		fn%type%array%type = ret_type
		fn%type%array%rank = -1  ! resolved per-call in resolve_overload
	else
		fn%type%type = ret_type
	end if

	np = 1
	if (has_dim ) np = np + 1
	if (has_mask) np = np + 1

	allocate(fn%params(np))
	allocate(fn%param_names%v(np))

	fn%params(1)%type = array_type
	allocate(fn%params(1)%array)
	fn%params(1)%array%type = elem_type
	fn%params(1)%array%rank = -1  ! negative means any rank
	fn%param_names%v(1)%s = array_name

	ip = 1
	if (has_dim) then
		ip = ip + 1
		fn%params(ip)%type = i32_type
		fn%param_names%v(ip)%s = "dim"
	end if

	if (has_mask) then
		ip = ip + 1
		fn%params(ip)%type = array_type
		allocate(fn%params(ip)%array)
		fn%params(ip)%array%type = bool_type
		fn%params(ip)%array%rank = -1
		fn%param_names%v(ip)%s = "mask"
	end if

end function new_reduce_fn

!===============================================================================

subroutine declare_array_fns(fns, id_index, fn_array)

	! Declare array intrinsic functions (size, count, sum, product, etc.)

	type(fns_t), intent(inout) :: fns
	integer, intent(inout) :: id_index
	type(fn_t), allocatable, intent(out) :: fn_array(:)

	!********

	type(fn_t) :: size_fn, count_fn, any_fn, all_fn, &
		minval_i32_fn, minval_i64_fn, minval_f32_fn, minval_f64_fn, &
		maxval_i32_fn, maxval_i64_fn, maxval_f32_fn, maxval_f64_fn, &
		sum_i32_fn, sum_i64_fn, sum_f32_fn, sum_f64_fn, &
		product_i32_fn, product_i64_fn, product_f32_fn, product_f64_fn, &
		norm2_f32_fn, norm2_f64_fn, &
		dot_f32_fn, dot_f64_fn, dot_i32_fn, dot_i64_fn, &
		reshape_fn, transpose_fn, shape_fn

	! Overloads of the reductions that take `dim` and/or `mask` args.  These are
	! resolved from the plain names (e.g. sum) by resolve_overload().  They all
	! share one INTR id per reduction (see intr_id_from_name), which dispatches
	! on the arg types and count at runtime
	integer, parameter :: NUM_EXT = 3 + 4 * 4 * 3
	integer, parameter :: elem_types(4) = [i32_type, i64_type, f32_type, f64_type]
	character(len = 3), parameter :: type_names(4) = ["i32", "i64", "f32", "f64"]
	character(len = 7), parameter :: ops(4) = &
		["sum    ", "minval ", "maxval ", "product"]

	integer :: iop, it, n_ext
	type(fn_t) :: ext_fns(NUM_EXT)

	!********

	size_fn%type%type = i64_type
	allocate(size_fn%params(1))
	allocate(size_fn%param_names%v(1))

	size_fn%params(1)%type = array_type

	allocate(size_fn%params(1)%array)
	size_fn%params(1)%array%type = any_type
	size_fn%params(1)%array%rank = -1  ! negative means any rank

	size_fn%param_names%v(1)%s = "array"

	size_fn%variadic_min = 0
	size_fn%variadic_max = 1
	size_fn%variadic_type = i32_type
	size_fn%variadic_name = "dim"

	call fns%insert("size", size_fn, id_index)

	!********

	count_fn%type%type = i64_type
	allocate(count_fn%params(1))
	allocate(count_fn%param_names%v(1))

	count_fn%params(1)%type = array_type

	allocate(count_fn%params(1)%array)
	count_fn%params(1)%array%type = bool_type
	count_fn%params(1)%array%rank = -1  ! negative means any rank

	count_fn%param_names%v(1)%s = "mask"

	! The `dim` overload is declared separately below as "0count_dim" and picked
	! by resolve_overload() when count() is called with 2 args

	call fns%insert("count", count_fn, id_index)

	!********

	minval_i32_fn%type%type = i32_type
	allocate(minval_i32_fn%params(1))
	allocate(minval_i32_fn%param_names%v(1))

	minval_i32_fn%params(1)%type = array_type

	allocate(minval_i32_fn%params(1)%array)
	minval_i32_fn%params(1)%array%type = i32_type
	minval_i32_fn%params(1)%array%rank = -1  ! negative means any rank

	minval_i32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0minval_i32", minval_i32_fn, id_index)

	!********

	minval_i64_fn%type%type = i64_type
	allocate(minval_i64_fn%params(1))
	allocate(minval_i64_fn%param_names%v(1))

	minval_i64_fn%params(1)%type = array_type

	allocate(minval_i64_fn%params(1)%array)
	minval_i64_fn%params(1)%array%type = i64_type
	minval_i64_fn%params(1)%array%rank = -1  ! negative means any rank

	minval_i64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0minval_i64", minval_i64_fn, id_index)

	!********

	minval_f32_fn%type%type = f32_type
	allocate(minval_f32_fn%params(1))
	allocate(minval_f32_fn%param_names%v(1))

	minval_f32_fn%params(1)%type = array_type

	allocate(minval_f32_fn%params(1)%array)
	minval_f32_fn%params(1)%array%type = f32_type
	minval_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	minval_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0minval_f32", minval_f32_fn, id_index)

	!********

	minval_f64_fn%type%type = f64_type
	allocate(minval_f64_fn%params(1))
	allocate(minval_f64_fn%param_names%v(1))

	minval_f64_fn%params(1)%type = array_type

	allocate(minval_f64_fn%params(1)%array)
	minval_f64_fn%params(1)%array%type = f64_type
	minval_f64_fn%params(1)%array%rank = -1  ! negative means any rank

	minval_f64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0minval_f64", minval_f64_fn, id_index)

	!********

	maxval_i32_fn%type%type = i32_type
	allocate(maxval_i32_fn%params(1))
	allocate(maxval_i32_fn%param_names%v(1))

	maxval_i32_fn%params(1)%type = array_type

	allocate(maxval_i32_fn%params(1)%array)
	maxval_i32_fn%params(1)%array%type = i32_type
	maxval_i32_fn%params(1)%array%rank = -1  ! negative means any rank

	maxval_i32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0maxval_i32", maxval_i32_fn, id_index)

	!********

	maxval_i64_fn%type%type = i64_type
	allocate(maxval_i64_fn%params(1))
	allocate(maxval_i64_fn%param_names%v(1))

	maxval_i64_fn%params(1)%type = array_type

	allocate(maxval_i64_fn%params(1)%array)
	maxval_i64_fn%params(1)%array%type = i64_type
	maxval_i64_fn%params(1)%array%rank = -1  ! negative means any rank

	maxval_i64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0maxval_i64", maxval_i64_fn, id_index)

	!********

	maxval_f32_fn%type%type = f32_type
	allocate(maxval_f32_fn%params(1))
	allocate(maxval_f32_fn%param_names%v(1))

	maxval_f32_fn%params(1)%type = array_type

	allocate(maxval_f32_fn%params(1)%array)
	maxval_f32_fn%params(1)%array%type = f32_type
	maxval_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	maxval_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0maxval_f32", maxval_f32_fn, id_index)

	!********

	maxval_f64_fn%type%type = f64_type
	allocate(maxval_f64_fn%params(1))
	allocate(maxval_f64_fn%param_names%v(1))

	maxval_f64_fn%params(1)%type = array_type

	allocate(maxval_f64_fn%params(1)%array)
	maxval_f64_fn%params(1)%array%type = f64_type
	maxval_f64_fn%params(1)%array%rank = -1  ! negative means any rank

	maxval_f64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0maxval_f64", maxval_f64_fn, id_index)

	!********

	sum_i32_fn%type%type = i32_type
	allocate(sum_i32_fn%params(1))
	allocate(sum_i32_fn%param_names%v(1))

	sum_i32_fn%params(1)%type = array_type

	allocate(sum_i32_fn%params(1)%array)
	sum_i32_fn%params(1)%array%type = i32_type
	sum_i32_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_i32_fn%param_names%v(1)%s =  "array"

	! The `dim` and `mask` overloads are declared separately below, e.g.
	! "0sum_i32_dim", "0sum_i32_mask", and "0sum_i32_dim_mask".  Likewise for
	! minval, maxval, and product

	call fns%insert("0sum_i32", sum_i32_fn, id_index)

	!********

	sum_i64_fn%type%type = i64_type
	allocate(sum_i64_fn%params(1))
	allocate(sum_i64_fn%param_names%v(1))

	sum_i64_fn%params(1)%type = array_type

	allocate(sum_i64_fn%params(1)%array)
	sum_i64_fn%params(1)%array%type = i64_type
	sum_i64_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_i64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_i64", sum_i64_fn, id_index)

	!********

	sum_f32_fn%type%type = f32_type
	allocate(sum_f32_fn%params(1))
	allocate(sum_f32_fn%param_names%v(1))

	sum_f32_fn%params(1)%type = array_type

	allocate(sum_f32_fn%params(1)%array)
	sum_f32_fn%params(1)%array%type = f32_type
	sum_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_f32", sum_f32_fn, id_index)

	!********

	sum_f64_fn%type%type = f64_type
	allocate(sum_f64_fn%params(1))
	allocate(sum_f64_fn%param_names%v(1))

	sum_f64_fn%params(1)%type = array_type

	allocate(sum_f64_fn%params(1)%array)
	sum_f64_fn%params(1)%array%type = f64_type
	sum_f64_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_f64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_f64", sum_f64_fn, id_index)

	!********

	product_i32_fn%type%type = i32_type
	allocate(product_i32_fn%params(1))
	allocate(product_i32_fn%param_names%v(1))

	product_i32_fn%params(1)%type = array_type

	allocate(product_i32_fn%params(1)%array)
	product_i32_fn%params(1)%array%type = i32_type
	product_i32_fn%params(1)%array%rank = -1

	product_i32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0product_i32", product_i32_fn, id_index)

	!********

	product_i64_fn%type%type = i64_type
	allocate(product_i64_fn%params(1))
	allocate(product_i64_fn%param_names%v(1))

	product_i64_fn%params(1)%type = array_type

	allocate(product_i64_fn%params(1)%array)
	product_i64_fn%params(1)%array%type = i64_type
	product_i64_fn%params(1)%array%rank = -1  ! negative means any rank

	product_i64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0product_i64", product_i64_fn, id_index)

	!********

	product_f32_fn%type%type = f32_type
	allocate(product_f32_fn%params(1))
	allocate(product_f32_fn%param_names%v(1))

	product_f32_fn%params(1)%type = array_type

	allocate(product_f32_fn%params(1)%array)
	product_f32_fn%params(1)%array%type = f32_type
	product_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	product_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0product_f32", product_f32_fn, id_index)

	!********

	product_f64_fn%type%type = f64_type
	allocate(product_f64_fn%params(1))
	allocate(product_f64_fn%param_names%v(1))

	product_f64_fn%params(1)%type = array_type

	allocate(product_f64_fn%params(1)%array)
	product_f64_fn%params(1)%array%type = f64_type
	product_f64_fn%params(1)%array%rank = -1  ! negative means any rank

	product_f64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0product_f64", product_f64_fn, id_index)

	!********

	! norm2 only takes float args, not integer, while dot can take integers.
	! This is intentional and matches fortran behavior

	norm2_f32_fn%type%type = f32_type
	allocate(norm2_f32_fn%params(1))
	allocate(norm2_f32_fn%param_names%v(1))

	norm2_f32_fn%params(1)%type = array_type

	allocate(norm2_f32_fn%params(1)%array)
	norm2_f32_fn%params(1)%array%type = f32_type
	norm2_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	norm2_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0norm2_f32", norm2_f32_fn, id_index)

	!********

	norm2_f64_fn%type%type = f64_type
	allocate(norm2_f64_fn%params(1))
	allocate(norm2_f64_fn%param_names%v(1))

	norm2_f64_fn%params(1)%type = array_type

	allocate(norm2_f64_fn%params(1)%array)
	norm2_f64_fn%params(1)%array%type = f64_type
	norm2_f64_fn%params(1)%array%rank = -1  ! negative means any rank

	norm2_f64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0norm2_f64", norm2_f64_fn, id_index)

	!********

	dot_f32_fn%type%type = f32_type

	allocate(dot_f32_fn%params(2))
	allocate(dot_f32_fn%param_names%v(2))

	dot_f32_fn%params(1)%type = array_type
	allocate(dot_f32_fn%params(1)%array)
	dot_f32_fn%params(1)%array%type = f32_type
	dot_f32_fn%params(1)%array%rank = 1
	dot_f32_fn%param_names%v(1)%s =  "vector_a"

	dot_f32_fn%params(2)%type = array_type
	allocate(dot_f32_fn%params(2)%array)
	dot_f32_fn%params(2)%array%type = f32_type
	dot_f32_fn%params(2)%array%rank = 1
	dot_f32_fn%param_names%v(2)%s =  "vector_b"

	call fns%insert("0dot_f32", dot_f32_fn, id_index)

	!********

	dot_f64_fn%type%type = f64_type

	allocate(dot_f64_fn%params(2))
	allocate(dot_f64_fn%param_names%v(2))

	! dot only takes rank-1 arrays, like fortran's dot_product()
	dot_f64_fn%params(1)%type = array_type
	allocate(dot_f64_fn%params(1)%array)
	dot_f64_fn%params(1)%array%type = f64_type
	dot_f64_fn%params(1)%array%rank = 1
	dot_f64_fn%param_names%v(1)%s =  "vector_a"

	dot_f64_fn%params(2)%type = array_type
	allocate(dot_f64_fn%params(2)%array)
	dot_f64_fn%params(2)%array%type = f64_type
	dot_f64_fn%params(2)%array%rank = 1
	dot_f64_fn%param_names%v(2)%s =  "vector_b"

	call fns%insert("0dot_f64", dot_f64_fn, id_index)

	!********

	dot_i32_fn%type%type = i32_type

	allocate(dot_i32_fn%params(2))
	allocate(dot_i32_fn%param_names%v(2))

	dot_i32_fn%params(1)%type = array_type
	allocate(dot_i32_fn%params(1)%array)
	dot_i32_fn%params(1)%array%type = i32_type
	dot_i32_fn%params(1)%array%rank = 1
	dot_i32_fn%param_names%v(1)%s =  "vector_a"

	dot_i32_fn%params(2)%type = array_type
	allocate(dot_i32_fn%params(2)%array)
	dot_i32_fn%params(2)%array%type = i32_type
	dot_i32_fn%params(2)%array%rank = 1
	dot_i32_fn%param_names%v(2)%s =  "vector_b"

	call fns%insert("0dot_i32", dot_i32_fn, id_index)

	!********

	dot_i64_fn%type%type = i64_type

	allocate(dot_i64_fn%params(2))
	allocate(dot_i64_fn%param_names%v(2))

	dot_i64_fn%params(1)%type = array_type
	allocate(dot_i64_fn%params(1)%array)
	dot_i64_fn%params(1)%array%type = i64_type
	dot_i64_fn%params(1)%array%rank = 1
	dot_i64_fn%param_names%v(1)%s =  "vector_a"

	dot_i64_fn%params(2)%type = array_type
	allocate(dot_i64_fn%params(2)%array)
	dot_i64_fn%params(2)%array%type = i64_type
	dot_i64_fn%params(2)%array%rank = 1
	dot_i64_fn%param_names%v(2)%s =  "vector_b"

	call fns%insert("0dot_i64", dot_i64_fn, id_index)

	!********

	all_fn%type%type = bool_type
	allocate(all_fn%params(1))
	allocate(all_fn%param_names%v(1))

	all_fn%params(1)%type = array_type

	allocate(all_fn%params(1)%array)
	all_fn%params(1)%array%type = bool_type
	all_fn%params(1)%array%rank = -1  ! negative means any rank

	all_fn%param_names%v(1)%s = "mask"

	! The `dim` overload is declared separately below as "0all_dim"

	call fns%insert("all", all_fn, id_index)

	!********

	any_fn%type%type = bool_type
	allocate(any_fn%params(1))
	allocate(any_fn%param_names%v(1))

	any_fn%params(1)%type = array_type

	allocate(any_fn%params(1)%array)
	any_fn%params(1)%array%type = bool_type
	any_fn%params(1)%array%rank = -1  ! negative means any rank

	any_fn%param_names%v(1)%s = "mask"

	! The `dim` overload is declared separately below as "0any_dim"

	call fns%insert("any", any_fn, id_index)

	!********

	! std::reshape(source, shape) -- reshape an array to new dimensions.
	! Returns a new array with the same flat buffer but different rank/size.
	! Registered under the "std::" prefix so it must be called as std::reshape().
	! Element type and result rank are resolved per-call in resolve_overload().

	reshape_fn%type%type = array_type
	allocate(reshape_fn%type%array)
	reshape_fn%type%array%type = any_type
	reshape_fn%type%array%rank = -1  ! resolved per-call in resolve_overload

	allocate(reshape_fn%params(2))
	allocate(reshape_fn%param_names%v(2))

	! param 1: source -- array of any element type and rank
	reshape_fn%params(1)%type = array_type
	allocate(reshape_fn%params(1)%array)
	reshape_fn%params(1)%array%type = any_type
	reshape_fn%params(1)%array%rank = -1  ! any rank

	reshape_fn%param_names%v(1)%s = "source"

	! param 2: shape -- rank-1 i32 array giving the target extents
	reshape_fn%params(2)%type = array_type
	allocate(reshape_fn%params(2)%array)
	reshape_fn%params(2)%array%type = i32_type
	reshape_fn%params(2)%array%rank = 1

	reshape_fn%param_names%v(2)%s = "shape"

	call fns%insert("std::reshape", reshape_fn, id_index)

	!********

	! std::transpose(source) -- transpose a rank-2 array.
	! Returns the source with rows and columns exchanged; the flat buffer is
	! physically permuted (column-major).  Element type is preserved and the
	! result is always rank-2.  Registered under "std::" so users may still
	! define their own transpose() without conflict.
	! Element type is resolved per-call in resolve_overload().

	transpose_fn%type%type = array_type
	allocate(transpose_fn%type%array)
	transpose_fn%type%array%type = any_type
	transpose_fn%type%array%rank = 2  ! always rank-2

	allocate(transpose_fn%params(1))
	allocate(transpose_fn%param_names%v(1))

	! param 1: source -- rank-2 array of any element type
	transpose_fn%params(1)%type = array_type
	allocate(transpose_fn%params(1)%array)
	transpose_fn%params(1)%array%type = any_type
	transpose_fn%params(1)%array%rank = 2  ! must be rank-2

	transpose_fn%param_names%v(1)%s = "source"

	call fns%insert("std::transpose", transpose_fn, id_index)

	!********

	! std::shape(source) -- return the extents of source as a rank-1 i64 array.
	! The result length equals the rank of source; result[i] is the size along
	! dimension i.  Return type is i64 for consistency with the size() intrinsic.
	! Registered under "std::" so users may still define their own shape().

	shape_fn%type%type = array_type
	allocate(shape_fn%type%array)
	shape_fn%type%array%type = i64_type
	shape_fn%type%array%rank = 1  ! always rank-1

	allocate(shape_fn%params(1))
	allocate(shape_fn%param_names%v(1))

	! param 1: source -- array of any element type and rank
	shape_fn%params(1)%type = array_type
	allocate(shape_fn%params(1)%array)
	shape_fn%params(1)%array%type = any_type
	shape_fn%params(1)%array%rank = -1  ! any rank

	shape_fn%param_names%v(1)%s = "source"

	call fns%insert("std::shape", shape_fn, id_index)

	!********

	! dim-overloads of count/all/any.  These take a bool array and return a
	! reduced array of i64/bool/bool, or a scalar for rank-1 input

	n_ext = 0

	n_ext = n_ext + 1
	ext_fns(n_ext) = new_reduce_fn("mask", bool_type, i64_type, .true., .false.)
	call fns%insert("0count_dim", ext_fns(n_ext), id_index)

	n_ext = n_ext + 1
	ext_fns(n_ext) = new_reduce_fn("mask", bool_type, bool_type, .true., .false.)
	call fns%insert("0all_dim", ext_fns(n_ext), id_index)

	n_ext = n_ext + 1
	ext_fns(n_ext) = new_reduce_fn("mask", bool_type, bool_type, .true., .false.)
	call fns%insert("0any_dim", ext_fns(n_ext), id_index)

	!********

	! sum/minval/maxval/product with a dim and/or mask arg

	do iop = 1, size(ops)
	do it = 1, size(elem_types)

		n_ext = n_ext + 1
		ext_fns(n_ext) = new_reduce_fn("array", elem_types(it), elem_types(it), &
			.true., .false.)
		call fns%insert("0"//trim(ops(iop))//"_"//type_names(it)//"_dim", &
			ext_fns(n_ext), id_index)

		n_ext = n_ext + 1
		ext_fns(n_ext) = new_reduce_fn("array", elem_types(it), elem_types(it), &
			.false., .true.)
		call fns%insert("0"//trim(ops(iop))//"_"//type_names(it)//"_mask", &
			ext_fns(n_ext), id_index)

		n_ext = n_ext + 1
		ext_fns(n_ext) = new_reduce_fn("array", elem_types(it), elem_types(it), &
			.true., .true.)
		call fns%insert("0"//trim(ops(iop))//"_"//type_names(it)//"_dim_mask", &
			ext_fns(n_ext), id_index)

	end do
	end do

	!********

	! Return array of all functions declared in this module
	fn_array = &
		[ &
			size_fn, count_fn, &
			minval_i32_fn, minval_i64_fn, minval_f32_fn, minval_f64_fn, &
			maxval_i32_fn, maxval_i64_fn, maxval_f32_fn, maxval_f64_fn, &
			sum_i32_fn, sum_i64_fn, sum_f32_fn, sum_f64_fn, &
			product_i32_fn, product_i64_fn, product_f32_fn, product_f64_fn, &
			norm2_f32_fn, norm2_f64_fn, &
			dot_f32_fn, dot_f64_fn, dot_i32_fn, dot_i64_fn, &
			all_fn, any_fn, &
			reshape_fn, &
			transpose_fn, &
			shape_fn, &
			ext_fns &
		]

end subroutine declare_array_fns

!===============================================================================

end module syntran__intr_fns_array_m

!===============================================================================

