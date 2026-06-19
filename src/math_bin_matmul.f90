
!===============================================================================

module syntran__math_matmul_m

	use syntran__value_m

	implicit none

	interface matmul_
		module procedure matmul_value_t
	end interface matmul_

!===============================================================================

contains

!===============================================================================

subroutine matmul_value_t(left, right, res, op_text)

	! Evaluate the matmul operator `@` for two array operands.
	!
	! Shape rules (same as NumPy @):
	!   rank-1 @ rank-1 -> scalar (dot product)
	!   rank-2 @ rank-1 -> rank-1 vector  (M x V)
	!   rank-1 @ rank-2 -> rank-1 vector  (V x M)
	!   rank-2 @ rank-2 -> rank-2 matrix  (M x N)
	!
	! Arrays are stored column-major: element [i,j] of an (R x C) array
	! lives at flat index  i + R*j  (0-based), so reshape(buf, [R, C])
	! is the correct Fortran 2-D view and matmul() / dot_product() apply
	! directly.

	type(value_t), intent(in)    :: left, right
	type(value_t), intent(inout) :: res
	character(len = *), intent(in) :: op_text

	!********

	integer :: lrank, rrank
	integer(kind = 8) :: lr, lc, rr, rc, out_len

	lrank = left %array%rank
	rrank = right%array%rank

	! ---- vector @ vector -> scalar (dot product) -------------------------
	if (lrank == 1 .and. rrank == 1) then

		! Dimension check
		if (left%array%size(1) /= right%array%size(1)) then
			write(*,*) err_matmul_dim(left%array%size(1), right%array%size(1))
			call internal_error()
		end if

		select case (magic * left%array%type + right%array%type)

		case (magic * i32_type + i32_type)
			res%type = i32_type
			res%sca%i32 = dot_product(left%array%i32, right%array%i32)

		case (magic * i64_type + i64_type)
			res%type = i64_type
			res%sca%i64 = dot_product(left%array%i64, right%array%i64)

		case (magic * f32_type + f32_type)
			res%type = f32_type
			res%sca%f32 = dot_product(left%array%f32, right%array%f32)

		case (magic * f64_type + f64_type)
			res%type = f64_type
			res%sca%f64 = dot_product(left%array%f64, right%array%f64)

		! Mixed-type promotions (cast to the wider type)
		case (magic * i32_type + i64_type)
			res%type = i64_type
			res%sca%i64 = dot_product(int(left%array%i32, 8), right%array%i64)

		case (magic * i64_type + i32_type)
			res%type = i64_type
			res%sca%i64 = dot_product(left%array%i64, int(right%array%i32, 8))

		case (magic * f32_type + f64_type)
			res%type = f64_type
			res%sca%f64 = dot_product(real(left%array%f32, 8), right%array%f64)

		case (magic * f64_type + f32_type)
			res%type = f64_type
			res%sca%f64 = dot_product(left%array%f64, real(right%array%f32, 8))

		case (magic * i32_type + f32_type)
			res%type = f32_type
			res%sca%f32 = dot_product(real(left%array%i32), right%array%f32)

		case (magic * f32_type + i32_type)
			res%type = f32_type
			res%sca%f32 = dot_product(left%array%f32, real(right%array%i32))

		case (magic * i32_type + f64_type)
			res%type = f64_type
			res%sca%f64 = dot_product(real(left%array%i32, 8), right%array%f64)

		case (magic * f64_type + i32_type)
			res%type = f64_type
			res%sca%f64 = dot_product(left%array%f64, real(right%array%i32, 8))

		case (magic * i64_type + f32_type)
			res%type = f32_type
			res%sca%f32 = dot_product(real(left%array%i64, 4), right%array%f32)

		case (magic * f32_type + i64_type)
			res%type = f32_type
			res%sca%f32 = dot_product(left%array%f32, real(right%array%i64, 4))

		case (magic * i64_type + f64_type)
			res%type = f64_type
			res%sca%f64 = dot_product(real(left%array%i64, 8), right%array%f64)

		case (magic * f64_type + i64_type)
			res%type = f64_type
			res%sca%f64 = dot_product(left%array%f64, real(right%array%i64, 8))

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

		return
	end if

	! ---- matrix/vector cases (at least one operand is rank-2) ------------

	! Determine inner dimension sizes for compatibility check and output shape.
	!
	! For rank-2 arrays: size(1) = num rows, size(2) = num cols (column-major).
	! For rank-1 arrays (treated as a column-vector for M@V or a row-vector for
	! V@M): the single dimension plays the role of the contracting index.

	if (lrank == 2) then
		lr = left%array%size(1)
		lc = left%array%size(2)
	else
		! rank-1: treat as 1 x N row vector for V @ M
		lr = 1_8
		lc = left%array%size(1)
	end if

	if (rrank == 2) then
		rr = right%array%size(1)
		rc = right%array%size(2)
	else
		! rank-1: treat as N x 1 column vector for M @ V
		rr = right%array%size(1)
		rc = 1_8
	end if

	! Inner dimensions must match
	if (lc /= rr) then
		write(*,*) err_matmul_dim(lc, rr)
		call internal_error()
	end if

	! Allocate result array
	allocate(res%array)
	res%type = array_type
	res%array%kind = expl_array

	if (lrank == 2 .and. rrank == 2) then
		! Matrix @ matrix -> matrix (rank 2)
		res%array%rank = 2
		allocate(res%array%size(2))
		res%array%size(1) = lr
		res%array%size(2) = rc
	else
		! Matrix @ vector or vector @ matrix -> vector (rank 1)
		res%array%rank = 1
		allocate(res%array%size(1))
		! M @ V: output length = lr (rows of M); V @ M: output length = rc (cols of M)
		if (lrank == 1) then
			res%array%size(1) = rc
		else
			res%array%size(1) = lr
		end if
	end if

	res%array%len_ = product(res%array%size)
	out_len = res%array%len_

	select case (magic * left%array%type + right%array%type)

	! ---- i32 @ i32 -------------------------------------------------------
	case (magic * i32_type + i32_type)
		res%array%type = i32_type
		res%array%cap  = out_len
		allocate(res%array%i32(out_len))
		call do_matmul_i32(left, right, res, lrank, rrank, lr, lc, rc)

	! ---- i64 @ i64 -------------------------------------------------------
	case (magic * i64_type + i64_type)
		res%array%type = i64_type
		res%array%cap  = out_len
		allocate(res%array%i64(out_len))
		call do_matmul_i64(left, right, res, lrank, rrank, lr, lc, rc)

	! ---- f32 @ f32 -------------------------------------------------------
	case (magic * f32_type + f32_type)
		res%array%type = f32_type
		res%array%cap  = out_len
		allocate(res%array%f32(out_len))
		call do_matmul_f32(left, right, res, lrank, rrank, lr, lc, rc)

	! ---- f64 @ f64 -------------------------------------------------------
	case (magic * f64_type + f64_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64(left, right, res, lrank, rrank, lr, lc, rc)

	! ---- mixed integer ---------------------------------------------------
	case (magic * i32_type + i64_type)
		res%array%type = i64_type
		res%array%cap  = out_len
		allocate(res%array%i64(out_len))
		call do_matmul_i64_from_i32_i64(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * i64_type + i32_type)
		res%array%type = i64_type
		res%array%cap  = out_len
		allocate(res%array%i64(out_len))
		call do_matmul_i64_from_i64_i32(left, right, res, lrank, rrank, lr, lc, rc)

	! ---- mixed float -----------------------------------------------------
	case (magic * f32_type + f64_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64_from_f32_f64(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * f64_type + f32_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64_from_f64_f32(left, right, res, lrank, rrank, lr, lc, rc)

	! ---- int + float promotions -> float ---------------------------------
	case (magic * i32_type + f32_type)
		res%array%type = f32_type
		res%array%cap  = out_len
		allocate(res%array%f32(out_len))
		call do_matmul_f32_from_i32_f32(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * f32_type + i32_type)
		res%array%type = f32_type
		res%array%cap  = out_len
		allocate(res%array%f32(out_len))
		call do_matmul_f32_from_f32_i32(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * i32_type + f64_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64_from_i32_f64(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * f64_type + i32_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64_from_f64_i32(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * i64_type + f32_type)
		res%array%type = f32_type
		res%array%cap  = out_len
		allocate(res%array%f32(out_len))
		call do_matmul_f32_from_i64_f32(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * f32_type + i64_type)
		res%array%type = f32_type
		res%array%cap  = out_len
		allocate(res%array%f32(out_len))
		call do_matmul_f32_from_f32_i64(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * i64_type + f64_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64_from_i64_f64(left, right, res, lrank, rrank, lr, lc, rc)

	case (magic * f64_type + i64_type)
		res%array%type = f64_type
		res%array%cap  = out_len
		allocate(res%array%f64(out_len))
		call do_matmul_f64_from_f64_i64(left, right, res, lrank, rrank, lr, lc, rc)

	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine matmul_value_t

!===============================================================================
!
! Internal helpers: typed matmul kernels.
!
! Convention:
!   lrank, rrank : ranks of left and right operands (1 or 2)
!   lr, lc       : left  operand row/col counts (lc = contracting dim)
!   rc           : right operand col count (output cols, or 1 for rank-1 right)
!
! For rank-1 operands the flat buffer is used as a 1-D Fortran array directly.
! For rank-2 operands we reshape the flat buffer to (lr, lc) / (lc, rc) in
! column-major order (consistent with the subscript_i32_eval indexing).
!
! The result is stored flat in res%array%<type> (buffer already allocated).
!
!===============================================================================

subroutine do_matmul_i32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in)    :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in)          :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc

	integer(kind = 4), allocatable :: C(:)

	if (lrank == 2 .and. rrank == 2) then
		C = reshape(matmul( &
			reshape(left %array%i32, [int(lr), int(lc)]), &
			reshape(right%array%i32, [int(lc), int(rc)])), &
			[int(lr * rc)])
	else if (lrank == 2) then
		! M @ v: right is rank-1 (treated as column vector)
		C = matmul( &
			reshape(left%array%i32, [int(lr), int(lc)]), &
			right%array%i32)
	else
		! v @ M: left is rank-1 (treated as row vector)
		C = matmul( &
			left%array%i32, &
			reshape(right%array%i32, [int(lc), int(rc)]))
	end if
	res%array%i32 = C

end subroutine do_matmul_i32

!===============================================================================

subroutine do_matmul_i64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in)    :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in)          :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc

	integer(kind = 8), allocatable :: C(:)

	if (lrank == 2 .and. rrank == 2) then
		C = reshape(matmul( &
			reshape(left %array%i64, [int(lr), int(lc)]), &
			reshape(right%array%i64, [int(lc), int(rc)])), &
			[int(lr * rc)])
	else if (lrank == 2) then
		C = matmul( &
			reshape(left%array%i64, [int(lr), int(lc)]), &
			right%array%i64)
	else
		C = matmul( &
			left%array%i64, &
			reshape(right%array%i64, [int(lc), int(rc)]))
	end if
	res%array%i64 = C

end subroutine do_matmul_i64

!===============================================================================

subroutine do_matmul_f32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in)    :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in)          :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc

	real(kind = 4), allocatable :: C(:)

	if (lrank == 2 .and. rrank == 2) then
		C = reshape(matmul( &
			reshape(left %array%f32, [int(lr), int(lc)]), &
			reshape(right%array%f32, [int(lc), int(rc)])), &
			[int(lr * rc)])
	else if (lrank == 2) then
		C = matmul( &
			reshape(left%array%f32, [int(lr), int(lc)]), &
			right%array%f32)
	else
		C = matmul( &
			left%array%f32, &
			reshape(right%array%f32, [int(lc), int(rc)]))
	end if
	res%array%f32 = C

end subroutine do_matmul_f32

!===============================================================================

subroutine do_matmul_f64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in)    :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in)          :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc

	real(kind = 8), allocatable :: C(:)

	if (lrank == 2 .and. rrank == 2) then
		C = reshape(matmul( &
			reshape(left %array%f64, [int(lr), int(lc)]), &
			reshape(right%array%f64, [int(lc), int(rc)])), &
			[int(lr * rc)])
	else if (lrank == 2) then
		C = matmul( &
			reshape(left%array%f64, [int(lr), int(lc)]), &
			right%array%f64)
	else
		C = matmul( &
			left%array%f64, &
			reshape(right%array%f64, [int(lc), int(rc)]))
	end if
	res%array%f64 = C

end subroutine do_matmul_f64

!===============================================================================
! Mixed-type helpers: cast to the promoted type before calling matmul
!===============================================================================

subroutine do_matmul_i64_from_i32_i64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(left%array, i64_type)
	allocate(tmp%array%i64(size(left%array%i32)))
	tmp%array%i64 = int(left%array%i32, 8)
	call do_matmul_i64(tmp, right, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_i64_from_i32_i64

subroutine do_matmul_i64_from_i64_i32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(right%array, i64_type)
	allocate(tmp%array%i64(size(right%array%i32)))
	tmp%array%i64 = int(right%array%i32, 8)
	call do_matmul_i64(left, tmp, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_i64_from_i64_i32

subroutine do_matmul_f64_from_f32_f64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(left%array, f64_type)
	allocate(tmp%array%f64(size(left%array%f32)))
	tmp%array%f64 = real(left%array%f32, 8)
	call do_matmul_f64(tmp, right, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f64_from_f32_f64

subroutine do_matmul_f64_from_f64_f32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(right%array, f64_type)
	allocate(tmp%array%f64(size(right%array%f32)))
	tmp%array%f64 = real(right%array%f32, 8)
	call do_matmul_f64(left, tmp, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f64_from_f64_f32

subroutine do_matmul_f32_from_i32_f32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(left%array, f32_type)
	allocate(tmp%array%f32(size(left%array%i32)))
	tmp%array%f32 = real(left%array%i32)
	call do_matmul_f32(tmp, right, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f32_from_i32_f32

subroutine do_matmul_f32_from_f32_i32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(right%array, f32_type)
	allocate(tmp%array%f32(size(right%array%i32)))
	tmp%array%f32 = real(right%array%i32)
	call do_matmul_f32(left, tmp, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f32_from_f32_i32

subroutine do_matmul_f64_from_i32_f64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(left%array, f64_type)
	allocate(tmp%array%f64(size(left%array%i32)))
	tmp%array%f64 = real(left%array%i32, 8)
	call do_matmul_f64(tmp, right, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f64_from_i32_f64

subroutine do_matmul_f64_from_f64_i32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(right%array, f64_type)
	allocate(tmp%array%f64(size(right%array%i32)))
	tmp%array%f64 = real(right%array%i32, 8)
	call do_matmul_f64(left, tmp, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f64_from_f64_i32

subroutine do_matmul_f32_from_i64_f32(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(left%array, f32_type)
	allocate(tmp%array%f32(size(left%array%i64)))
	tmp%array%f32 = real(left%array%i64, 4)
	call do_matmul_f32(tmp, right, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f32_from_i64_f32

subroutine do_matmul_f32_from_f32_i64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(right%array, f32_type)
	allocate(tmp%array%f32(size(right%array%i64)))
	tmp%array%f32 = real(right%array%i64, 4)
	call do_matmul_f32(left, tmp, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f32_from_f32_i64

subroutine do_matmul_f64_from_i64_f64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(left%array, f64_type)
	allocate(tmp%array%f64(size(left%array%i64)))
	tmp%array%f64 = real(left%array%i64, 8)
	call do_matmul_f64(tmp, right, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f64_from_i64_f64

subroutine do_matmul_f64_from_f64_i64(left, right, res, lrank, rrank, lr, lc, rc)
	type(value_t), intent(in) :: left, right
	type(value_t), intent(inout) :: res
	integer, intent(in) :: lrank, rrank
	integer(kind = 8), intent(in) :: lr, lc, rc
	type(value_t) :: tmp
	allocate(tmp%array)
	tmp%array = mold(right%array, f64_type)
	allocate(tmp%array%f64(size(right%array%i64)))
	tmp%array%f64 = real(right%array%i64, 8)
	call do_matmul_f64(left, tmp, res, lrank, rrank, lr, lc, rc)
end subroutine do_matmul_f64_from_f64_i64

!===============================================================================

end module syntran__math_matmul_m

!===============================================================================
