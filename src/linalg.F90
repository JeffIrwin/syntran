
!===============================================================================

module syntran__linalg_m

	! Dense linear algebra kernels: matrix product (gemm) and linear solve
	! (gesv) for f32 and f64.
	!
	! By default these call BLAS/LAPACK (sgemm/dgemm/sgesv/dgesv).  Define
	! SYNTRAN_NO_BLAS (cmake: -DSYNTRAN_NO_BLAS=ON, fpm: --flag
	! "-DSYNTRAN_NO_BLAS") to drop the external dependency and use the portable
	! Fortran fallbacks below.
	!
	! The uppercase .F90 extension is deliberate, for the same reason as
	! compiler.F90: fpm doesn't pass -cpp for plain .f90 sources, so an #ifdef
	! anywhere else would silently differ between fpm and cmake.  This is the
	! only file with BLAS #ifdefs.
	!
	! Only BLAS *subroutines* are used.  Accelerate's legacy Fortran ABI returns
	! double for real-valued functions like sdot, which is a trap.
	!
	! All matrices are column-major.  Callers may pass flat rank-1 buffers for
	! the rank-2 dummies (sequence association).  Buffers can be longer than
	! the matrix.

	implicit none

#ifdef SYNTRAN_NO_BLAS
	logical, parameter :: blas_enabled = .false.
#else
	logical, parameter :: blas_enabled = .true.

	interface
		subroutine sgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
			character, intent(in) :: transa, transb
			integer, intent(in) :: m, n, k, lda, ldb, ldc
			real(kind = 4), intent(in) :: alpha, beta
			real(kind = 4), intent(in) :: a(lda, *), b(ldb, *)
			real(kind = 4), intent(inout) :: c(ldc, *)
		end subroutine sgemm

		subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
			character, intent(in) :: transa, transb
			integer, intent(in) :: m, n, k, lda, ldb, ldc
			real(kind = 8), intent(in) :: alpha, beta
			real(kind = 8), intent(in) :: a(lda, *), b(ldb, *)
			real(kind = 8), intent(inout) :: c(ldc, *)
		end subroutine dgemm

		subroutine sgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
			integer, intent(in) :: n, nrhs, lda, ldb
			integer, intent(out) :: ipiv(*), info
			real(kind = 4), intent(inout) :: a(lda, *), b(ldb, *)
		end subroutine sgesv

		subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
			integer, intent(in) :: n, nrhs, lda, ldb
			integer, intent(out) :: ipiv(*), info
			real(kind = 8), intent(inout) :: a(lda, *), b(ldb, *)
		end subroutine dgesv
	end interface

	private :: sgemm, dgemm, sgesv, dgesv
#endif

!===============================================================================

contains

!===============================================================================

subroutine gemm_f32(m, n, k, a, b, c)

	! c(m,n) = a(m,k) * b(k,n)

	integer, intent(in) :: m, n, k
	real(kind = 4), intent(in)  :: a(m, k), b(k, n)
	real(kind = 4), intent(out) :: c(m, n)

	!********

	if (m <= 0 .or. n <= 0) return
	if (k <= 0) then
		c = 0
		return
	end if

#ifdef SYNTRAN_NO_BLAS
	c = matmul(a, b)
#else
	call sgemm('N', 'N', m, n, k, 1.0, a, m, b, k, 0.0, c, m)
#endif

end subroutine gemm_f32

!===============================================================================

subroutine gemm_f64(m, n, k, a, b, c)

	! c(m,n) = a(m,k) * b(k,n)

	integer, intent(in) :: m, n, k
	real(kind = 8), intent(in)  :: a(m, k), b(k, n)
	real(kind = 8), intent(out) :: c(m, n)

	!********

	if (m <= 0 .or. n <= 0) return
	if (k <= 0) then
		c = 0
		return
	end if

#ifdef SYNTRAN_NO_BLAS
	c = matmul(a, b)
#else
	call dgemm('N', 'N', m, n, k, 1.0d0, a, m, b, k, 0.0d0, c, m)
#endif

end subroutine gemm_f64

!===============================================================================

subroutine gesv_f32(n, nrhs, a, b, info)

	! Solve a * x = b.  `a` is not modified.  `b` is overwritten with `x`.
	! info > 0 means that `a` is exactly singular (u(info,info) == 0), like
	! LAPACK

	integer, intent(in) :: n, nrhs
	real(kind = 4), intent(in)    :: a(n, n)
	real(kind = 4), intent(inout) :: b(n, nrhs)
	integer, intent(out) :: info

	!********

	real(kind = 4), allocatable :: w(:,:)
#ifndef SYNTRAN_NO_BLAS
	integer, allocatable :: ipiv(:)
#endif

	info = 0
	if (n <= 0 .or. nrhs <= 0) return

	w = a

#ifdef SYNTRAN_NO_BLAS
	call lu_solve_f32(n, nrhs, w, b, info)
#else
	allocate(ipiv(n))
	call sgesv(n, nrhs, w, n, ipiv, b, n, info)
#endif

end subroutine gesv_f32

!===============================================================================

subroutine gesv_f64(n, nrhs, a, b, info)

	! See gesv_f32

	integer, intent(in) :: n, nrhs
	real(kind = 8), intent(in)    :: a(n, n)
	real(kind = 8), intent(inout) :: b(n, nrhs)
	integer, intent(out) :: info

	!********

	real(kind = 8), allocatable :: w(:,:)
#ifndef SYNTRAN_NO_BLAS
	integer, allocatable :: ipiv(:)
#endif

	info = 0
	if (n <= 0 .or. nrhs <= 0) return

	w = a

#ifdef SYNTRAN_NO_BLAS
	call lu_solve_f64(n, nrhs, w, b, info)
#else
	allocate(ipiv(n))
	call dgesv(n, nrhs, w, n, ipiv, b, n, info)
#endif

end subroutine gesv_f64

!===============================================================================
!
! Portable fallbacks.  These are only called when SYNTRAN_NO_BLAS is defined,
! but they're always compiled so that both configurations type-check
!
!===============================================================================

subroutine lu_solve_f32(n, nrhs, a, b, info)

	! Gaussian elimination with partial pivoting.  `a` is destroyed (it's a
	! working copy), `b` is overwritten with the solution

	integer, intent(in) :: n, nrhs
	real(kind = 4), intent(inout) :: a(n, n), b(n, nrhs)
	integer, intent(out) :: info

	!********

	integer :: i, j, p
	real(kind = 4) :: f
	real(kind = 4) :: rowtmp(n), btmp(nrhs)

	info = 0

	do j = 1, n

		! Pivot: largest magnitude entry in column j at or below the diagonal
		p = j - 1 + maxloc(abs(a(j: n, j)), 1)
		if (a(p, j) == 0) then
			info = j
			return
		end if

		if (p /= j) then
			rowtmp = a(j, :) ; a(j, :) = a(p, :) ; a(p, :) = rowtmp
			btmp   = b(j, :) ; b(j, :) = b(p, :) ; b(p, :) = btmp
		end if

		do i = j + 1, n
			f = a(i, j) / a(j, j)
			if (f == 0) cycle
			a(i, j+1: n) = a(i, j+1: n) - f * a(j, j+1: n)
			b(i, :) = b(i, :) - f * b(j, :)
		end do

	end do

	! Back substitution
	do i = n, 1, -1
		if (i < n) b(i, :) = b(i, :) - matmul(a(i, i+1: n), b(i+1: n, :))
		b(i, :) = b(i, :) / a(i, i)
	end do

end subroutine lu_solve_f32

!===============================================================================

subroutine lu_solve_f64(n, nrhs, a, b, info)

	! See lu_solve_f32

	integer, intent(in) :: n, nrhs
	real(kind = 8), intent(inout) :: a(n, n), b(n, nrhs)
	integer, intent(out) :: info

	!********

	integer :: i, j, p
	real(kind = 8) :: f
	real(kind = 8) :: rowtmp(n), btmp(nrhs)

	info = 0

	do j = 1, n

		p = j - 1 + maxloc(abs(a(j: n, j)), 1)
		if (a(p, j) == 0) then
			info = j
			return
		end if

		if (p /= j) then
			rowtmp = a(j, :) ; a(j, :) = a(p, :) ; a(p, :) = rowtmp
			btmp   = b(j, :) ; b(j, :) = b(p, :) ; b(p, :) = btmp
		end if

		do i = j + 1, n
			f = a(i, j) / a(j, j)
			if (f == 0) cycle
			a(i, j+1: n) = a(i, j+1: n) - f * a(j, j+1: n)
			b(i, :) = b(i, :) - f * b(j, :)
		end do

	end do

	do i = n, 1, -1
		if (i < n) b(i, :) = b(i, :) - matmul(a(i, i+1: n), b(i+1: n, :))
		b(i, :) = b(i, :) / a(i, i)
	end do

end subroutine lu_solve_f64

!===============================================================================

end module syntran__linalg_m

!===============================================================================
