
!===============================================================================

module syntran__linalg_m

	! Dense linear algebra kernels: matrix product (gemm), linear solve (gesv),
	! and LU factorization (getrf) with a solve from its factors (getrs), for
	! f32 and f64.
	!
	! By default these call BLAS/LAPACK (sgemm/dgemm, sgesv/dgesv, sgetrf/dgetrf,
	! sgetrs/dgetrs).  Define
	! SYNTRAN_NO_BLAS to drop the external dependency and use the portable
	! Fortran fallbacks below.  That's -DSYNTRAN_NO_BLAS=ON for cmake, and the
	! `noblas` feature for fpm (`--features release,noblas`, see [features] in
	! fpm.toml), which also stops fpm from linking BLAS/LAPACK
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

		subroutine sgetrf(m, n, a, lda, ipiv, info)
			integer, intent(in) :: m, n, lda
			integer, intent(out) :: ipiv(*), info
			real(kind = 4), intent(inout) :: a(lda, *)
		end subroutine sgetrf

		subroutine dgetrf(m, n, a, lda, ipiv, info)
			integer, intent(in) :: m, n, lda
			integer, intent(out) :: ipiv(*), info
			real(kind = 8), intent(inout) :: a(lda, *)
		end subroutine dgetrf

		subroutine sgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
			character, intent(in) :: trans
			integer, intent(in) :: n, nrhs, lda, ldb, ipiv(*)
			integer, intent(out) :: info
			real(kind = 4), intent(in) :: a(lda, *)
			real(kind = 4), intent(inout) :: b(ldb, *)
		end subroutine sgetrs

		subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
			character, intent(in) :: trans
			integer, intent(in) :: n, nrhs, lda, ldb, ipiv(*)
			integer, intent(out) :: info
			real(kind = 8), intent(in) :: a(lda, *)
			real(kind = 8), intent(inout) :: b(ldb, *)
		end subroutine dgetrs
	end interface

	private :: sgemm, dgemm, sgesv, dgesv, sgetrf, dgetrf, sgetrs, dgetrs
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
	integer, allocatable :: ipiv(:)

	info = 0
	if (n <= 0 .or. nrhs <= 0) return

	w = a
	allocate(ipiv(n))

#ifdef SYNTRAN_NO_BLAS
	call lu_factor_f32(n, n, w, ipiv, info)
	if (info == 0) call lu_subst_f32(n, nrhs, w, ipiv, b)
#else
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
	integer, allocatable :: ipiv(:)

	info = 0
	if (n <= 0 .or. nrhs <= 0) return

	w = a
	allocate(ipiv(n))

#ifdef SYNTRAN_NO_BLAS
	call lu_factor_f64(n, n, w, ipiv, info)
	if (info == 0) call lu_subst_f64(n, nrhs, w, ipiv, b)
#else
	call dgesv(n, nrhs, w, n, ipiv, b, n, info)
#endif

end subroutine gesv_f64

!===============================================================================

subroutine getrf_f32(m, n, a, ipiv, info)

	! LU factorization with partial pivoting of an m x n matrix, in place:
	! a = P * L * U.  L is unit lower trapezoidal (below the diagonal) and U is
	! upper trapezoidal, both stored in `a`.  `ipiv` has min(m,n) entries and is
	! 1-based like LAPACK: row i was swapped with row ipiv(i).
	!
	! info > 0 means that u(info,info) is exactly zero.  The factorization is
	! still completed, like LAPACK

	integer, intent(in) :: m, n
	real(kind = 4), intent(inout) :: a(m, n)
	integer, intent(out) :: ipiv(min(m, n))
	integer, intent(out) :: info

	!********

	info = 0
	if (m <= 0 .or. n <= 0) return

#ifdef SYNTRAN_NO_BLAS
	call lu_factor_f32(m, n, a, ipiv, info)
#else
	call sgetrf(m, n, a, m, ipiv, info)
#endif

end subroutine getrf_f32

!===============================================================================

subroutine getrf_f64(m, n, a, ipiv, info)

	! See getrf_f32

	integer, intent(in) :: m, n
	real(kind = 8), intent(inout) :: a(m, n)
	integer, intent(out) :: ipiv(min(m, n))
	integer, intent(out) :: info

	!********

	info = 0
	if (m <= 0 .or. n <= 0) return

#ifdef SYNTRAN_NO_BLAS
	call lu_factor_f64(m, n, a, ipiv, info)
#else
	call dgetrf(m, n, a, m, ipiv, info)
#endif

end subroutine getrf_f64

!===============================================================================

subroutine getrs_f32(n, nrhs, a, ipiv, b)

	! Solve a * x = b using the n x n factors and 1-based pivots from
	! getrf_f32().  `b` is overwritten with `x`.  The caller must check that `a`
	! has no zero diagonal and that `ipiv` is in range

	integer, intent(in) :: n, nrhs
	real(kind = 4), intent(in)    :: a(n, n)
	integer, intent(in) :: ipiv(n)
	real(kind = 4), intent(inout) :: b(n, nrhs)

	!********

#ifndef SYNTRAN_NO_BLAS
	integer :: info
#endif

	if (n <= 0 .or. nrhs <= 0) return

#ifdef SYNTRAN_NO_BLAS
	call lu_subst_f32(n, nrhs, a, ipiv, b)
#else
	call sgetrs('N', n, nrhs, a, n, ipiv, b, n, info)
#endif

end subroutine getrs_f32

!===============================================================================

subroutine getrs_f64(n, nrhs, a, ipiv, b)

	! See getrs_f32

	integer, intent(in) :: n, nrhs
	real(kind = 8), intent(in)    :: a(n, n)
	integer, intent(in) :: ipiv(n)
	real(kind = 8), intent(inout) :: b(n, nrhs)

	!********

#ifndef SYNTRAN_NO_BLAS
	integer :: info
#endif

	if (n <= 0 .or. nrhs <= 0) return

#ifdef SYNTRAN_NO_BLAS
	call lu_subst_f64(n, nrhs, a, ipiv, b)
#else
	call dgetrs('N', n, nrhs, a, n, ipiv, b, n, info)
#endif

end subroutine getrs_f64

!===============================================================================
!
! Portable fallbacks.  These are only called when SYNTRAN_NO_BLAS is defined,
! but they're always compiled so that both configurations type-check
!
!===============================================================================

subroutine lu_factor_f32(m, n, a, ipiv, info)

	! Gaussian elimination with partial pivoting of an m x n matrix, in place,
	! like LAPACK getf2.  `ipiv` is 1-based.  On an exactly zero pivot, info is
	! set to the first such column but the elimination continues, so that the
	! factors and info match LAPACK's

	integer, intent(in) :: m, n
	real(kind = 4), intent(inout) :: a(m, n)
	integer, intent(out) :: ipiv(min(m, n))
	integer, intent(out) :: info

	!********

	integer :: j, k, p
	real(kind = 4) :: rowtmp(n)

	info = 0

	do j = 1, min(m, n)

		! Pivot: largest magnitude entry in column j at or below the diagonal
		p = j - 1 + maxloc(abs(a(j: m, j)), 1)
		ipiv(j) = p

		if (a(p, j) == 0) then
			if (info == 0) info = j
			cycle
		end if

		if (p /= j) then
			rowtmp = a(j, :) ; a(j, :) = a(p, :) ; a(p, :) = rowtmp
		end if

		a(j+1: m, j) = a(j+1: m, j) / a(j, j)
		do k = j + 1, n
			a(j+1: m, k) = a(j+1: m, k) - a(j+1: m, j) * a(j, k)
		end do

	end do

end subroutine lu_factor_f32

!===============================================================================

subroutine lu_subst_f32(n, nrhs, a, ipiv, b)

	! Solve using the n x n factors from lu_factor_f32().  `b` is overwritten
	! with the solution

	integer, intent(in) :: n, nrhs
	real(kind = 4), intent(in) :: a(n, n)
	integer, intent(in) :: ipiv(n)
	real(kind = 4), intent(inout) :: b(n, nrhs)

	!********

	integer :: i, j, k, p
	real(kind = 4) :: btmp(nrhs)

	! Apply the row swaps in order
	do i = 1, n
		p = ipiv(i)
		if (p /= i) then
			btmp = b(i, :) ; b(i, :) = b(p, :) ; b(p, :) = btmp
		end if
	end do

	do k = 1, nrhs

		! Forward substitution with unit lower L, column-oriented
		do j = 1, n - 1
			b(j+1: n, k) = b(j+1: n, k) - b(j, k) * a(j+1: n, j)
		end do

		! Back substitution with U
		do j = n, 1, -1
			b(j, k) = b(j, k) / a(j, j)
			b(1: j-1, k) = b(1: j-1, k) - b(j, k) * a(1: j-1, j)
		end do

	end do

end subroutine lu_subst_f32

!===============================================================================

subroutine lu_factor_f64(m, n, a, ipiv, info)

	! Gaussian elimination with partial pivoting of an m x n matrix, in place,
	! like LAPACK getf2.  `ipiv` is 1-based.  On an exactly zero pivot, info is
	! set to the first such column but the elimination continues, so that the
	! factors and info match LAPACK's

	integer, intent(in) :: m, n
	real(kind = 8), intent(inout) :: a(m, n)
	integer, intent(out) :: ipiv(min(m, n))
	integer, intent(out) :: info

	!********

	integer :: j, k, p
	real(kind = 8) :: rowtmp(n)

	info = 0

	do j = 1, min(m, n)

		! Pivot: largest magnitude entry in column j at or below the diagonal
		p = j - 1 + maxloc(abs(a(j: m, j)), 1)
		ipiv(j) = p

		if (a(p, j) == 0) then
			if (info == 0) info = j
			cycle
		end if

		if (p /= j) then
			rowtmp = a(j, :) ; a(j, :) = a(p, :) ; a(p, :) = rowtmp
		end if

		a(j+1: m, j) = a(j+1: m, j) / a(j, j)
		do k = j + 1, n
			a(j+1: m, k) = a(j+1: m, k) - a(j+1: m, j) * a(j, k)
		end do

	end do

end subroutine lu_factor_f64

!===============================================================================

subroutine lu_subst_f64(n, nrhs, a, ipiv, b)

	! Solve using the n x n factors from lu_factor_f64().  `b` is overwritten
	! with the solution

	integer, intent(in) :: n, nrhs
	real(kind = 8), intent(in) :: a(n, n)
	integer, intent(in) :: ipiv(n)
	real(kind = 8), intent(inout) :: b(n, nrhs)

	!********

	integer :: i, j, k, p
	real(kind = 8) :: btmp(nrhs)

	! Apply the row swaps in order
	do i = 1, n
		p = ipiv(i)
		if (p /= i) then
			btmp = b(i, :) ; b(i, :) = b(p, :) ; b(p, :) = btmp
		end if
	end do

	do k = 1, nrhs

		! Forward substitution with unit lower L, column-oriented
		do j = 1, n - 1
			b(j+1: n, k) = b(j+1: n, k) - b(j, k) * a(j+1: n, j)
		end do

		! Back substitution with U
		do j = n, 1, -1
			b(j, k) = b(j, k) / a(j, j)
			b(1: j-1, k) = b(1: j-1, k) - b(j, k) * a(1: j-1, j)
		end do

	end do

end subroutine lu_subst_f64

!===============================================================================

end module syntran__linalg_m

!===============================================================================
