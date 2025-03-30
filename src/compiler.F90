
!===============================================================================

module syntran__compiler_m

	implicit none

	! The uppercase file extension .F90 automatically enables compiler
	! pre-processing

#if defined(__GFORTRAN__)

	character(len = *), parameter :: fort_compiler = "gfortran"
	integer, parameter :: fort_vers(*) = [__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__]

#if (__GNUC__ <= 9)
#error syntran is not compatible with gfortran <= 9. upgrade to gfortran 10, 11, or 12
#endif

#if (__GNUC__ >= 15)
#error syntran is not compatible with gfortran >= 15. downgrade to gfortran 10 through 14
! gfortran 15 isn't released yet (as of January 2025).  It might work, but it
! needs to be tested
#endif

#elif defined(__INTEL_COMPILER)

	! Could be either ifx or ifort
	character(len = *), parameter :: fort_compiler = "intel"
	integer, parameter :: fort_vers(*) = [__INTEL_COMPILER]

#else
#error Neither __GFORTRAN__ nor __INTEL__ are defined.  Please use a supported compiler and compile with pre-processing `-cpp` (gfortran) or `-fpp` (intel)

	character(len = *), parameter :: fort_compiler = "unknown"
	integer, parameter :: fort_vers(*) = []

#endif

	! dev commit is replaced with the git hash by gen-header.sh
	character(len = *), parameter :: git_commit = "DEV_COMMIT"
	character(len = *), parameter :: build_date = __DATE__

end module syntran__compiler_m

!===============================================================================

