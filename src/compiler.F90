
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
#if (__GNUC__ >= 13)
#error syntran is not compatible with gfortran >= 13. downgrade to gfortran 10, 11, or 12
#endif

#elif defined(__INTEL_COMPILER)

	! Could be ifx or ifort
	character(len = *), parameter :: fort_compiler = "intel"
	integer, parameter :: fort_vers(*) = [__INTEL_COMPILER]

#else

	character(len = *), parameter :: fort_compiler = "unknown"
	integer, parameter :: fort_vers(*) = []

#error Neither __GFORTRAN__ nor __INTEL__ are defined.  Please use a supported compiler and compile with pre-processing `-cpp` (gfortran) or `-fpp` (intel)

#endif

end module syntran__compiler_m

!===============================================================================

