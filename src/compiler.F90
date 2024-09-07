
!===============================================================================

module syntran__compiler_m

	implicit none

	! The uppercase file extension .F90 automatically enables compiler
	! pre-processing

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#if defined(__GFORTRAN__)
!#if defined(__GFRTRAN__)

#define SY_GFORT_VERS STR(__GNUC__) "." STR(__GNUC_MINOR__) "." STR(__GNUC_PATCHLEVEL__)

	character(len = *), parameter :: fort_compiler = "gfortran"
	integer, parameter :: fort_vers(*) = [__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__]
	!integer, parameter :: fort_major = __GNUC__
	!integer, parameter :: fort_minor = __GNUC_MINOR__
	!integer, parameter :: fort_patch = __GNUC_PATCHLEVEL__
	!character(len = *), parameter :: fort_vers = (SY_GFORT_VERS)

#elif defined(__INTEL_COMPILER)

	! Could be ifx or ifort
	character(len = *), parameter :: fort_compiler = "intel"
	!character(len = *), parameter :: fort_vers = STR(__INTEL_COMPILER)
	integer, parameter :: fort_vers(*) = [__INTEL_COMPILER]
	!integer, parameter :: fort_major = __INTEL_COMPILER
	!integer, parameter :: fort_major = __GNUC_MINOR__
	!integer, parameter :: fort_major = __GNUC_PATCHLEVEL__
	!print *, "intel defined"

#else

	character(len = *), parameter :: fort_compiler = "unknown"
#error "Neither __GFORTRAN__ nor __INTEL__ are defined.  Please use a supported compiler and compile with pre-processing `-cpp` (gfortran) or `-fpp` (intel)"

#endif

end module syntran__compiler_m

!===============================================================================

