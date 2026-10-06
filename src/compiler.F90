
!===============================================================================

module syntran__compiler_m

	implicit none

	! The uppercase file extension .F90 automatically enables compiler
	! pre-processing

#if defined(__GFORTRAN__)

	character(len = *), parameter :: fort_compiler = "gfortran"
	integer, parameter :: fort_vers(*) = [__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__]

#if (__GNUC__ <= 9)
#error syntran is not compatible with gfortran <= 9. upgrade to gfortran 10 through 15
#endif

#if (__GNUC__ >= 17)
#error syntran is not compatible with gfortran >= 16. downgrade to gfortran 10 through 15
! gfortran 16 isn't released yet (as of November 2025).  It might work, but it
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

	! dev commit is replaced with the git hash by utils/gen-header.sh
	character(len = *), parameter :: git_commit = "DEV_COMMIT"
	character(len = *), parameter :: build_date = __DATE__

	! Runtime subscript/string-index bounds checking (RC_SUBSCRIPT_OOB, R33).
	! Set by -DSYNTRAN_BOUNDS_CHECK (CMake's Debug build defines it
	! automatically; fpm needs `--flag -DSYNTRAN_BOUNDS_CHECK`).  Every check
	! site is a plain `if (bounds_check) then ... end if` in ordinary Fortran
	! -- not further #ifdef'd -- so both configurations always type-check, and
	! the check is dead-code-eliminated when this is .false. (the default,
	! e.g. release builds) instead of costing anything at run time.
	!
	! This can't be an ordinary `#ifdef SYNTRAN_BOUNDS_CHECK`/`#endif` block
	! wrapped around each check site directly: fpm doesn't pass -cpp to
	! gfortran for plain lowercase .f90 sources, so an #ifdef there is just an
	! illegal-directive warning and BOTH branches get compiled, while CMake
	! does pass -cpp/-fpp -- the two build systems would silently disagree on
	! which branch survives. This file's uppercase .F90 extension is the one
	! extension both gfortran and ifx always preprocess regardless of flags,
	! which is why the #ifdef lives here and nowhere else.
#ifdef SYNTRAN_BOUNDS_CHECK
	logical, parameter :: bounds_check = .true.
#else
	logical, parameter :: bounds_check = .false.
#endif

end module syntran__compiler_m

!===============================================================================

