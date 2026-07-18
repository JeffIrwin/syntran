
!===============================================================================

#if defined(__GFORTRAN__)
module ifport
	! Unfortunately this is the only way i can get fpm to not complain about
	! ifport with gfortran (it auto-discovers and compiles every *.f90 file
	! regardless of --compiler, so this guard is still needed even though
	! cmake excludes this whole file for Intel builds -- see CMakeLists.txt)
	!
	! TODO: try using "external-modules" in fpm.toml, I think this is what it's
	! for
end module ifport
#endif

!===============================================================================
