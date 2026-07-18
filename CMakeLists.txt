
cmake_minimum_required(VERSION 3.13)

# TODO: use FC env var or -D cmake arg to define this consistently on all
# platforms
if(DEFINED ENV{SYNTRAN_INTEL})

	# Honor whatever Intel compiler the caller has already selected (e.g. via
	# setup-fortran in CI, which exports FC=ifx or FC=ifort), falling back to
	# ifx if unset.
	if(DEFINED ENV{FC})
		set(CMAKE_Fortran_COMPILER "$ENV{FC}")
	else()
		set(CMAKE_Fortran_COMPILER "ifx")
	endif()

#elseif(NOT APPLE)
elseif(NOT DEFINED CMAKE_Fortran_COMPILER)

	set(CMAKE_Fortran_COMPILER "gfortran")

endif()

set(PROJECT syntran)

project(${PROJECT} Fortran C)

if(WIN32 OR CMAKE_GENERATOR MATCHES "MSYS" OR CMAKE_GENERATOR MATCHES "MinGW")
	message("defining _WIN32 in cmake")
	add_compile_definitions(_WIN32)
else()
	message("NOT defining _WIN32 in cmake")
endif()

if(DEFINED ENV{SYNTRAN_INTEL})
	# ifort-specific flags must NOT reach the C compiler (icx doesn't
	# understand -fpp/etc), so gate them to Fortran only.
	add_compile_options(
		"$<$<COMPILE_LANGUAGE:Fortran>:-W1>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-qopenmp>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-fPIC>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-fpp>"
	)
	add_compile_definitions(SYNTRAN_INTEL)
else()

	# gfortran-specific dialect/warning flags must NOT reach the C compiler
	# (AppleClang rejects -cpp / -fopenmp), so gate them to Fortran only.
	add_compile_options(
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wall>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wextra>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-fopenmp>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-cpp>"

		# Why would you warn about tabs in free-form Fortran?
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wno-tabs>"

		# More false alarms than anything
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wno-maybe-uninitialized>"

		# I need to compare reals because syntran users need to be able to
		# compare reals if they want to
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wno-compare-reals>"

		# Suppress preprocessor warnings about apostrophes in comments (e.g. "There's")
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wno-cpp>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-Wp,-w>"
	)

endif()
#add_compile_options("-J ${CMAKE_CURRENT_SOURCE_DIR}/build/")

message("")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	message("Debug")
	# gfortran-specific debug flags must NOT reach the C compiler
	add_compile_options(
		"$<$<COMPILE_LANGUAGE:Fortran>:-pedantic>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-fbounds-check>"
		"$<$<COMPILE_LANGUAGE:Fortran>:-fbacktrace>"
	)
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
	message("Release")
	if(DEFINED ENV{SYNTRAN_INTEL})
		add_compile_options(
			"$<$<COMPILE_LANGUAGE:Fortran>:-Ofast>"
		)
		add_compile_options("$<$<COMPILE_LANGUAGE:C>:-O2>")
	else()
		add_compile_options(-O3)
	endif()
else()
	message("Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()
message("CMAKE_Fortran_COMPILER = ${CMAKE_Fortran_COMPILER}")
message("")

if(APPLE)
	add_link_options(-openmp)
elseif(DEFINED ENV{SYNTRAN_INTEL})
	# No -qmkl: nothing in this codebase calls into BLAS/LAPACK (grep finds
	# only a speculative TODO comment), and CI's Intel install doesn't
	# provide the MKL libraries -- linking it just fails with "cannot find
	# -lmkl_intel_lp64" et al.
	add_link_options(-fopenmp)
	if(CMAKE_BUILD_TYPE STREQUAL "Release")
		# Drives whole-program IPO at link time (invokes xild internally);
		# matches the -ipo compile flags added above and the xiar archiver
		# set for the static lib.
		add_link_options(-ipo)
	endif()
else()
	add_link_options(-fopenmp)
endif()
if(WIN32)
	# On Windows/MinGW a FULL `-static` link is REQUIRED: the dynamically-linked
	# runtime triggers an intermittent heap-corruption SIGSEGV (~1-3% of runs) in
	# syntran's nested-allocatable parse paths, and only a full static link
	# (libgfortran + libgcc/SEH-unwind + libwinpthread + libquadmath) reliably
	# eliminates it (0 crashes in ~975 runs vs ~1-3% dynamic; -static-libgfortran
	# alone still crashes).  It also makes the binary self-contained.  (The old
	# `-static-libquadmath -static-libgfortran -static-libgcc` also failed to link
	# at all under MinGW gfortran 12.2 -- `-static-libquadmath` is unrecognized.)
	add_link_options(-static)
else()
	# macOS/Linux do NOT have the crash and do NOT support a full `-static` link
	# (Apple ld has no crt0.o for static linking).  Keep the partial static
	# runtime link for a more self-contained binary, as before.
	add_link_options(-static-libquadmath -static-libgfortran -static-libgcc)
endif()

set(SRC_DIR "src")

#===============================================================================

# Core library, used by main interpreter and unit tests
set(LIB "${PROJECT}")
set(LIB_SRC
	${SRC_DIR}/c/isocline_wrap.c
	# Compiled here (once), instead of duplicated into each of
	# EXE_SRC/TEST_SRC/LONG_SRC below, to avoid recompiling it 3x per config.
	${SRC_DIR}/app.f90
	${SRC_DIR}/bool.f90
	${SRC_DIR}/bytecode.f90
	${SRC_DIR}/compiler.F90
	${SRC_DIR}/compile.f90
	${SRC_DIR}/compile_ctrl.f90
	${SRC_DIR}/consts.f90
	${SRC_DIR}/core.f90
	${SRC_DIR}/errors.f90
	${SRC_DIR}/eval.f90
	${SRC_DIR}/eval_array.f90
	${SRC_DIR}/eval_control.f90
	${SRC_DIR}/eval_expr.f90
	${SRC_DIR}/eval_fn.f90
	${SRC_DIR}/vm.f90
	${SRC_DIR}/vm_exec.f90
	${SRC_DIR}/vm_intr.f90
	${SRC_DIR}/intr_fns.f90
	${SRC_DIR}/intr_fns_array.f90
	${SRC_DIR}/intr_fns_io.f90
	${SRC_DIR}/intr_fns_math.f90
	${SRC_DIR}/intr_fns_minmax.f90
	${SRC_DIR}/intr_fns_trig.f90
	${SRC_DIR}/intr_vars.f90
	${SRC_DIR}/lex.f90
	${SRC_DIR}/line_edit.f90
	${SRC_DIR}/math.f90
	${SRC_DIR}/math_bin_add.f90
	${SRC_DIR}/math_bin_div.f90
	${SRC_DIR}/math_bin_matmul.f90
	${SRC_DIR}/math_bin_mul.f90
	${SRC_DIR}/math_bin_pow.f90
	${SRC_DIR}/math_bin_subtract.f90
	${SRC_DIR}/math_bit_and.f90
	${SRC_DIR}/math_bit_left_shift.f90
	${SRC_DIR}/math_bit_not.f90
	${SRC_DIR}/math_bit_or.f90
	${SRC_DIR}/math_bit_right_shift.f90
	${SRC_DIR}/math_bit_xor.f90
	${SRC_DIR}/parse.f90
	${SRC_DIR}/parse_array.f90
	${SRC_DIR}/parse_control.f90
	${SRC_DIR}/parse_expr.f90
	${SRC_DIR}/parse_fn.f90
	${SRC_DIR}/parse_misc.f90
	${SRC_DIR}/syntran.f90
	${SRC_DIR}/types.f90
	${SRC_DIR}/types_copy.f90
	${SRC_DIR}/types_dict.f90
	${SRC_DIR}/types_node.f90
	${SRC_DIR}/types_ops.f90
	${SRC_DIR}/utils.f90
	${SRC_DIR}/value.f90
)

if(NOT DEFINED ENV{SYNTRAN_INTEL})
	# app.f90's `use ifport` (for isatty()) is Intel-only; under Intel it
	# resolves to the compiler's own built-in IFPORT module and produces no
	# local .mod.  ifport_shim.f90 provides an empty local module of the same
	# name so fpm's (and, without this exclusion, cmake's) source-text-based
	# Fortran module scanner -- which isn't preprocessor-aware and can't tell
	# the `use` is inactive here -- has a real producer to satisfy.  Under
	# Intel, cmake must NOT compile this file at all: if it did, the scanner
	# would expect a real "ifport.mod" build artifact that Intel never
	# actually writes (it comes from the compiler's own module path instead),
	# and cmake's copy-module-between-targets step would fail trying to find
	# it.  fpm has no per-compiler source exclusion, so it always compiles
	# this file; ifport_shim.f90's own internal `#if defined(__GFORTRAN__)`
	# guard keeps that safe under fpm+Intel too.
	list(APPEND LIB_SRC ${SRC_DIR}/ifport_shim.f90)
endif()

if(DEFINED ENV{SYNTRAN_INTEL})
	# -ipo (added to Release flags below) emits LLVM-IR objects instead of
	# native object code.  Archiving those into a STATIC lib needs a symbol
	# index built by an LTO-aware archiver; xiar (set as CMAKE_AR/RANLIB) is
	# supposed to handle that, but in practice its gold-plugin bridge fails
	# on Intel 2025.2's bitcode ("LLVM gold plugin has failed to create LTO
	# module: Unknown attribute kind"), silently producing an archive with a
	# broken symbol index -- the final link then can't find symbols that are
	# genuinely present in it (mass "undefined reference" errors).  An OBJECT
	# library sidesteps archiving entirely: the final link gets the raw .o
	# files directly, which need no symbol index at all.  (This is also why
	# `-ipo` doesn't work under fpm at all -- see commit 355efcea3 -- fpm
	# always archives with the default ar/ld.)
	add_library(${LIB} OBJECT ${LIB_SRC})
else()
	#add_library(${LIB} SHARED ${LIB_SRC}) # dll
	add_library(${LIB} STATIC ${LIB_SRC}) # static lib
endif()

set(LINK_LIBS "${LIB}")

#===============================================================================

# Main interpreter exe
set(EXE "${PROJECT}")
set(EXE_BIN "${EXE}-bin")
set(EXE_SRC
	${SRC_DIR}/main.f90
)

add_executable("${EXE_BIN}"
	${EXE_SRC}
)
target_link_libraries(${EXE_BIN} ${LINK_LIBS})

# Explicitly set module output dir per target because parallel cmake will bug
# out if multiple threads are reading/writing to the same *.mod file in the same
# dir
set_target_properties("${EXE_BIN}" PROPERTIES
	Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/${EXE_BIN}
)
# TODO: need post-build command or something to copy mods where nvim linter
# expects them

# Workaround exe and lib w/ same name:
#
#     https://gist.github.com/jlgerber/eafc4ee2b9954e27dd2bb009496b1b03
#
set_target_properties("${EXE_BIN}" PROPERTIES OUTPUT_NAME "${EXE}")

#===============================================================================

# Unit test exe
set(TEST_EXE "test")
set(TEST_SRC
	${SRC_DIR}/tests/core.f90
	${SRC_DIR}/tests/test.f90
)

add_executable(${TEST_EXE}
	${TEST_SRC}
)
target_link_libraries(${TEST_EXE} ${LINK_LIBS})

set_target_properties("${TEST_EXE}" PROPERTIES
	Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/${TEST_EXE}
)

# Thanks gfort, I'm aware of integer division
target_compile_options(${TEST_EXE} PRIVATE "-Wno-integer-division")

#===============================================================================

# Long test exe
set(LONG_EXE "long")
set(LONG_SRC
	${SRC_DIR}/tests/core.f90
	${SRC_DIR}/tests/long.f90
)

add_executable(${LONG_EXE}
	${LONG_SRC}
)
target_link_libraries(${LONG_EXE} ${LINK_LIBS})

set_target_properties("${LONG_EXE}" PROPERTIES
	Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod/${LONG_EXE}
)

# Thanks gfort, I'm aware of integer division
target_compile_options(${LONG_EXE} PRIVATE "-Wno-integer-division")

#===============================================================================

# This tells cmake to use the fortran compiler as the linker.  See:
#
#     https://stackoverflow.com/questions/40767212/use-fortran-linker-in-cmake-mixed-language-project
#
set_property(TARGET ${EXE_BIN} PROPERTY LINKER_LANGUAGE Fortran)

