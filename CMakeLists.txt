
cmake_minimum_required(VERSION 3.7)

# TODO: use FC env var or -D cmake arg to define this consistently on all
# platforms
if(DEFINED ENV{SYNTRAN_INTEL})

	set(CMAKE_Fortran_COMPILER "ifort")

elseif(NOT APPLE)

	set(CMAKE_Fortran_COMPILER "gfortran")

endif()

set(PROJECT syntran)

project(${PROJECT} Fortran)

if(WINDOWS)

	# TODO: test on actual Windows or remove if not needed for realpath/
	# fullpath.  Either way, get github actions building.  This branch seems not
	# to be triggered in msys, need bare metal to test

	message("defining _WIN32 in cmake")
	add_compile_definitions(_WIN32)
else()
	message("NOT defining _WIN32 in cmake")
endif()

if(DEFINED ENV{SYNTRAN_INTEL})
	set(CMAKE_Fortran_FLAGS "-W1 -qopenmp -qmkl -fPIC -fpp")
	add_compile_definitions(SYNTRAN_INTEL)
else()

	set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fopenmp -cpp")

	# Why would you warn about tabs in free-form Fortran?
	add_compile_options("-Wno-tabs")

	# More false alarms than anything
	add_compile_options("-Wno-maybe-uninitialized")

	# I need to compare reals because syntran users need to be able to compare
	# reals if they want to
	add_compile_options("-Wno-compare-reals")

endif()

set(CMAKE_Fortran_FLAGS_DEBUG "-pedantic -fbounds-check -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

message("")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	message("Debug")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG}")
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
	message("Release")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE}")
else()
	message("Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()
message("CMAKE_Fortran_COMPILER = ${CMAKE_Fortran_COMPILER}")
message("CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")
message("")

if(APPLE)

	set(CMAKE_EXE_LINKER_FLAGS "-openmp")

else()

	if(DEFINED ENV{SYNTRAN_INTEL})
		set(CMAKE_EXE_LINKER_FLAGS "-fopenmp -qmkl")
	else()
		set(CMAKE_EXE_LINKER_FLAGS "-fopenmp")
	endif()

endif()

message("")
message("CMAKE_EXE_LINKER_FLAGS = ${CMAKE_EXE_LINKER_FLAGS}")
message("")

set(SRC_DIR "src")

#===============================================================================

# Core library, used by main interpreter and unit tests
set(LIB "${PROJECT}")
set(LIB_SRC
	${SRC_DIR}/compiler.F90
	${SRC_DIR}/consts.f90
	${SRC_DIR}/utils.f90
	${SRC_DIR}/errors.f90
	${SRC_DIR}/value.f90
	${SRC_DIR}/bool.f90
	${SRC_DIR}/math_bin_add.f90
	${SRC_DIR}/math_bin_subtract.f90
	${SRC_DIR}/math_bin_mul.f90
	${SRC_DIR}/math_bin_div.f90
	${SRC_DIR}/math_bin_pow.f90
	${SRC_DIR}/math_bit_left_shift.f90
	${SRC_DIR}/math.f90
	${SRC_DIR}/types.f90
	${SRC_DIR}/lex.f90
	${SRC_DIR}/parse_array.f90
	${SRC_DIR}/parse_control.f90
	${SRC_DIR}/parse_expr.f90
	${SRC_DIR}/intr_fns.f90
	${SRC_DIR}/parse_fn.f90
	${SRC_DIR}/parse_misc.f90
	${SRC_DIR}/parse.f90
	${SRC_DIR}/eval.f90
	${SRC_DIR}/core.f90
	${SRC_DIR}/syntran.f90
	)

#add_library(${LIB} SHARED ${LIB_SRC}) # dll
add_library(${LIB} STATIC ${LIB_SRC}) # static lib

set(LINK_LIBS "${LIB}")

#===============================================================================

# Main interpreter exe
set(EXE "${PROJECT}")
set(EXE_BIN "${EXE}-bin")
set(EXE_SRC
	${SRC_DIR}/app.f90
	${SRC_DIR}/main.f90
	)

add_executable("${EXE_BIN}"
	${EXE_SRC}
	)
target_link_libraries(${EXE_BIN} ${LINK_LIBS})

# Workaround exe and lib w/ same name:
#
#     https://gist.github.com/jlgerber/eafc4ee2b9954e27dd2bb009496b1b03
#
set_target_properties("${EXE_BIN}" PROPERTIES OUTPUT_NAME "${EXE}")

#===============================================================================

# Unit tests exe
set(TEST_EXE "test")
set(TEST_SRC
	${SRC_DIR}/tests/core.f90
	${SRC_DIR}/tests/test.f90
	)

add_executable(${TEST_EXE}
	${TEST_SRC}
	)
target_link_libraries(${TEST_EXE} ${LINK_LIBS})

# Thanks gfort, I'm aware of integer division
target_compile_options(${TEST_EXE} PRIVATE "-Wno-integer-division")

#===============================================================================

# This tells cmake to use the fortran compiler as the linker.  See:
#
#     https://stackoverflow.com/questions/40767212/use-fortran-linker-in-cmake-mixed-language-project
#
set_property(TARGET ${EXE_BIN} PROPERTY LINKER_LANGUAGE Fortran)

