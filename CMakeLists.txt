
cmake_minimum_required(VERSION 3.7)

if(DEFINED ENV{SYNTRAN_INTEL})

	set(CMAKE_Fortran_COMPILER "ifort")

else()

	set(CMAKE_Fortran_COMPILER "gfortran")

endif()

set(PROJECT syntran)

project(${PROJECT} Fortran)

if(DEFINED ENV{SYNTRAN_INTEL})
	set(CMAKE_Fortran_FLAGS "-W1 -qopenmp -qmkl -fPIC")
else()

	set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fopenmp")

	# Why would you warn about tabs in free-form Fortran?
	add_compile_options("-Wno-tabs")

	# More false alarms than anything
	add_compile_options("-Wno-maybe-uninitialized")

	# I need to compare reals because syntran uses need to be able to compare
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
	${SRC_DIR}/utils.f90
	${SRC_DIR}/errors.f90
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

