#!/bin/bash

# TODO: cmake

# Fortran compiler
fc=gfortran
#flags="-cpp -O3 -fopenmp"
#flags="-cpp -Wall -Wextra -Wno-tabs"
#flags="-cpp -Wall -Wextra -Wno-tabs -fbounds-check"
flags="-cpp -Wall -Wextra -Wno-tabs -fbounds-check -Wno-maybe-uninitialized"

#fc=ifort
#flags="-fpp -check all -check bounds -traceback -check uninit"

$fc --version

exe=syntran
srcdir=src
src="$srcdir/utils.f90 $srcdir/core.f90"

# TODO: build lib instead of linking both exe's from scratch

# Build interpreter
$fc -o $exe $src $srcdir/main.f90 $flags

# Build unit tests
$fc -o test $src $srcdir/tests/test.f90 $flags

