#!/bin/bash

# TODO: cmake

# Fortran compiler
fc=gfortran
#flags="-cpp -O3 -fopenmp"
#flags="-cpp -Wall -Wextra -Wno-tabs"
flags="-cpp -Wall -Wextra -Wno-tabs -fbounds-check -Wno-maybe-uninitialized"

#fc=ifort
#flags="-fpp -check all -check bounds -traceback -check uninit"

$fc --version

exe=syntran
srcdir=src
src="$srcdir/utils.f90 $srcdir/main.f90"

$fc -o $exe $src $flags

