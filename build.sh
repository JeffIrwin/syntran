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

exe=fint
srcdir=src
src=$srcdir/main.f90

#$fc -o main ../../utils.f90 ../*.f90 main.f90 $flags

$fc -o $exe $src $flags

