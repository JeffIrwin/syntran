#!/bin/bash

./build.sh release

# Repeat benchmark to get a sense for the natural variation in run times
n=10

echo "Running $n benchmarks each ..."

echo "=============================================================="
echo "    SYNTRAN:"

for ((i=1; i<=$n; i++)) ; do
	time ./build/syntran.exe src/benchmarks/primes-1.syntran
done

echo "=============================================================="
echo "    PYTHON:"

for ((i=1; i<=$n; i++)) ; do
	time python3 src/benchmarks/primes-1.py
done

echo "=============================================================="

