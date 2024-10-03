
# Automatic source generation

Some of the Fortran source code of the syntran interpreter is auto-generated.  To make it easy on users who build from source, and to make cross-platform builds easier (macOS does not have a functional `sed`), the pre-generated source is included in the git repo.  Hence, most users will never need to worry about source generation.

The script `src/gen_math.sh` uses the template `src/math_bin_template.f90` to create these source files:

```
src/math_bin_add.f90
src/math_bin_div.f90
src/math_bin_mul.f90
src/math_bin_pow.f90
src/math_bin_subtract.f90
```

This source defines binary arithmetic operators on various syntran types, for example:
```fortran
res%sca%i32 = left%sca%i32 + right%sca%i32
```

These expressions are nested inside many nested `select case` statements for every permutation of operand and result types and scalars vs arrays.  Further, every binary operator (`+`, `-`, `*`, `/`, `**`) is implemented.

These implementations are literally formulaic and it's easier to generate all the operators from a template rather than being WET.

For now, the modulo operator `%` as well as comparison operators (`<`, `>`, `<=`, etc.) are manual.  I might make these auto-generated as well in the future.

