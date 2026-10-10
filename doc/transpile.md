# Transpiling to Fortran

Syntran can write a program as modern Fortran source, instead of running it with
the interpreter.  You then compile that with the Fortran compiler of your choice.

```
syntran --transpile fib.f90 fib.syntran
gfortran -O3 fib.f90 -o fib
./fib
```

`-t` is short for `--transpile`.  Its argument is the output file, or `-` for
standard output.  The input is a file, or a command string with `-c`.

The generated file is self-contained.  It embeds the small runtime that it needs
(formatting numbers like `println()` does, array ranges, etc.), so there is
nothing to link against.  It needs a compiler which supports Fortran 2018, for
assumed-rank arrays: gfortran 10 or later, ifx, nvfortran, and flang all do.
The degree trigonometric functions (`sind()`, `cosd()`, etc.) are Fortran 2023
intrinsics, so they need a recent compiler, or one with them as an extension.

By default, the generated program prints the value of the last statement of the
program when it finishes, just like the interpreter does.  Use `--quiet` to
leave that out.

A transpiled program is typically several times faster than the interpreter,
since it is compiled natively, and since the compiler can optimize it.

## What is supported

This is a subset of the language so far:

- Types: `i32`, `i64`, `f32`, `f64`, `bool`, `str`, enums, and arrays of those
  of any rank
- Variables and assignment, including compound assignment, assignment to
  elements and slices of arrays and to characters of strings, and assignment
  used as a value, like `a = b = 1`.  Scopes and shadowing
- Operators: arithmetic, comparison, logical, bitwise, and matrix multiplication
  (`@`).  They are also elementwise on arrays, and `+` and the comparisons are
  elementwise on arrays of strings
- `if`, `else if`, `else`, `while`, `for`, `break`, `continue`, `return`
- `switch`, with value, range, and guard arms, and a `default`.  The subject can
  be a scalar or a whole array
- Array literals of every form: `[a, b, c]`, `[a: b]`, `[a: step: b]`,
  `[a: b; n]`, `[v; n, m]`, and `[a, b, c, d; n, m]`
- Subscripts and slices, including steps, omitted bounds, and vector subscripts
- Functions, including recursion, arrays as parameters and return values, and
  parameters passed by reference with `&` or `&const`
- Structs: declarations, instances, members that are scalars, strings, arrays,
  enums, or other structs, assignment of members and of elements of array
  members (including compound assignment), arrays of structs, passing and
  returning structs, and printing.  Methods, const methods, and implicit access
  of the members of `self`
- Enums: declarations (with explicit values and aliases), `Suit.Hearts`, the
  casts `i32(Suit.Hearts)` and `Suit(2)`, comparison, printing, arrays of
  enums, a bare enum name as in `for v in Suit`, and `switch` on an enum
- File I/O: `open`, `std::try_open`, `close`, `writeln`, `readln`, `eof`,
  `std::exists`, the members `f.is_open`, `f.eof`, and `f.name`, and `readln()`
  and `eof()` of standard input.  `std::IN`, `std::OUT`, `std::ERR`, `std::PI`,
  `std::getenv`, `std::hasenv`, and `std::args()`
- Function pointers: a variable of a fn type, passing and returning one, a member
  of a struct that is one, calling one, and comparing two with `==` and `!=`
- Modules: `use`, qualified and glob imports, aliases, subdirectories, and the
  fns and variables of a module, including modules that import others.  The
  statements of a module run where it is imported
- These intrinsic functions: `println`, `str`, `len`, `repeat`, `char`, `size`,
  `count`, `all`, `any`, `sum`, `product`, `minval`, `maxval` (with `dim` and
  `mask` too), `norm2`, `dot`, `min`, `max`, `abs`, `exp`, `log`, `log2`,
  `log10`, `sqrt`, the trigonometric functions and their inverses in radians and
  in degrees, `i32`, `i64`, `parse_i32`, `parse_i64`, `parse_f32`, `parse_f64`,
  `exit`

## What isn't supported yet

Programs which use these are rejected with
[E115](errors.md#e115----transpile-unsupported), pointing at each statement
which isn't supported.  Run them with the interpreter instead.

- The call stack of the interpreter, which is `std::caller()`,
  `std::print_trace()`, and `std::stack_trace()`
- Assignment as a value inside the condition of a `while` loop or an `else if`
- Printing an array, or having an array of strings, with a rank above 4

## Differences from the interpreter

The output of a transpiled program is the same as the interpreter's, but a few
things are different:

- Syntran runtime errors, like [R33](errors.md#r33----subscript-oob) for a
  subscript out of bounds, are not checked.  Compile with `-fcheck=all` to catch
  these when developing, which stops with a Fortran runtime error.
- The order in which the operands of an operator are evaluated, and whether both
  operands of `and` and `or` are, is up to the Fortran compiler.  It only
  matters if the operands are function calls with side effects, like printing.
- Overflow of integers is whatever the compiler does, usually wrapping around.
- Floating point results can differ in the last digits if you compile with
  options like `-Ofast` or `-ffast-math`.  So can calls of math functions with
  constant arguments, which a compiler may evaluate itself when compiling.
- File I/O errors, like opening a file that doesn't exist, stop the program with
  a Fortran runtime error that has its own message.  `std::args()` is all of
  the command line arguments of the compiled program, while the interpreter's
  are those after `--`.
- A function that doesn't return a value on some path isn't detected at run
  time.
- The `Exiting syntran with status` message of `exit()` is never colored.

## How it works

Each variable is emitted as `<name>_g<n>` for a global or `<name>_l<n>` for a
local, where `n` is the unique slot that the parser gave it.  Syntran has block
scopes and shadowing, while Fortran declares everything at the top of a
procedure.  Distinct names sidestep that.  An array is a Fortran allocatable
array with the same rank, indexed from 1 internally, and an array of strings is
an array of a small wrapper type.  Functions are `recursive` procedures of a
module, and the top-level statements are a subroutine.

By-value array and string parameters are copied on entry, unless the compiler
can see that nothing could change the caller's variable during the call.

The runtime is in [`src/rt/syntran_rt.f90`](../src/rt/syntran_rt.f90).  It is
embedded in `src/transpile_rt.f90` by `src/gen_transpile_rt.sh`.

To check a change to the transpiler, run

```
bash utils/test-transpile.sh
```

which transpiles, compiles, and runs every syntran test program and sample, and
compares the output with the interpreter's.  It reports which unsupported
constructs are the most common among the programs that it can't compile yet.
