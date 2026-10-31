# Compiling natively with LLVM

Syntran can compile a program to machine code with [LLVM](https://llvm.org),
instead of running it with the interpreter.

```
syntran --native fib fib.syntran
./fib
```

`--native` takes the output file, then the input is a file, or a command string
with `-c`.  What is written depends on the extension of the output:

| Output | What it is |
|---|---|
| `fib` (anything else) | An executable.  The object file is linked with the runtime by the C compiler in `$CC`, or `cc` |
| `fib.o` | An object file, which is not linked |
| `fib.ll` | The textual LLVM IR, for looking at what the compiler made of the program |

By default, the compiled program prints the value of the last statement of the
program when it finishes, just like the interpreter does.  Use `--quiet` to leave
that out.

The program is compiled for the machine that syntran is running on, using all of
its instruction set, and is optimized with LLVM's `-O2` pipeline.  Floating
point is not fast-math, and operations are not fused, so the results are the
same as the interpreter's.

## Building syntran with LLVM

This is opt-in, because it needs LLVM (version 17 or later) installed.  Without
it, syntran builds as always and `--native` is an error.

```
brew install llvm          # or: sudo apt install llvm-dev
export LIBRARY_PATH=$(llvm-config --libdir)
fpm build --features debug,llvm-blas
```

fpm allows only one feature to link libraries, so `llvm-blas` is both BLAS and
LLVM, and `--features debug,noblas,llvm` is LLVM without BLAS.  With CMake, pass
`-DSYNTRAN_LLVM=ON`, and `-DCMAKE_PREFIX_PATH=$(llvm-config --prefix)` if it
isn't found.

## What is supported

This is a subset of the language so far:

- Types: `i32`, `i64`, `f32`, `f64`, `bool`, `str`, and arrays of those, of any
  rank
- Variables and assignment, including compound assignment.  Scopes and shadowing
- Operators: arithmetic, comparison, logical, bitwise, and shifts.  Strings are
  joined with `+`.  Operators apply to each element of an array
- `if`, `while`, `for` (over a range, an array, or a string), `switch`, `break`,
  `continue`, and `return`
- Functions, including recursion and `&` and `&const` parameters, which can take
  and return numbers, bools, strings, and arrays
- `use` of other modules
- Array literals of every kind: `[a, b, c]`, `[a: b]`, `[a: step: b]`,
  `[a: b; n]`, `[v; n, m]`, and `[a, b, c; n, m]`.  Elements and slices
  (including steps, omitted bounds, and vectors of indices) can be read and
  assigned, and so can characters and substrings of a string
- These intrinsic functions: `println`, `std::print`, `str`, `exit`, `len`,
  `repeat`, `char`, `i32`, `i64`, `f32`, `f64`, `abs`, `min`, `max`, `sqrt`,
  `exp`, `log`, `log2`, `log10`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`,
  `parse_i32`, `parse_i64`, `parse_f32`, `parse_f64`, `parse_bool`, `size`,
  `reshape`, and `sum`, `product`, `minval`, `maxval`, `all`, `any`, and `count`
  of a whole array.  The math functions and conversions also apply to each
  element of an array
- Structs, including methods, members that are arrays or other structs, arrays
  of structs, and optional structs (`?Node`) that can be `null`.  A struct is a
  value, so assigning or passing one copies it
- Enums, including arrays of them and enums as members of structs.  They compare
  and print like the interpreter's, and `i32(e)` and `Suit(2)` convert

## What isn't supported yet

Programs which use anything else are rejected with
[E130](errors.md#e130----native-unsupported), pointing at each statement which
isn't supported.  Run them with the interpreter instead.  So far, that includes
files, function pointers, the matrix product `@`, a `dim` or `mask` argument
of the reductions above, and slices or substrings of a member of a struct.

## Differences from the interpreter

The output of a compiled program is the same as the interpreter's, but:

- Syntran runtime errors are not checked yet.  In particular, a subscript which
  is out of bounds is undefined behavior, as it is in the interpreter unless it
  is built with `SYNTRAN_BOUNDS_CHECK`.
- Overflow of integers wraps around, like in the interpreter.  Division of an
  integer by zero is 0, and `%` by zero is the dividend, like it is there.

## How it works

Like the [Fortran backend](transpile.md), this lowers the typed AST rather than
the bytecode, since many opcodes of the bytecode call back into the interpreter.
The parser has already resolved every name to a unique slot, so syntran's block
scopes never need to be mirrored.  A global variable is an LLVM global, and the
top-level statements are `main()`.

The LLVM-C API is called directly from Fortran, through the interfaces in
[`src/llvm_c.f90`](../src/llvm_c.f90).  The emitter is
[`src/native.F90`](../src/native.F90), and all of it that calls LLVM is inside
`#ifdef SYNTRAN_LLVM`.

The runtime that a compiled program is linked with is
[`src/rt/syntran_crt.c`](../src/rt/syntran_crt.c).  It is embedded in
`src/native_rt.f90` by `src/gen_native_rt.sh`, so syntran doesn't need to find
it at run time.  A string is a heap object that the generated code passes around
by pointer.  The code that makes a string owns it until it is moved into a
variable or consumed by an operation, and a string literal is a constant that is
never freed.  An array is the same kind of object: a fixed-size block of
elements in column-major order, with its rank and shape, that is copied when it
is assigned and freed when its owner is.  Element access is compiled inline,
while slicing, and the construction of ranges, are in the runtime.  A struct is
a heap object too, with a slot for each member after a pointer to a constant
that describes the struct, so the runtime can copy, free, and print it.  An
enum is the index of its variant, with constant tables of the variants' names
and backing values.  The runtime writes floats exactly as the Fortran runtime
does, which a unit test checks against it.

To check a change to the native backend, run

```
bash utils/test-transpile.sh --backend native
```

which compiles and runs every syntran test program and sample, and compares the
output with the interpreter's.  It reports which unsupported constructs are the
most common among the programs that it can't compile yet.  Every file in
`utils/native-pass.txt` must keep passing.
