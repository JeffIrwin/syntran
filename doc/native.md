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

This is opt-in, because it needs LLVM (version 15 or later) installed.  Without
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

- Types: `i32`, `i64`, `f32`, `f64`, `bool`, and `str`
- Variables and assignment, including compound assignment.  Scopes and shadowing
- Operators: arithmetic, comparison, logical, bitwise, and shifts.  Strings are
  joined with `+`
- These intrinsic functions: `println`, `std::print`, `str`, and `exit`

## What isn't supported yet

Programs which use anything else are rejected with
[E130](errors.md#e130----native-unsupported), pointing at each statement which
isn't supported.  Run them with the interpreter instead.  So far, that includes
control flow, functions, arrays, structs, and enums.

## Differences from the interpreter

The output of a compiled program is the same as the interpreter's, but:

- Syntran runtime errors are not checked yet.
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
never freed.  The runtime writes floats exactly as the Fortran runtime does, which
a unit test checks against it.

To check a change to the native backend, run

```
bash utils/test-transpile.sh --backend native
```

which compiles and runs every syntran test program and sample, and compares the
output with the interpreter's.  It reports which unsupported constructs are the
most common among the programs that it can't compile yet.  Every file in
`utils/native-pass.txt` must keep passing.
