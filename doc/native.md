# Compiling natively with LLVM

Syntran can compile a program to machine code with [LLVM](https://llvm.org),
instead of running it on the bytecode VM, in one of two ways: ahead of time
(AOT) with `--aot`, which writes an executable, or just in time (JIT) with
`--jit`, which compiles in memory and runs the program right away.  Both use
the same compiler, which this page calls the native backend.

To compile ahead of time:

```
syntran --aot fib fib.syntran
./fib
```

`--aot` (or `-o`) takes the output file, then the input is a file, or a command string
with `-c`.  What is written depends on the extension of the output:

| Output | What it is |
|---|---|
| `fib` (anything else) | An executable.  The object file is linked with the runtime by the C compiler in `$CC`, or `cc` |
| `fib.o` | An object file, which is not linked |
| `fib.ll` | The textual LLVM IR, for looking at what the compiler made of the program |

To JIT-compile and run a program in one step, without writing a file, use `--jit`
(or `-j`):

```
syntran --jit fib.syntran -- 30
```

The program is compiled in memory with LLVM's JIT and runs inside syntran's own
process.  It prints, exits, and reads `std::args()` just as the executable
would, so the output and exit status are the same.  It needs LLVM, but no C
compiler.  The runtime that it uses is the one built into syntran, which is
compiled with the flags of syntran, not `-ffp-contract=off`, and without BLAS.
So float results, especially of `@` and `std::solve()`, can differ from an
executable in the last digits.  Use `--aot` if that matters.

`--jit` honors `--cd`, so a program's relative file paths are resolved against
the directory of its source file, as in the interpreter.  An executable made by
`--aot` doesn't: its relative paths are resolved against the directory that it is
run from.

By default, the compiled program prints the value of the last statement of the
program when it finishes, just like the interpreter does.  Use `--quiet` to leave
that out.

The program is compiled for the machine that syntran is running on, using all of
its instruction set, and is optimized with LLVM's `-O2` pipeline.  Floating
point is not fast-math, and operations are not fused, so the results are the
same as the interpreter's.

## Official binaries

The Linux, macOS, and Windows binaries from the releases page all have the native
backend built in, with LLVM linked statically, so `--jit` needs nothing else
installed.  `--aot` also needs a C compiler to link the executable (`$CC`, or
`cc`, or `gcc` on Windows), and a program that uses linear algebra (`@`,
`std::solve()`, etc.) needs BLAS and LAPACK for the linker too:

- **Linux**: `cc`, and BLAS and LAPACK from e.g. `libopenblas-dev`.
  `Dockerfile.rocky` builds the binary.
- **macOS**: `cc` from Xcode's command line tools, and linear algebra links
  Accelerate, which comes with the system.  The binary is built with
  `./build.sh release --llvm --blas -- -DSYNTRAN_LLVM_STATIC=ON`, where
  `SYNTRAN_LLVM_STATIC` takes LLVM's component archives from `llvm-config
  --link-static` instead of `libLLVM.dylib`, so the package doesn't need LLVM,
  zstd, or z3 dylibs.
- **Windows**: a mingw `gcc` on the `PATH`, which Windows doesn't come with, and
  OpenBLAS for it, e.g. from MSYS2.  See the `$env:CC` example under
  [Windows](#windows) below.  The package is still just `syntran.exe` with no DLLs
  next to it, and it is built as in
  [Linking LLVM statically on Windows](#linking-llvm-statically-on-windows).

## Building syntran with LLVM

This needs LLVM (version 17 or later) installed.  Without it, syntran builds as
always and `--aot` is an error.  `build.sh` turns the native backend on if it
finds `llvm-config` (including Homebrew's keg-only one), and `--llvm` or
`--no-llvm` overrides that:

```
brew install llvm          # or: sudo apt install llvm-dev
./build.sh                 # cmake
./build.sh --fpm           # fpm, installed to build/fpm/Debug/bin/
```

The same thing, by hand with fpm:

```
export LIBRARY_PATH=$(llvm-config --libdir)
fpm build --features debug,llvm-blas
```

fpm allows only one feature to link libraries, so `llvm-blas` is both BLAS and
LLVM, and `--features debug,noblas,llvm` is LLVM without BLAS.  With CMake, pass
`-DSYNTRAN_LLVM=ON`, and `-DCMAKE_PREFIX_PATH=$(llvm-config --prefix)` if it
isn't found.

### Windows

Use the LLVM from [MSYS2](https://www.msys2.org)'s `ucrt64` environment, which
targets the same C runtime (UCRT) and `x86_64-w64-windows-gnu` triple as a mingw
gfortran.  The official LLVM installers target MSVC, which doesn't link with
gcc.  You need fpm 0.13.0 or later for the `llvm` features.

From Git Bash, `bash build.sh --fpm` (or just `bash build.sh`) finds the MSYS2
LLVM and OpenBLAS, makes the private dirs below, and passes the flags for you.
To do it by hand:

```powershell
C:\msys64\usr\bin\pacman.exe -S --needed --noconfirm mingw-w64-ucrt-x86_64-llvm mingw-w64-ucrt-x86_64-openblas

# Don't put C:\msys64\ucrt64\lib itself on -L, because its libgfortran.a would
# shadow your toolchain's.  Copy the libraries that are needed into private dirs
New-Item -ItemType Directory -Force build/llvm-win, build/openblas
Copy-Item (Get-Item C:\msys64\ucrt64\lib\libLLVM-*.dll.a).FullName build/llvm-win/libLLVM.a
Copy-Item C:\msys64\ucrt64\lib\libopenblas.a build/openblas/libblas.a
Copy-Item C:\msys64\ucrt64\lib\libopenblas.a build/openblas/liblapack.a

# libLLVM-*.dll is loaded when syntran starts, and needs the runtime DLLs next to it
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"

fpm install --test --features debug,llvm-blas --prefix ./build/native/ --flag "-Wno-tabs -static -fopenmp -Lbuild/openblas -Lbuild/llvm-win -Wl,--wrap=setlocale -Wl,--stack,67108864"
```

(The import library, `libLLVM-<major version>.dll.a`, is found with a wildcard,
since MSYS2 bumps the version of LLVM.)

The flags other than the `-L`s are the same as for any Windows build of syntran.
Compiling a program links it with `gcc`, or `$CC`.  A program which uses `@` or
`std::solve()` links BLAS and LAPACK, so give `CC` the same flags:

```powershell
$env:CC = "gcc -static -fopenmp -L$PWD/build/openblas"
build\native\bin\syntran --aot hello.exe -c 'println(\"hello\");'
```

#### Linking LLVM statically on Windows

The build above needs `libLLVM-*.dll` and the DLLs under it next to syntran.exe or
on the `PATH`, which add up to ~150 MB.  The official binary links LLVM's component
archives instead, like `Dockerfile.rocky` does on Linux.  MSYS2's `llvm` package
has them in `C:\msys64\ucrt64\lib` as well as the DLL.  There is no single
`libLLVM.a`, so `utils/gen-llvm-static-win.sh` writes one in a private dir, as a
linker script which groups the archives that syntran uses with zlib and zstd
(its output also needs MSYS2's `llvm-nm`).  Run it from Git Bash or MSYS2:

```bash
export PATH="/c/msys64/ucrt64/bin:$PATH"
bash utils/gen-llvm-static-win.sh build/llvm-static
```

It also writes `build/llvm-static/syntran.def`, which makes syntran.exe export
the few LLVM fns that `src/c/llvm_shim.c` finds at run time with
`GetProcAddress()`, because the fns that exist depend on the version of LLVM.
A static LLVM exports nothing by default, so without this `--jit` would fail.
Then build as above, but with `-Lbuild/llvm-static -Wl,build/llvm-static/syntran.def`
in place of `-Lbuild/llvm-win`.  `--gc-sections` and `-s` shrink it from 157 MB to
106 MB.

Build with MSYS2's own gfortran (`pacman -S mingw-w64-ucrt-x86_64-gcc-fortran`, with
`C:\msys64\ucrt64\bin` first on the `PATH`), because the archives were compiled by
a newer GCC and mingw-w64 than other mingw distributions might have.  GCC 15.2 and
mingw-w64 v13 fail to link with undefined references to
`std::__get_once_callable()`, `std::__get_once_call()`, and `__imp__setjmp`.  The
result is still static, so it doesn't need MSYS2 to run:

```powershell
fpm install --features release,llvm-blas --prefix ./build/native/ --flag "-static -fopenmp -Lbuild/openblas -Lbuild/llvm-static -Wl,build/llvm-static/syntran.def -Wl,--wrap=setlocale -Wl,--stack,67108864 -Wl,--gc-sections -Wl,-s"
```

The result imports only Windows system DLLs, which CI checks with `objdump -p`.

## What is supported

- Types: `i32`, `i64`, `f32`, `f64`, `bool`, `str`, and arrays of those, of any
  rank
- Variables and assignment, including compound assignment.  Scopes and shadowing
- Operators: arithmetic, comparison, logical, bitwise, and shifts.  Strings are
  joined with `+`.  Operators apply to each element of an array.  A chain of
  `+ - * /` on arrays and numbers, like a stencil
  `u[1: n-1] += 0.25 * (u[0: n-2] - 2.0 * u[1: n-1] + u[2: n])`, is one loop that
  reads a consecutive slice of a rank-1 array in place, so it copies no slice and
  makes no array between the operators
- `if`, `while`, `for` (over a range, an array, or a string), `switch`, `break`,
  `continue`, and `return`
- Functions, including recursion and `&` and `&const` parameters, which can take
  and return numbers, bools, strings, and arrays
- `use` of other modules
- Array literals of every kind: `[a, b, c]`, `[a: b]`, `[a: step: b]`,
  `[a: b; n]`, `[v; n, m]`, and `[a, b, c; n, m]`, and the empty `[Node; 0]`.
  Elements and slices (including steps, omitted bounds, and vectors of indices)
  can be read and assigned, also with a compound assignment like
  `a[1, :] += 100`, and so can characters and substrings of a string, and of
  each element of a slice of an array of strings.  These work on the members of
  a struct too.  The subscripts of the result of a call, like `f()[i]`, work as
  well
- These intrinsic functions: `println`, `std::print`, `str`, `exit`, `len`,
  `repeat`, `char`, `i32`, `i64`, `f32`, `f64`, `abs`, `min`, `max`, `sqrt`,
  `exp`, `log`, `log2`, `log10`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`,
  `sind`, `cosd`, `tand`, `asind`, `acosd`, `atand`,
  `parse_i32`, `parse_i64`, `parse_f32`, `parse_f64`, `parse_bool`, `size`,
  `reshape`, `shape`, and `sum`, `product`, `minval`, `maxval`, `all`, `any`, and
  `count` of an array, with a `dim` or a `mask` or both.  The math functions and
  conversions also apply to each element of an array
- The constant `std::PI`
- `std::format()`, `std::printf()`, and `std::writef()`, of numbers, bools,
  strings, arrays of those, structs, and enums, with every spec of the
  interpreter's
- Linear algebra: the matrix product `@`, and `std::transpose()`, `std::dot()`,
  `std::norm2()`, `std::solve()`, `std::lu_factor()`, and `std::lu_solve()`.
  If syntran was built with BLAS and LAPACK, a program which multiplies
  matrices of floats or solves is linked with them (`-llapack -lblas`), so that
  the results are the same as the interpreter's, so BLAS and LAPACK must be
  installed where the program is compiled, or the link fails.  If syntran was
  built without them, and for integers, the runtime does the same operations in
  the same order itself
- `std::print_trace()`, `std::stack_trace()`, and `std::caller()`, which look at the
  stack of calls, as a runtime error does
- `std::args()`, which is the arguments that the compiled program is run with,
  `std::getenv()`, `std::hasenv()`, and `std::system()`
- `std::move()` of an array or a str, which leaves an empty one in its variable or
  member, and assigning the result to a variable or member doesn't copy it
- Structs, including methods, members that are arrays or other structs, arrays
  of structs, and optional structs (`?Node`) that can be `null`.  A struct is a
  value, so assigning or passing one copies it
- Files: `open`, `std::try_open`, `close`, `readln`, `eof`, `writeln`,
  `std::write`, `std::exists`, `std::read_file`, `std::read_i32` and the other
  `std::read_<type>` fns, the `ok` argument of `parse_<type>()`, `std::IN`, `std::OUT`, and `std::ERR`, and the members `f.is_open`,
  `f.eof`, and `f.name`.  A file handle is a value, so assigning or passing one
  copies it
- Function pointers: `fn(i32): i32` variables, parameters, return values, and
  members of structs, including the subscripts of the result of a call.  They
  compare with `==` and print as their type, as in the interpreter
- Enums, including arrays of them and enums as members of structs.  They compare
  and print like the interpreter's, `i32(e)` and `Suit(2)` convert, and
  `std::variant_of()` gives the name of a variant

## What isn't supported yet

Programs which use anything else are rejected with
[E130](errors.md#e130----native-unsupported), pointing at each statement which
isn't supported.  Run them with the interpreter instead.  Everything that the
test programs and samples use is supported, and so are the Advent of Code
solutions in `external/aoc-syntran`, so a program which isn't is rare.

## Differences from the interpreter

The output of a compiled program is the same as the interpreter's, but:

- Runtime errors are reported like the interpreter does, to stdout, with the
  code and message, the line of the source, and the stack of calls, and the
  program exits with status 1.  The only exception is a subscript which is out
  of bounds (R33).  It is undefined behavior, as it is in the interpreter, unless
  syntran was built with `SYNTRAN_BOUNDS_CHECK`, in which case the programs that
  it compiles check too.  The source line is the statement that failed, and not
  the expression, as in the interpreter.
- Keeping the stack of calls for that report costs a compiled program a little
  for each call of a fn, about a nanosecond on a recent machine.  It makes a
  recursive `fib()` which does nothing else about 50% slower.
- `sind()`, `cosd()`, and `tand()` are the runtime's own, rather than
  libgfortran's.  An angle with a simple result, like `sind(30.0)` or
  `tand(45.0)`, has exactly that, as in the interpreter, but others can differ
  from the interpreter's in the last digit.  So can `norm2()` and a matrix
  product if syntran was built without BLAS
- Overflow of integers wraps around, like in the interpreter.  Division of an
  integer by zero is 0, and `%` by zero is the dividend, like it is there.
- `std::args()` is all of the arguments after the name of the program.  There
  is no `--`, because there are no options of syntran to tell them apart from.
- Opening a file with mode `"w"` empties it right away.  The interpreter only
  does when it first writes to it.
- A method gets a copy of its receiver if that is an element of an array, like
  `a[1].bump()` or `w.items[1].bump()`, and it is put back when the method
  returns, like the interpreter does.  So a fn that the method calls sees the
  receiver as it was before the call, and if the method makes the receiver go
  away, like `bump()` assigning a shorter array to `a`, then the interpreter and
  a compiled program built with `SYNTRAN_BOUNDS_CHECK` report an out of bounds
  subscript (R33) when the method returns.  The copy costs as much as copying
  the struct.  A receiver which is a variable, or a member of one, is passed by
  address, as are the other `&` arguments, so if the fn also reads that variable
  by name, like a global, then it sees what the method wrote.  The interpreter
  moves it out of the variable for the call, so there a fn that reads it
  crashes, or sees what was there before the call for numbers and strings.
- A float which is out of range or NaN converts to an integer like the machine
  does: `i32(1.0e20)` saturates on Arm, and is the smallest integer on x86.
  That is the interpreter's too, when it is on the same machine.
- A shift by a negative count masks the count to the width of the integer, like
  the interpreter does.

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
(with and without the enum's name, for `str()` and `std::variant_of()`) and
backing values.  The runtime writes floats exactly as the Fortran runtime
does, which a unit test checks against it.

A runtime error is reported from the runtime, with the source line of the
statement that failed and the calls that led to it.  Each statement stores its
number in a global of the runtime when it starts, and a table of the source
lines, and one of the names of the fns, are handed to the runtime when the
program starts.  A fn pushes a frame, with its own id and the number of the
statement that called it, in its first block, and pops it in its last.  Both are
inline, so a call costs a few loads and stores.

To check a change to the native backend, there are two kinds of tests.  The first
runs the unit tests of the interpreter with every program that they evaluate
compiled by the native backend and run in memory, with LLVM's JIT, instead of
on the VM:

```
fpm test test --features debug,llvm-blas --flag -Wno-tabs -- --native
```

A program that stops with a runtime error, or calls `std::exit()`, leaves the
JIT with a `longjmp()` rather than `exit()`, and the runtime keeps its result and
its error for the test.  This skips the tests of the REPL, of the other
backends, and of the API that hands back a `value_t`, and a few cases that are
listed in `src/tests/test.f90`.  Every other test must pass.

The second kind compiles and links every syntran test program and sample, runs
it, and compares the output with the interpreter's.  It reports which
unsupported constructs are the most common among the programs that it can't
compile yet.  Every file in `utils/native-pass.txt` must keep passing.  Give it a
syntran that was built with LLVM, since its default, `build/v1/bin/syntran`,
usually isn't:

```
bash utils/test-transpile.sh --backend native build/native/bin/syntran
bash utils/test-transpile.sh --backend native --long build/native/bin/syntran
```

The second one compiles the programs of the long tests (`src/tests/long.f90`),
which are bigger programs from Advent of Code, and compares their output to the
interpreter's.  It takes a few minutes, so CI doesn't run it.  CI does run all of
the solutions in `external/aoc-syntran` with `--jit`, as the last check of each
release binary.
