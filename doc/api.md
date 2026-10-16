
# Using syntran as a Fortran library

Syntran is an interpreter written in Fortran, so a Fortran program can embed
it:  hand a string of syntran source to a function and get the result back as a
Fortran value.  This is handy for user-supplied expressions in a config file,
a formula field in an input deck, a small scripting layer in a larger
simulation, and so on.

```fortran
program main
	use syntran
	implicit none
	print *, syntran_eval_i32('1 + 2 * 3;')   ! 7
end program main
```

Runnable examples are in the [`example/`](../example) directory:

| File | Shows |
|---|---|
| [`hello_world.f90`](../example/hello_world.f90) | the smallest possible program:  `syntran_eval('println("hello world");')` |
| [`eval_basic.f90`](../example/eval_basic.f90) | typed results (`i32`, `i64`, `f64`, `bool`, `str`), arrays, user functions |
| [`eval_errors.f90`](../example/eval_errors.f90) | handling parse errors, runtime errors, and wrong result types without exiting the host |
| [`interpret_file.f90`](../example/interpret_file.f90) | running a whole script file |

Run one from the project root with fpm, for example
`fpm run --example eval_basic`.

## Depending on syntran

### With fpm

**A plain `git` dependency does not work right now.**  Syntran vendors the
[isocline](https://github.com/daanx/isocline) line editor as a git submodule
(`external/isocline`), and fpm does not fetch submodules of dependencies, so
building fails with

```
fatal error: '../../external/isocline/src/isocline.c' file not found
```

You can either [build without isocline](#without-isocline), or clone syntran
recursively and use a `path` dependency:

```bash
git clone --recursive https://github.com/JeffIrwin/syntran
```

```toml
# fpm.toml of your project.  fpm requires path dependencies to be relative
[dependencies]
syntran = { path = "../syntran" }
```

Then `use syntran` in any of your sources.

#### Without isocline

If you only want to embed the interpreter, you don't need isocline.  Build with
`SYNTRAN_NO_ISOCLINE` defined and the submodule isn't read at all, so a plain
`git` dependency works:

```toml
[dependencies]
syntran = { git = "https://github.com/JeffIrwin/syntran" }
```

```bash
fpm build --c-flag "-DSYNTRAN_NO_ISOCLINE"
```

The macro is only read by the C shim, so it goes in `--c-flag`, not `--flag`.
It removes line editing (history, arrow keys) from the REPL.  Everything else
is unchanged, and the REPL still works with plain line input, like it does for
piped stdin.

#### BLAS and LAPACK

The `@` operator on `f32`/`f64` arrays calls BLAS `gemm`,
[`std::solve()`](intrinsics.md#solve) calls LAPACK `gesv`, and
[`std::lu_factor()`](intrinsics.md#lu_factor)/[`std::lu_solve()`](intrinsics.md#lu_solve)
call LAPACK `getrf`/`getrs`.  They must be installed to link:

- macOS: nothing to install, the Accelerate framework is used
- Debian/Ubuntu: `sudo apt-get install libopenblas-dev`.  Install OpenBLAS, not
  the reference `libblas-dev`/`liblapack-dev`, which are many times slower
- Alpine: `apk add openblas-dev`, then `ln -s libopenblas.so /usr/lib/libblas.so`
- Fedora/Rocky: `dnf install --enablerepo=crb openblas-devel blas-devel lapack-devel`

Building syntran itself with fpm links them by default.  To build without them,
use the `noblas` feature, which uses portable Fortran instead.  It's slower for
large matrices but otherwise gives the same results up to rounding:

```bash
fpm build --features release,noblas
```

`--features` takes the place of `--profile`, so name `release` or `debug` in
the list.  It needs fpm 0.13.0 or later.

**Depending on syntran from another fpm project** links BLAS and LAPACK for you
too, so there's nothing to do other than installing them.  fpm only applies
features of the package being built, so `--features noblas` isn't available to
your project.  Define the macro yourself instead:

```bash
# The macro is read by a Fortran file, so it goes in --flag, not --c-flag
fpm build --flag "-DSYNTRAN_NO_BLAS"
```

That removes the calls, but fpm still links `-llapack -lblas` for the
dependency, so they must still be present to link.

### With CMake

The CMake build produces a static library, which the `syntran`
executable, the tests, and your own targets can link against.  See
`add_library(${LIB} ...)` in the top-level `CMakeLists.txt`.  The `example/`
programs are only wired up for fpm.

Pass `-DSYNTRAN_NO_ISOCLINE=ON` to build without isocline, as described above.

CMake finds BLAS and LAPACK with `find_package(LAPACK)`, and it's an error if
they're not found.  Pass `-DSYNTRAN_NO_BLAS=ON` to build without them, as
described [above](#blas-and-lapack).

## Reference

Everything below is in the `syntran` module (`src/syntran.f90`).  Unless a
function says otherwise, the optional arguments are the same across them.

### Typed evaluation

```fortran
integer                  function syntran_eval_i32(str_, quiet, io)
integer(kind = 8)        function syntran_eval_i64(str_, quiet, io)
real(kind = 4)           function syntran_eval_f32(str_, quiet, io)
real(kind = 8)           function syntran_eval_f64(str_, quiet, io)
logical                  function syntran_eval_bool(str_, quiet, io)
character(len = :), allocatable
                         function syntran_eval_str(str_, quiet, io)
```

Parse and evaluate `str_`, and return the value of the final expression.

The result type must match the function.  `syntran_eval_i32('1.5;')` is an
error, not a conversion, so cast in the syntran source if you want one:
`syntran_eval_i32('i32(1.5);')`.  On any failure the function returns 0 (or
`''`, or `.false.`), so check `io` if a legitimate result of 0 is possible.

### `syntran_eval()`

```fortran
character(len = :), allocatable function syntran_eval(str_, quiet, src_file, &
		chdir_, script_args, diags, syntax_only, transpile, io)
```

Works for any result type, including arrays and structs.  The result is
formatted as a string, as it would be printed by the REPL.  Use this when there
is no typed function for what you need, or you want to display the result.

| Argument | Meaning |
|---|---|
| `str_` | The syntran source |
| `quiet` | Don't print diagnostics.  **Pass `.true.` from a library**, see below |
| `src_file` | A name to report in diagnostics, and the base for relative `use`/`#include` paths.  If absent, the source is named `<stdin>` |
| `chdir_` | A *string*:  the directory for resolving relative file paths, e.g. for `open()` |
| `script_args` | A `string_vector_t` of arguments, available to the script through `std::args()` |
| `diags` | `string_vector_t` of diagnostics, one element per error |
| `syntax_only` | Parse and type check, but don't run anything |
| `transpile` | Produce Fortran source instead of running.  See [transpile.md](transpile.md) |
| `io` | `exit_success` or `exit_failure` |

### `syntran_eval_value()`

```fortran
subroutine syntran_eval_value(str_, val, quiet, want_type, src_file, chdir_, &
		script_args, diags, syntax_only, transpile, io)
```

The common front end of all of the above.  It returns the raw `value_t`, for
result types that don't have a typed function above.

### `syntran_interpret_file()`

```fortran
character(len = :), allocatable function syntran_interpret_file(filename, &
		quiet, quiet_info, chdir_, script_args, diags, syntax_only, &
		transpile, io)
```

Run a whole script file, like `syntran file.syntran` does on the command line.
Output from the script goes to stdout.  `quiet` and `quiet_info` both suppress
the `Interpreting file "..."` line that is otherwise printed first, and `quiet`
also keeps a runtime error from calling `exit()`.  Note that here `chdir_` is a *logical*
which says whether to change the working directory to the file's directory,
unlike `syntran_eval()`.

### `syntran_interpret()`

The interactive REPL, which is what the `syntran` executable runs when you give
it no file.  Embedding it is rarely what you want.

It also takes `startup_file` and `script_args`.  `chdir_` is a *logical*, as in
`syntran_interpret_file()`:  if it is true, relative file paths are resolved
against the directory of `startup_file`.  This is what `--cd` does with `-i`.

## Things to know

### Errors and `quiet`

If `quiet` is not `.true.`, the library behaves like the command-line
interpreter does:  diagnostics are printed to stdout, and **a runtime error
calls `exit()`, which ends your whole program**.  That is what you want for a
script runner, but not for a library.

So from a host program, pass `quiet = .true.` and check `io` and/or `diags`:

```fortran
integer :: i, io
type(string_vector_t) :: diags
character(len = :), allocatable :: res

res = syntran_eval(src, quiet = .true., diags = diags, io = io)
if (io /= exit_success) then
	do i = 1, diags%len_
		print *, diags%v(i)%s
	end do
end if
```

Diagnostics are rendered like the ones on the command line, with the location and
a caret under the offending source.  Each has an error code such as `E48` or
`R2`, which is described in [errors.md](errors.md).

### No state between calls

Every call starts with a fresh interpreter.  Variables and functions declared by
one `syntran_eval*()` call are not visible to the next.  Put everything that
belongs together into one string.

### Getting data in

There's no way to bind a Fortran variable into a syntran program yet.  Build the
source string instead, for example with an internal write:

```fortran
write(buf, '(a, es24.16, a)') 'sqrt(', x, ');'
y = syntran_eval_f64(trim(adjustl(buf)), quiet = .true.)
```

Use enough digits to round trip, and remember that syntran's float literals
need a decimal point or exponent to be an `f64`.  Arrays can be passed as
`[1.0, 2.0, 3.0]` literals.

### Performance

Programs are parsed and compiled to bytecode on every call, so the overhead
per call is more significant than the evaluation of a short expression.  If you
have a loop that is calling the same expression thousands of times, write the
loop in syntran and call the library once.
