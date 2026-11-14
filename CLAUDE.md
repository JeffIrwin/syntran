# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Syntran

Syntran is an array-oriented programming language with an interpreter written in Fortran. It's similar to MATLAB but with curly braces, type checking, and zero-indexed arrays.

## CRITICAL: Tab Handling

**This codebase uses TABS for indentation, not spaces.**

When using the Edit tool:
1. ALWAYS read the file first to see the exact indentation
2. When extracting `old_string` from Read tool output, remember that the line number prefix format is: `spaces + line_number + tab`
3. Everything AFTER the tab following the line number is the actual file content (which uses tabs for indentation)
4. NEVER convert tabs to spaces in your `old_string` or `new_string` - preserve the exact tab characters
5. If Edit fails with "String to replace not found", the most common cause is tab/space mismatch - check the raw file content

## Fortran Submodule Gotcha

When adding a procedure to a submodule (`src/parse_*.f90`, `src/types_*.f90`,
etc.), prefix it with `module function` / `module subroutine`, both in the
submodule and in the parent module's interface block. gfortran doesn't care
if you forget, but Intel Fortran refuses to compile it.

## Build Commands

Two build systems are supported: FPM (Fortran Package Manager) and CMake.

### FPM
```bash
fpm build --profile debug   # Debug build, preferred for development
fpm build --profile release # Release build
fpm run --profile debug     # Build and run interpreter
fpm run --profile debug -- file.syntran     # Run a syntran file
yes | fpm clean             # Clean build artifacts
```

#### build.sh

`build.sh` wraps cmake, or fpm with `--fpm`, behind one set of options, so you
don't have to assemble `--features`/`--flag`/`LIBRARY_PATH` by hand.  LLVM and
BLAS are auto-detected (a link test, plus a search of the usual dirs), and
forced with `--llvm`/`--no-llvm` and `--blas`/`--no-blas`.  fpm output goes to
`build/fpm/<Debug|Release>/`.  `bash build.sh --help` lists everything.

```bash
bash build.sh --fpm --test     # fpm, debug, build + run the short tests
bash build.sh release --no-llvm
```

#### Runtime bounds checking

`-DSYNTRAN_BOUNDS_CHECK` turns an out-of-bounds `a[i]`/`s[i]`/slice/etc. into
a normal syntran runtime error (`RC_SUBSCRIPT_OOB`, R33) instead of a raw
Fortran bounds-check abort. CMake's `Debug` build type defines it
automatically; fpm needs it passed explicitly since it's off by default
(including in `fpm build --profile debug`):

```bash
fpm build --profile debug --flag "-DSYNTRAN_BOUNDS_CHECK"
fpm test test --profile debug --flag "-DSYNTRAN_BOUNDS_CHECK"
```

See `src/compiler.F90`'s `bounds_check` docstring for why the flag lives in
that one uppercase `.F90` file instead of a `#ifdef` at each check site.

#### BLAS/LAPACK

`@` on f32/f64, `std::solve()`, and `std::lu_factor()`/`std::lu_solve()` call BLAS
`gemm` / LAPACK `gesv`, `getrf`, and `getrs` through
`src/linalg.F90`, which is the only file with BLAS `#ifdef`s.  macOS resolves
`-llapack -lblas` to Accelerate with no install.  Linux needs an *optimized*
implementation (`sudo apt-get install libopenblas-dev`), not the reference
`libblas-dev`/`liblapack-dev`.

fpm links them through the `blas` feature in `fpm.toml`, which the default
profile always enables, so `fpm build`, `--profile debug`, etc. all link BLAS.
Opt out with `--features release,noblas` (or `debug,noblas`).  That defines
`SYNTRAN_NO_BLAS`, which swaps in portable Fortran fallbacks (intrinsic
`matmul`, a built-in LU), and links nothing.  `--features` can't be combined
with `--profile`, and it also drops the default profile.  CMake spells it
`-DSYNTRAN_NO_BLAS=ON`.  Features need fpm >= 0.13.0.  The test suite must
pass both ways:

```bash
fpm test test --profile debug
fpm test test --features debug,noblas
```

A project that *depends* on syntran links BLAS too, but can't select its
features, so it opts out with `--flag "-DSYNTRAN_NO_BLAS"`, which still links
the libraries.  See `doc/api.md`.

#### LLVM (the native backend)

`syntran --native` (see `doc/native.md`) compiles through LLVM.  It's opt-in:
without LLVM installed, syntran builds as always and `--native` is an error.
Everything that calls LLVM is inside `#ifdef SYNTRAN_LLVM` in `src/native.F90`
(an uppercase `.F90`) and `src/c/llvm_shim.c`.  `src/llvm_c.f90` is only
interfaces to the LLVM-C API, so it always compiles.

```bash
brew install llvm                       # or: sudo apt install llvm-dev
export LIBRARY_PATH=$(llvm-config --libdir)
fpm build --features debug,llvm-blas    # or debug,noblas,llvm
fpm install --test --features debug,llvm-blas --prefix ./build/native/
```

fpm allows only one feature with a `build` table, so `blas` + `llvm` can't be
combined and `llvm-blas` is both.  The C macro `SYNTRAN_LLVM` reaches C files
too.  CMake spells it `-DSYNTRAN_LLVM=ON`, and the official macOS binary adds
`-DSYNTRAN_LLVM_STATIC=ON` to link LLVM's archives instead of `libLLVM.dylib`
(CI's `Pack macOS` step fails if an LLVM/zstd/z3 dylib sneaks back in).  The test suite must pass both with
and without LLVM, since the latter is the default: the groups `native_rt` and
`native` in `src/tests/test.f90` only exercise the backend if it's built in.

`Dockerfile.rocky` builds the official Linux binary with the `llvm-blas` feature.
It links LLVM statically, through `/opt/llvm-static/libLLVM.a`, which is a GNU ld
linker script that groups the component archives (like `/opt/blas-static` does
for OpenBLAS), and its `ldd` test fails the build if a shared LLVM/libstdc++
sneaks back in.

The official Windows binary links LLVM statically too, so it has no DLLs to ship.
`utils/gen-llvm-static-win.sh` writes `build/llvm-static/libLLVM.a` (a linker
script over MSYS2's component archives, like `/opt/llvm-static` above) and
`syntran.def`, which exports the LLVM fns that `src/c/llvm_shim.c` finds with
`GetProcAddress()` (like the `--dynamic-list` above).  CI's `Pack Windows` step fails
if `syntran.exe` imports anything but system DLLs.  The build uses MSYS2's own
gfortran, since the runner's older libstdc++ and mingw-w64 lack fns that MSYS2's
LLVM archives need (`std::__get_once_callable`, `__imp__setjmp`).

On Windows, use MSYS2's ucrt64 LLVM (`pacman -S mingw-w64-ucrt-x86_64-llvm`),
not the official installer, which targets MSVC.  `doc/native.md` has the exact
commands.  The gotchas, each with a `_WIN32` branch in the source:

- `C:\msys64\ucrt64\bin` must be first on the `PATH` for `libLLVM-*.dll`, but
  `C:\msys64\ucrt64\lib` must **not** be on `-L` (its `libgfortran.a` shadows the
  toolchain's).  Copy `libLLVM-*.dll.a` to `build/llvm-win/libLLVM.a` and use
  `-Lbuild/llvm-win`
- gfortran doesn't define `_WIN32` or `__MINGW32__` for the preprocessor, so
  `src/native.F90` checks `OS=Windows_NT` at run time (`is_windows()`), to quote
  with `"` and to default to `gcc`, since mingw has no `cc`
- The UCRT's `printf` rounds ties away from zero, unlike glibc and libgfortran,
  so `src/rt/syntran_crt.c` selects mingw's stdio with `__USE_MINGW_ANSI_STDIO`
- The JIT can't `longjmp()` out of JIT'd frames on Windows (no SEH unwind info),
  so the runtime uses `__builtin_setjmp`/`__builtin_longjmp`, and
  `src/c/llvm_shim.c` defines the `__main` that LLVM calls on windows-gnu
- Regenerate `src/native_rt.f90` with `CC=gcc bash src/gen_native_rt.sh`, since
  there is no `cc`

#### Getting a stable path to a built binary

fpm puts its output in `build/gfortran_<HASH>/`, where the hash changes with
the **flag set** — `-static`, `-g`, `-fcheck=all` and a sanitizer build each
land in a different directory. Never hardcode one of those paths: it silently
runs a binary built with the wrong flags, or one that no longer exists. Use
`fpm install --prefix` instead, with a separate prefix per flag variant:

```bash
fpm install --test --profile release --flag "-static -Wl,--wrap=setlocale -Wl,--stack,67108864" --prefix ./build/v1/
./build/v1/bin/syntran.exe
./build/v1/test/test.exe     # --test also installs the test binaries
./build/v1/test/synup.exe    # including synup, see "Toolchain manager" below
```

#### Windows link flags: always pass `-Wl,--wrap=setlocale`

On Windows, builds **must** link with `-Wl,--wrap=setlocale`. Without it the
test suite hits an intermittent SIGSEGV in ~0.7–2% of runs.

It's a libgfortran bug, not a syntran one. mingw has no `uselocale()`, so
libgfortran save/restores the *global* locale around every formatted
READ/WRITE, retaining the pointer from `setlocale(LC_NUMERIC, NULL)` across a
later `setlocale()` call. C99 7.11.1.1p8 only guarantees that string until the
next `setlocale()`, and on UCRT it points into an internal buffer that gets
rewritten — so the restore passes a stale pointer and UCRT's `mbstowcs()` scans
for a NUL through freed memory. It usually finds one harmlessly, which is why
a fully deterministic test suite failed only ~1% of the time, always inside
ucrtbase and never near syntran's own code. `__wrap_setlocale()` in
`src/c/isocline_wrap.c` copies query results into storage we own.

Also pass `-Wl,--stack,67108864`: a PE reserves only 2 MB of stack (Linux gives
8 MB and grows), and syntran's recursive parser/deep-copy/evaluator can exhaust
that on deeply nested input. Robustness only — it does *not* fix the SIGSEGV.

CMake sets both automatically (see `CMakeLists.txt`); for fpm you must pass
them yourself. Note fpm 0.12-alpha **silently ignores `--link-flag`** — use
`--flag`.

## Running Tests

### Short tests (quick)
```bash
fpm test test --profile debug      # FPM: run short tests only
```

Commands like `fpm test` and `fpm run` automatically invoke `fpm build`, so
there's no need to manually build first.

### Running a subset of test groups
The test driver takes `--only <substr>` / `--skip <substr>` to filter groups by
name (the part after `unit_test_`), and `--repeat <n>` to run the selected set
`n` times in one process. Useful for isolating a rare/flaky failure without
looping the whole suite:
```bash
./build/v1/test/test.exe --only repl_fns --repeat 5000
./build/v1/test/test.exe --skip repl_fns
```

### Running a single test file
Syntran script tests are in `src/tests/test-src/` organized by category. To run one:
```bash
fpm run --profile debug -- src/tests/test-src/modules/test-01.syntran
```

### Testing the Fortran backend
`unit_test_transpile` in `src/tests/test.f90` only checks the generated source.
To compile and run every test program and sample through both backends and diff
their output, which needs a Fortran compiler:
```bash
bash utils/test-transpile.sh                  # uses build/v1/bin/syntran and gfortran
bash utils/test-transpile.sh --only 'fns/'    # a subset, by path regex
bash utils/test-transpile.sh --update-pass-list  # after coverage grows
```
Every file in `utils/transpile-pass.txt` must keep passing.

### Testing the native (LLVM) backend
`unit_test_native` only checks diagnostics and the LLVM IR.  To compile and link
every test program and sample and diff their output against the interpreter's,
which needs a syntran built with LLVM and a C compiler:
```bash
bash utils/test-transpile.sh --backend native build/native/bin/syntran
bash utils/test-transpile.sh --backend native --update-pass-list build/native/bin/syntran
```
Every file in `utils/native-pass.txt` must keep passing.  Pass `--ref` with an
FMA-free interpreter, as for the Fortran backend.  Add `--long` to test the
programs that `src/tests/long.f90` runs instead, which are listed in
`utils/native-pass-long.txt` (set `TIMEOUT=<seconds>` if the interpreter is slow).

### Testing AoC solutions
`external/aoc-syntran` is a submodule of [aoc-syntran](https://github.com/JeffIrwin/aoc-syntran),
my Advent of Code solutions from 2015 on.  CI runs all of them with `--jit`, using
the release binary that ships on each OS (the longest test set, so not in a debug
build): in the `build-rocky` job, inside the image that `Dockerfile.rocky` builds,
and in the `test-os` job on Windows and macOS, right after the "Pack" steps.  The inputs are
gpg-encrypted, so CI needs the `AOC_GPG_PASSPHRASE` secret
(`gh secret set AOC_GPG_PASSPHRASE`), and the step fails if it's missing.  A day
passes if it exits 0, and the years that pass `-a` (`--assert`) also check their
own answers.  To run them locally you need a syntran built with LLVM, preferably
a release build, and the passphrase in your environment:
```bash
cd external/aoc-syntran
bash decrypt-everything.sh            # writes input.txt etc., which are gitignored
SYNTRAN=$PWD/../../build/native/bin/syntran bash run.sh
```
The submodule pins a commit of aoc-syntran, so a change to syntran that breaks
a solution must fix aoc-syntran and bump the pin in the same commit:
`git -C external/aoc-syntran pull && git add external/aoc-syntran`.

### Testing README.md code blocks
`README.md`'s code blocks are annotated with hidden `<!-- syntran-begin -->`
markers and run in CI to catch doc drift. Requires a build at
`build/Debug/syntran` (or pass a path explicitly):
```bash
bash utils/test-readme.sh              # check all blocks against their hidden expected output
bash utils/test-readme.sh --update     # regenerate the hidden expected output from actual behavior
```

### Testing the syntran lexer port
`compiler/lex.syntran` is a port of `src/lex.f90` to syntran, the first piece of
the eventual self-hosted compiler (see `doc/compiler-roadmap.md`).  The hidden flag
`syntran --lex file.syntran` dumps the Fortran lexer's raw tokens, one per line
(`lex_dump()` in `src/lex.f90` has the format), and `compiler/lex-dump.syntran`
dumps the port's in the same format.  To diff them over every file in
`src/tests/test-src`, which needs no Fortran compiler:
```bash
bash utils/test-lex.sh build/v1/bin/syntran                  # on the VM
bash utils/test-lex.sh --native build/native/bin/syntran     # compiled, much faster
bash utils/test-lex.sh -v build/v1/bin/syntran samples/      # more files, with diffs
```
The port copies the Fortran lexer's quirks on purpose, e.g. a 2-char operator
has the `pos` of its last char, so a mismatch is a bug in one or the other.  The
`pos` in the dump is 0-based.  `src/tests/test-src/errors/lex-edge-cases.syntran`
is a corpus of odd literals and operators.

### Testing the syntran parser port
`compiler/parse.syntran` is the port of the front end in `src/parse*.f90`, in
milestones (see `doc/compiler-roadmap.md`), and it's tested the same way as the
lexer.  The hidden flag `syntran --parse file.syntran` dumps the Fortran parser's
typed tree and its fn/struct/enum tables (`parse_dump()` in `src/types_ops.f90`
and `parse_dump_str()` in `src/syntran.f90` have the format), and
`compiler/parse-dump.syntran` dumps the port's.  With any diagnostic, only the
`diag <code>` lines are dumped.  Unlike the lexer, the port is unfinished, so
only the files in `utils/parse-pass.txt` have to match, and the list grows with
each milestone.  A non-zero exit of either parser is a `CRASH` for any file, in
the list or not, and `--update-pass-list` won't write the list while there is a
regression or a crash.  Slots, fields, and params in the dump are 0-based:
```bash
bash utils/test-parse.sh -v build/v1/bin/syntran                 # on the VM, ~2 min
bash utils/test-parse.sh --native build/native/bin/syntran       # compiled
bash utils/test-parse.sh --update-pass-list build/v1/bin/syntran # after the port grows
```
A new child member of `syntax_node_t` has to be added to `dump_node()` in
`src/types_ops.f90`, or exempted in `utils/check-node-sync.sh`, which CI runs.

### Syntran code style
`doc/style.md` is the style guide for syntran source: tabs, Allman braces,
`PascalCase` structs/enums, `snake_case` everything else (including enum variants),
and, for new modules, one public struct per module with a `new()` constructor
that consumers call as `module::new()` through a qualified `use`, not `use *`.
`compiler/lint.syntran` checks the mechanical rules, and it has to stay clean on
`compiler/`.  CI runs it, and so can you, without a Fortran compiler:
```bash
bash utils/lint.sh build/v1/bin/syntran                  # compiler/ by default
bash utils/lint.sh build/v1/bin/syntran path/to/file.syntran
```

### Toolchain manager (`synup`)
`synup` is a second executable (`src/synup/synup_main.f90`, in `fpm.toml` and
`CMakeLists.txt`) that installs versions of syntran side by side under
`~/.syntran` and links one as `bin/syntran`.  It ships in every release zip, and
`syntran --update` runs `synup update`.  User docs are in `doc/synup.md`.

In fpm, it's a `[[test]]`, not an `[[executable]]`, so that a bare `fpm run`
(which the README documents) still runs only syntran.  fpm has no default app:
it runs every executable, and passes your args to all of them.  The catches:
- Its main is in `src/synup/` and not `src/`, because fpm makes an app of every
  `program` in an executable's `source-dir`, and syntran's is `src`
- `fpm install` skips tests, so a release install needs `--test --testdir bin`,
  and then deletes the extra test programs.  Without `--testdir`, it's at
  `<prefix>/test/synup`, next to `test` and `long`
- `fpm test` runs it with no args, so that has to print help and succeed
```bash
fpm install --test --testdir bin --prefix ./build/rel/
rm ./build/rel/bin/test ./build/rel/bin/long          # test.exe, long.exe on Windows
```
- `src/synup.f90` - `syntran__synup_m`, all of the logic.  It's in the library,
  so `unit_test_synup` can test the pure parts.  It's `private` by default, so
  add a name to its `public` list to test it
- `src/c/synup.c` - platform asset name, path of the running exe, SHA-256, and
  the file system calls.  **Spaces, not tabs**, lines under 100 chars
- Downloads shell out to the system `curl`, never libcurl.  A download is
  refused unless `SHA256SUMS` has a matching line for its zip

**Lasting contract:** every synup that has shipped fetches these forever, so
never rename them: the assets `syntran-linux.zip`, `syntran-macos.zip`,
`syntran-windows.zip`, and `SHA256SUMS`, at
`releases/latest/download/<asset>` and `releases/download/<tag>/<asset>`, with
bare-version tags (`1.7.0`) and the rolling `dev-latest` tag.

A new release needs its `SHA256SUMS` uploaded after the zips:
```bash
bash utils/release-sums.sh syntran-linux.zip syntran-macos.zip syntran-windows.zip
gh release upload <tag> SHA256SUMS
```
CI's `publish-dev` job does this for the `dev-latest` prerelease on every green
push to `dev`.

`SYNUP_HOME` and `SYNUP_RELEASES_URL` (e.g. `file:///...`) override the install
dir and the release URL, which is how `utils/test-synup.sh` runs every command
offline.  It's run by CI on ubuntu and macOS, not Windows yet:
```bash
bash utils/test-synup.sh path/to/synup
fpm test test --profile debug -- --only synup
```

Gotchas in `src/synup.f90`:
- A function that runs a command (`run_cmd()` flushes stdout) must not be called
  inside a `write` to stdout, or it deadlocks on the unit.  Assign it first
- Fortran evaluates the operands of `.or.` in any order and without
  short-circuiting, so `make_dir(a) .or. make_dir(a//'/b')` can try the child
  first.  Do dependent steps one at a time
- Windows can't replace a running exe, but it can rename it, so
  `replace_exe()` renames to `.old` there, and every command deletes the leftover
- `execute_command_line()` on Windows needs the whole command wrapped in an
  extra pair of quotes when it starts with one

### Fuzzing
`utils/fuzz.c` mutates the `.syntran` files under `src/tests/test-src`,
`samples`, and `src/tests/long` (token-aware, so most mutants get past the
lexer) and runs each one through `syntran -q`.  The contract is that any input
either runs or stops with a coded `E##`/`R##` diagnostic.  It reports a crash for
a signal, a timeout, an internal error (`I##` or `Fatal error`), or a Fortran
runtime error.  It is plain C, outside the fpm build:
```bash
fpm install --profile debug --flag "-DSYNTRAN_BOUNDS_CHECK -fcheck=all -g" --prefix ./build/fuzz/
cc -O2 -Wall -o build/fuzz/fuzz utils/fuzz.c
build/fuzz/fuzz --time 300 --jobs 4           # or --iters 5000; --seed N to reproduce
build/fuzz/fuzz --reduce build/fuzz/crashes/<signature>.syntran    # writes <file>.min
```
It saves one reproducer and its output per crash signature to
`build/fuzz/crashes/`, and exits 1 if it found a signature that isn't already
there (delete that directory to re-check a fixed bug).  Each run is capped at
2 GB of address space on Linux, since a valid mutant like `[0; 2147483647]` wants
8 GB; `--mem-mb <n>` changes it.  A signature is the kind of
failure plus where: the Fortran `At line N of file F` when there is one, else the
frame addresses (their low 12 bits, since macOS backtraces have no symbol names).
So the same bug can still show up under a few signatures, and a fixed one stops
showing up.  A seed file's `use`/`#include` paths don't resolve, since mutants run
from `build/fuzz/work/<n>/`.

For each bug that the fuzzer finds, reduce the reproducer, fix the bug, and add
the `.min` file to `src/tests/test-src/` as a regression test, as described
above.  Don't take ASan/UBSan builds as the default: they make the build very
slow, and UBSan only instruments the C files.  `.github/workflows/fuzz.yml` runs
it nightly, separately from `main.yml` so that a find never blocks a push.

### Evaluating a script as a command-line string
To run a syntran script without saving it as a file:
```bash
fpm run -- -c 'println(\"hello world\");'
```
FPM requires escaping quotes within command arguments.

## Architecture Overview

The interpreter follows a lexer → parser → bytecode compiler → VM pipeline, implemented in Fortran modules:

### Core Pipeline
- `src/lex.f90` - Lexical analysis (tokenization)
- `src/parse.f90` - Main parser orchestration
- `src/parse_*.f90` - Parser submodules:
  - `parse_expr.f90` - Expression parsing
  - `parse_control.f90` - Control flow (if/while/for) and **module imports (`use` statements)**
  - `parse_fn.f90` - Function declarations
  - `parse_struct.f90` - Struct/enum declarations, struct instances, and methods
  - `parse_array.f90` - Array syntax
  - `parse_misc.f90` - Preprocessing (`#include`) and other misc parsing
- `src/compile.f90` - Bytecode compiler orchestration
- `src/compile_ctrl.f90` - Bytecode emission for control flow, assignment, and calls
- `src/bytecode.f90` - Bytecode program/instruction representation
- `src/vm.f90` - Bytecode VM orchestration
- `src/vm_exec.f90` - VM instruction dispatch loop
- `src/vm_intr.f90` - VM intrinsic function dispatch
- `src/runtime.f90` - Evaluation-time runtime state (`state_t`) shared by the VM
- `src/runtime_*.f90` - Runtime submodules for VM fallback paths (struct/dot-chain
  member access, array-slice subscripting, array-literal construction, for-loop
  iteration):
  - `runtime_array.f90` - Array/member subscripting and slicing
  - `runtime_control.f90` - Slice-LHS/compound assignment and array-literal construction
  - `runtime_expr.f90` - Name-expression subscripting
- `src/types.f90` - Fortran type definitions
- `src/types_*.f90` - Types submodules:
  - `types_copy.f90` - Deep copy procedures for types
  - `types_dict.f90` - Dictionary/ternary tree operations
  - `types_node.f90` - Syntax node/expression builders
  - `types_ops.f90` - Type checking and operator utilities
- `src/value.f90` - Runtime value representation
- `src/errors.f90` - Syntran error messages
- `src/intr_fns.f90` - Intrinsic (built-in) function interfaces
- `src/intr_fns_*.f90` - Intrinsic function submodules:
  - `intr_fns_array.f90` - Array functions (size, len, etc.)
  - `intr_fns_io.f90` - I/O functions (open, read, write, etc.)
  - `intr_fns_math.f90` - Math functions (abs, exp, log, etc.)
  - `intr_fns_minmax.f90` - Min/max functions
  - `intr_fns_trig.f90` - Trigonometric functions (sin, cos, etc.)

### Fortran Backend (`--transpile`)
An alternative to `compile*.f90` + `vm*.f90`: instead of bytecode, the typed AST
is lowered to the source of a standalone modern Fortran program that the user
compiles themselves.  User docs are in `doc/transpile.md`.
- `src/transpile.f90` - Interface module (`syntran__transpile_m`), the public
  `transpile_t` options/result type, and the internal `emitter_t` state
- `src/transpile_*.f90` - Submodules:
  - `transpile_expr.f90` - Expressions, array literals, subscripts, intrinsics
  - `transpile_stmt.f90` - Statements, fns, and assembling the whole program
  - `transpile_rt.f90` - **Generated**, never edit by hand.  The runtime below
    as a table of string literals.  Regenerate with `bash src/gen_transpile_rt.sh`
- `src/rt/syntran_rt.f90` - The runtime module that is embedded in every
  transpiled program (number formatting, ranges, etc.).  It is also compiled
  into the library, so it is checked on every build.  **Use spaces, not tabs**,
  because it ends up in users' programs, and keep lines under 100 chars
- Diagnostic `E115` is reported for constructs that aren't supported yet

A variable keeps its own name, unless it shadows another or could clash with a
Fortran intrinsic or a name that the emitter makes up.  Those are emitted as
`<name>_g<id>`/`<name>_l<id>` using the parser's unique slot ids, so syntran's
block scopes and shadowing never need to be mirrored in Fortran (see
`collect_names()` and `bare_ok()`).  The emitter reads types from `node%val`, and
fn signatures from `state%fns`, since a `fn_declaration` node only has slots.

### Native Backend (`--native`)
Compiles the typed AST straight to machine code with LLVM, calling the LLVM-C
API from Fortran.  User docs are in `doc/native.md`.
- `src/llvm_c.f90` - bind(c) interfaces to the LLVM-C functions that are used.
  Add an interface here when the emitter needs another one
- `src/native.F90` - `syntran__native_m`: the public `native_t` options/result
  type, the `nemitter_t` state, and `native_tree()`.  Everything that calls LLVM
  is under `#ifdef SYNTRAN_LLVM`
- `src/c/llvm_shim.c` - initializes LLVM for the host architecture, because
  `LLVMInitializeNativeTarget()` is a static inline in a header
- `src/rt/syntran_crt.c` - the C runtime that compiled programs link with
  (`sy_` prefix).  **Use spaces, not tabs**, and keep lines under 100 chars.
  It is also compiled into the library, so `unit_test_native_rt` can compare its
  float formatting to `src/rt/syntran_rt.f90`
- `src/native_rt.f90` - **Generated**, never edit by hand.  The C runtime as a
  table of string literals.  Regenerate with `bash src/gen_native_rt.sh`
- Diagnostic `E130` is reported for constructs that aren't supported yet

A value that the emitter builds is an `nval_t`.  A `str` is a heap object that
whoever made it owns, until it is moved into a variable or `dispose()`d.

### Math Operations (auto-generated)
Binary arithmetic operations are generated from `src/math_bin_template.f90` via `src/gen_math.sh`:
- `src/math_bin_add.f90`, `math_bin_subtract.f90`, `math_bin_mul.f90`, `math_bin_div.f90`, `math_bin_pow.f90`

Bitwise operations follow the same pattern in `src/math_bit_*.f90`.

### Utilities
- `src/bool.f90` - Boolean operations
- `src/consts.f90` - Constants
- `src/math.f90` - Math module (coordinates generated math files)
- `src/utils.f90` - Utility functions

### Public API
- `src/syntran.f90` - Public API module (REPL, file interpretation)
- `src/core.f90` - Internal core module that pulls together all other modules

### Entry Points
- `src/main.f90` - CLI entry point
- `src/app.f90` - Application-level utilities (argument parsing, etc.)
- `src/synup.f90`, `src/synup/synup_main.f90`, `src/c/synup.c` - The `synup` toolchain
  manager, see "Toolchain manager" above

## Module Import System

Module paths in `use` statements are resolved relative to the current source file's directory. Supported syntax:
- `use mymodule;` - Qualified import from same directory
- `use mymodule::*;` - Unqualified glob import
- `use math/vectors;` - Subdirectory import (becomes `math::vectors::fn`)

## Key Source Locations

- `src/tests/test.f90` - Unit test orchestrator
- `src/tests/long.f90` - Long/integration tests
- `src/tests/core.f90` - Test core module
- `src/tests/test-src/` - Syntran script unit test files by category
- `src/tests/long/aoc/` - Longer integration tests
- `compiler/` - The self-hosted compiler, written in syntran.  The lexer and the
  core of the parser so far, and the linter for its style
- `samples/` - Example syntran programs (not tested in CI)
- `doc/README.md` - Table of contents for the other doc pages
- `doc/intrinsics.md` - Intrinsic function documentation

New features and bug fixes should have unit tests.
Short syntran program strings can be evaluated directly in `test.f90`.
Anything longer than a few lines can go in a `*.syntran` file under `test-src`, but it needs to be called using `interpret_file()` in `test.f90`.

## Adding a New Error Code

Every diagnostic (`EC_*`/`RC_*`/`IC_*`/`WC_*` in `src/errors.f90`) needs all of
the following — skipping the doc/example steps is an easy mistake since the
build and tests pass without them:

1. Add the `E##`/`R##`/`I##`/`W##` constant (next unused number in that
   letter's series; never renumber or reuse a retired code) and its
   `codes%push(...)` registration in `src/errors.f90`.
2. Add the `err_*()` constructor function alongside the other `err_*`
   functions in `src/errors.f90`.
3. Emit it from the relevant parse/eval site.
4. Add a reproduction file at
   `src/tests/test-src/errors/E##-kebab-name.syntran` (mirror the existing
   files there), placed on a non-trivial line/column if possible so location
   tests actually exercise line/col arithmetic.
5. Document it in `doc/errors.md`: a `### E## -- kebab-name` heading, a
   one-line description, and an `[Example](../src/tests/test-src/errors/E##-kebab-name.syntran)`
   link to the file from step 4.
6. Add coverage in `src/tests/test.f90`: a `diag_has_code(...)` (and usually
   `diag_count_code(...) == 1`) row in `unit_test_error_codes`, plus a
   `diag_loc_ok(...)` row in `unit_test_error_locations` for the reproduction
   file's exact line/col/caret-width (run the file through the built
   interpreter to read the real caret before writing the assertion).
