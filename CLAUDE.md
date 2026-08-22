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

### Testing README.md code blocks
`README.md`'s code blocks are annotated with hidden `<!-- syntran-begin -->`
markers and run in CI to catch doc drift. Requires a build at
`build/Debug/syntran` (or pass a path explicitly):
```bash
bash utils/test-readme.sh              # check all blocks against their hidden expected output
bash utils/test-readme.sh --update     # regenerate the hidden expected output from actual behavior
```

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
- `samples/` - Example syntran programs (not tested in CI)
- `doc/README.md` - Intrinsic function documentation

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
