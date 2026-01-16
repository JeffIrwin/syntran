# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Syntran

Syntran is an array-oriented programming language with an interpreter written in Fortran. It's similar to MATLAB but with curly braces, type checking, and zero-indexed arrays.

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

## Running Tests

### Short tests (quick)
```bash
fpm test test --profile debug      # FPM: run short tests only
```

Commands like `fpm test` and `fpm run` automatically invoke `fpm build`, so
there's no need to manually build first.

### Running a single test file
Syntran script tests are in `src/tests/test-src/` organized by category. To run one:
```bash
fpm run --profile debug -- src/tests/test-src/modules/test-01.syntran
```

### Evaluating a script as a command-line string
To run a syntran script without saving it as a file:
```bash
fpm run -- -c 'println(\"hello world\");'
```
FPM requires escaping quotes within command arguments.

## Architecture Overview

The interpreter follows a classic lexer → parser → evaluator pipeline, implemented in Fortran modules:

### Core Pipeline
- `src/lex.f90` - Lexical analysis (tokenization)
- `src/parse.f90` - Main parser orchestration
- `src/parse_*.f90` - Parser submodules:
  - `parse_expr.f90` - Expression parsing
  - `parse_control.f90` - Control flow (if/while/for) and **module imports (`use` statements)**
  - `parse_fn.f90` - Function declarations
  - `parse_array.f90` - Array syntax
  - `parse_misc.f90` - Preprocessing (`#include`) and other misc parsing
- `src/eval.f90` - AST evaluation/interpretation
- `src/eval_*.f90` - Evaluator submodules:
  - `eval_array.f90` - Array evaluation
  - `eval_control.f90` - Control flow evaluation
  - `eval_expr.f90` - Expression evaluation
  - `eval_fn.f90` - Function evaluation
- `src/types.f90` - Fortran type definitions
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
