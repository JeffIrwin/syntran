# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Syntran

Syntran is an array-oriented programming language with an interpreter written in Fortran. It's similar to MATLAB but with curly braces, type checking, and zero-indexed arrays. The name is a portmanteau of "syntax translator" (like FORTRAN = formula translator).

## Build Commands

Two build systems are supported: CMake and FPM (Fortran Package Manager).

### CMake (primary)
```bash
./build.sh           # Debug build (default)
./build.sh release   # Release build
```
Binary output: `./build/Debug/syntran` or `./build/Release/syntran`

### FPM
```bash
fpm build                   # Debug build
fpm build --profile release # Release build
fpm run                     # Build and run interpreter
fpm run -- file.syntran     # Run a syntran file
```

## Running Tests

### Short tests (quick)
```bash
fpm test test       # FPM: run short tests only
./build/Debug/test  # CMake: run test executable
```

### Long tests (Advent of Code solutions)
```bash
fpm test long       # Takes longer, tests AOC solutions
```

### Running a single test file
Syntran script tests are in `src/tests/test-src/` organized by category. To run one:
```bash
./build/Debug/syntran src/tests/test-src/modules/test-01.syntran
```

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
- `src/types.f90` - Type system definitions
- `src/value.f90` - Runtime value representation

### Math Operations (auto-generated)
Binary arithmetic operations are generated from `src/math_bin_template.f90` via `src/gen_math.sh`:
- `src/math_bin_add.f90`, `math_bin_subtract.f90`, `math_bin_mul.f90`, `math_bin_div.f90`, `math_bin_pow.f90`

Bitwise operations follow the same pattern in `src/math_bit_*.f90`.

### Public API
- `src/syntran.f90` - Public API module (REPL, file interpretation)
- `src/core.f90` - Internal core module that pulls together all submodules

### Entry Points
- `src/main.f90` - CLI entry point
- `src/app.f90` - Application-level utilities (argument parsing, etc.)

## Module Import System

Module paths in `use` statements are resolved relative to the current source file's directory. Supported syntax:
- `use mymodule;` - Qualified import from same directory
- `use mymodule::*;` - Unqualified glob import
- `use math/vectors;` - Subdirectory import (becomes `math::vectors::fn`)

**Note:** Parent directory references (`../`) are NOT supported for `use` statements, but ARE supported for `#include()` directives.

## Key Source Locations

- `src/tests/test-src/` - Syntran script test files by category
- `src/tests/long/aoc/` - Advent of Code solutions used as integration tests
- `samples/` - Example syntran programs (not tested in CI)
- `doc/README.md` - Intrinsic function documentation
