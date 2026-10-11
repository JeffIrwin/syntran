# Syntran documentation

The [top-level README](../README.md) is the language guide and quick-start.  The
pages below go into more detail on specific topics.

## Language reference

- [Intrinsic functions](intrinsics.md):  signatures and descriptions of the
  built-in functions
- [Error code index](errors.md):  every diagnostic, with its code and an example
- [Include files](include-files.md):  `#include` preprocessing, which is
  superseded by [modules](../README.md#modules)

## Using syntran from other languages

- [Fortran API](api.md):  using syntran as a Fortran library
- [Transpiling to Fortran](transpile.md):  generating a standalone Fortran
  program instead of running the interpreter

## Installing and building

- [Docker](docker.md):  running syntran in a container
- [Shared libraries](shared-libs.md):  notes on the binary packages
- [Automatic source generation](src-gen.md):  which Fortran sources are
  generated, and how
