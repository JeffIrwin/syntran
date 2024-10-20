
<!-- # Variadic and polymorphic functions -->
# Special functions

Some of the intrinsic functions have special properties:
- variadic
- polymorphic
- elemental

[Variadic](https://en.wikipedia.org/wiki/Variadic_function) intrinsic functions are indicated with `...`.

[Polymorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science))
function parameters are indicated with special types.  Rank-polymorphic array
function parameters are indicated with `any_rank`.

[Elemental](https://fortran-lang.org/en/learn/best_practices/element_operations/)
functions are tagged with `#elemental`.  Elemental functions can take either a
scalar argument or an array argument.  When given an array argument, the result
is also an array, with the same rank and size as the argument, with values as if
the function had been called element-wise on each member of the array.

These are the special polymorphic types used in documentation:
- `any`: any unlimitted primitive, built-in syntran type
- `any_num`: any numeric type, i.e. `i32`, `i64`, `f32`, or `f64`
- `any_float`: any floating-point type, i.e. `f32` or `f64`

All of these special properties are special cases for intrinsic functions.  For
now at least, user-defined functions cannot be variadic, polymorphic, or
elemental.

# List of intrinsic functions

## `all`
```rust
fn all(mask: [bool; any_rank])
```

Return `true` if every element of `mask` is `true`, including the case where
`mask` is empty.  Otherwise, i.e. if at least 1 element of `mask` is `false`,
return `false`.

Related functions: [`any`](#any)

## `any`
```rust
fn any(mask: [bool; any_rank])
```

Return `true` if at least 1 element of `mask` is `true`.  Otherwise, i.e. if
all elements of `mask` are `false` or `mask` is empty, return `false`.

Related functions: [`all`](#all)

## `char`
```rust
fn char(i: i32): str
```

Perform an ASCII table lookup and return the single-character string with ASCII
index `i`.  For example, `char(65)` is `"A"`.  The inverse of the ASCII lookup
is [`i32`](#i32), which is also overloaded to do other casting operations.

Related functions: [`i32`](#i32)

## `close`
```rust
fn close(file_handle: file)
```

Close a `file_handle` created by [`open`](#open)

Related functions: [`open`](#open), [`writeln`](#writeln)

## `count`
```rust
fn count(mask: [bool; any_rank]): i64
```

Return how many elements of `mask` are `true`.  If `mask` is empty, return `0`.
Not to be confused with [`size`](#size).

Related functions: [`any`](#any), [`all`](#all)

## `eof`
```rust
fn eof(file_handle: file): bool
```

Has the end of file (EOF) been reached yet while reading from `file_handle`?  The return value is `false` until and including the last line is read and does not become `true` until reading *past* the EOF is attempted, for which [`readln()`](#readln) will return an empty string

Related functions: [`close`](#close), [`readln`](#readln), [`open`](#open)

## `exit`
```rust
fn exit(status: i32)
```

Terminate the process normally,
return the system exit status `status`, and
perform the regular cleanup for terminating programs.

By the C convention, use status `0` for success and any other value for failure.

<!-- Abort execution and return the system exit status `status`. -->

## `exp`
```rust
fn exp(x: any_float): any_float
```

Compute the base _e_ exponential of `x`

## `i32`
```rust
fn i32(a: any_num): i32
#elemental
```
<!-- TODO: not just `any_num` type. Strings are allowed, but it's a runtime
error if you try a string of 2 or more chars. -->

Explicitly cast any numeric type to 32-bit integer `i32`. The `i32()` fn also
casts single-character strings to their ASCII values, e.g. `i32("A")` returns
`65`.  The inverse ASCII lookup is [`char`](#char).  To read a decimal number
from a string, use `parse_i32()` instead.

The `i32` function is _elemental_, i.e. it can operate on both scalars and
arrays.

Other non-numeric types (`bool`, `file`) cannot be cast to `i64`.

Related functions: [`char`](#char)

## `i64`
```rust
fn i64(a: any_num): i64
#elemental
```

Explicitly cast any numeric type to 64-bit integer `i64`.
<!-- TODO For the non-numeric type `str`, use [`parse_i32`](#parse_i64) instead.  -->
Other non-numeric types (`bool`, `file`) cannot be cast to `i64`.

## `len`
```rust
fn len(s: str): i32
```

Get the length of a string `s`

## `max`
```rust
fn max(a0: any_num, a1: any_num, a2: any_num, ...): any_num
```

Return the argument with the largest (most positive) value.  The arguments of
`max` must be homogeneous types, i.e. `a1` must be the same type as `a0` in one
invocation.  For example, this is a parser error:
```rust
let x = max(1, 2.0);
// Error: function `max` parameter 1 `a1` requires type `i32` but was given `f64`
//   --> <stdin>:1:8
//    |
//  1 | max(1, 2.0);
//    |        ^^^ wrong argument type
```

Seperate invocations with different types are fine:
```rust
let max_int = max(1, 2);
let max_flt = max(3.0, 2.0);
```

Related functions: [`min`](#min)

## `min`
```rust
fn min(a0: any_num, a1: any_num, a2: any_num, ...): any_num
```

Return the argument with the smallest (most negative) value.  Like with `max`,
the arguments of `min` must be homogeneous types.

Related functions: [`max`](#max)

## `open`
```rust
fn open(filename: str): file
```

Open a `file` handle named `filename`

Related functions: [`close`](#close), [`writeln`](#writeln)

## `parse_f32`
```rust
fn parse_f32(s: str): f32
```

Convert from a string to a float `f32`.  For example, `parse_f32("65.4")`
returns roughly `65.4f`.

Any invalid numbers will cause a runtime error.

Note that other numeric types (`i32`) can be converted to float simply by
multiplying by `1.0f`.

Related functions: [`parse_i32`](`parse_i32`), [`str`](#str)

## `parse_f64`
```rust
fn parse_f64(s: str): f64
```

Convert from a string to a double `f64`.  For example, `parse_f64("65.4")`
returns roughly `65.4`.

Any invalid numbers will cause a runtime error.

Note that other numeric types (`i32`) can be converted to double simply by
multiplying by `1.0`.

Related functions: [`parse_f32`](`parse_f32`), [`str`](#str)

## `parse_i32`
```rust
fn parse_i32(s: str): i32
```

Convert from a string to an integer `i32`.  For example, `parse_i32("65")`
returns `65`.  This is in contrast to calling `i32()` on character arguments,
which performs an ASCII table lookup.

Any invalid numbers will cause a runtime error.  Other languages variously refer
to similar functions as `atoi()`, `stoi()`, `read()`, `parse()`, `strtol()`,
`str2int()`, etc.

Note that other numeric types (`f32`) can be implicitly cast to `i32`.

Related functions: [`parse_i64`](`parse_i64`), [`str`](#str)

## `parse_i64`
```rust
fn parse_i64(s: str): i64
```

Convert from a string to an integer `i64`.  For example, `parse_i64("65")`
returns `65`.

Any invalid numbers will cause a runtime error.

Related functions: [`parse_i32`](`parse_i32`), [`str`](#str)

## `println`
```rust
fn println(s0: any, s1: any, s2: any, ...)
```

Print to the standard output, with a newline

Related functions: [`str`](#str), [`writeln`](#writeln)

## `readln`
```rust
fn readln(file_handle: file): str
```

Read a single line from `file_handle`

Related functions: [`close`](#close), [`eof`](#eof), [`open`](#open)

## `size`
```rust
fn size(array: [any; any_rank], dim: i32): i32
```

Determine the extent of `array` along a specified dimension `dim`
<!-- , or the total number of elements in ARRAY if DIM is absent. -->

## `str`
```rust
fn str(s0: any, s1: any, s2: any, ...): str
```

Convert arguments to `str` and concatenate

Related functions: [`println`](#println), [`writeln`](#writeln), [`parse_i32`](#parse_i32)

## `sum`
```rust
fn sum(array: [any_num; any_rank]): any_num
```

Return the sum of all elements in an array.

## `writeln`
```rust
fn writeln(file_handle: file, s0: any, s1: any, s2: any, ...)
```

Write to `file_handle`, with a newline

Related functions: [`close`](#close), [`readln`](#readln), [`println`](#println), [`open`](#open)

