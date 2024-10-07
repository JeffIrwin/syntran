
# Variadic and polymorphic functions

[Variadic](https://en.wikipedia.org/wiki/Variadic_function) intrinsic functions are indicated with `...`.

[Polymorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) function parameters are indicated with special types.  Rank-polymorphic array function parameters are indicated with `any_rank`.

These are the special polymorphic types used in documentation:
- `any`: any unlimitted primitive, built-in syntran type
- `any_num`: any numeric type, i.e. `i32`, `i64`, `f32`, or `f64`
- `any_float`: any floating-point type, i.e. `f32` or `f64`

These are special cases for intrinsic functions.  For now at least, user-defined functions cannot be variadic or polymorphic.

# List of intrinsic functions

## `close`
```rust
fn close(file_handle: file)
```

Close a `file_handle` created by [`open`](#open)

Related functions: [`open`](#open), [`writeln`](#writeln)

## `eof`
```rust
fn eof(file_handle: file): bool
```

Has the end of file (EOF) been reached yet while reading from `file_handle`?  The return value is `false` until and including the last line is read and does not become `true` until reading *past* the EOF is attempted, for which [`readln()`](#readln) will return an empty string

Related functions: [`close`](#close), [`readln`](#readln), [`open`](#open)

## `exp`
```rust
fn exp(x: any_float): any_float
```

Compute the base _e_ exponential of `x`

## `i32`
```rust
fn i32(a: any_num): i32
```

Explicitly cast any numeric type to 32-bit integer `i32`. The `i32()` fn also
casts single-character strings to their ASCII values, e.g. `i32("A")` returns
`65`.  To read a decimal number from a string, use `parse_i32()` instead.

Other non-numeric types (`bool`, `file`) cannot be cast to `i64`.

## `i64`
```rust
fn i64(a: any_num): i64
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

Related functions: [`str`](#str)

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

