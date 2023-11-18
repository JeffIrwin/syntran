
# Variadic and polymorphic functions

[Variadic](https://en.wikipedia.org/wiki/Variadic_function) intrinsic functions are indicated with `...`.

[Polymorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) function parameters are indicated with `any` type.  Rank-polymorphic array function parameters are indicated with `any_rank`.

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
fn exp(x: f32): f32
```

Compute the base _e_ exponential of `x`

## `len`
```rust
fn len(s: str): i32
```

Get the length of a string `s`

## `i32`
```rust
fn i32(s: str): i32
```

Convert from a string to an integer `i32`.  Any invalid numbers will cause a runtime
error.

Note that other numeric types (`f32`) can be implicitly cast to `i32`.

Related functions: [`str`](#str)

## `max`
```rust
fn max(a0: i32, a1: i32, a2: i32, ...): i32
```

Return the argument with the largest (most positive) value

Related functions: [`min`](#min)

## `min`
```rust
fn min(a0: i32, a1: i32, a2: i32, ...): i32
```

Return the argument with the smallest (most negative) value

Related functions: [`max`](#max)

## `open`
```rust
fn open(filename: str): file
```

Open a `file` handle named `filename`

Related functions: [`close`](#close), [`writeln`](#writeln)

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

Related functions: [`println`](#println), [`writeln`](#writeln), [`i32`](#i32)

## `writeln`
```rust
fn writeln(file_handle: file, s0: any, s1: any, s2: any, ...)
```

Write to `file_handle`, with a newline

Related functions: [`close`](#close), [`readln`](#readln), [`println`](#println), [`open`](#open)

