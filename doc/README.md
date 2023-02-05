
# Variadic and polymorphic functions

# Intrinsic functions

## `close`
```rust
fn close(file_handle: file)
```

Close a `file` created by [`open`](#open)

Related functions: [`open`](#open), [`writeln`](#writeln)

## `exp`
```rust
fn exp(x: f32): f32
```

Compute the base _e_ exponential of `x`

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

Open a `file` named `filename`

Related functions: [`close`](#close), [`writeln`](#writeln)

## `println`
```rust
fn println(s0: any, s1: any, s2: any, ...)
```

Print to the standard output, with a newline

Related functions: [`str`](#str), [`writeln`](#writeln)

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

Related functions: [`println`](#println), [`writeln`](#writeln)

## `writeln`
```rust
fn writeln(file_handle: file, s0: any, s1: any, s2: any, ...)
```

Write to `file`, with a newline

Related functions: [`close`](#close), [`println`](#println), [`open`](#open)

