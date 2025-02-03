
![](https://github.com/JeffIrwin/syntran/workflows/CI/badge.svg)

# Syntran

<!-- [![](doc/imgs/logo-syntran-128p.png)](https://github.com/JeffIrwin/syntran/blob/main/samples/logo.syntran) -->
[![](doc/imgs/logo-syntran-128p.png)](samples/logo.syntran)

⚠️ Syntran is alpha and I don't recommend using it for anything serious.  You will discover bugs, missing features, many pain points in general; and later updates will be incompatible.

## [Syntax translator](https://www.practo.com/medicine-info/syntran-100-mg-capsule-18930)

An interpreter written in Fortran, I guess

> Jerry: Fungicide. I mean what could she have?
>
> Elaine: I don't know.
>
> Kramer: Fungus.

This began as a sandbox for me to play in as I followed along with [Immo Landwerth's _building a compiler_ series](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y), but it has since diverged.  Syntran has morphed into an interpretted, array-oriented language -- basically MATLAB but with curly braces, type checking, and zero-indexed arrays.

## Install binary

To run syntran, you can either install a binary or build it from the Fortran source code.  To build it from source, [see the next section](#build-the-interpreter-from-source).

Download the binary from the latest github release for your operating system:

### Linux (most* distros)
```
curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-linux.zip"
unzip syntran-linux.zip
chmod +x ./syntran
./syntran -h
```

The binary asset `syntran-linux.zip` works on most distros:  arch, debian, ubuntu, fedora, kali, and ubuntu.  It does not work on rocky or alma due to glibc incompatibilities.

### Linux (rocky and alma)
```
curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-rocky.zip"
unzip syntran-rocky.zip
chmod +x ./syntran
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
./syntran -h
```

Note that there is an extra step to set the `LD_LIBRARY_PATH` here, because the
rocky build image cannot statically link libgfortran.  This works on ubuntu and
probably other distros too, or you can just use the more universal asset above.

### Windows
```
curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-windows.zip"
tar -xvf syntran-windows.zip
.\syntran -h
```
Or, download and unzip it however you like.

More or less any terminal on Windows should work, but [Windows Terminal](https://aka.ms/terminal) supports colors and arrow key history better than Windows CMD.  A git bash shell is even better.  Also see the [section on arrow keys](#arrow-keys-and-command-history).

<!-- mac works too but i don't want to encourage apple usery -->

### Path

Feel free to add the directory to your PATH environment variable, or type the full path.

Whenever you see something like `./build/Debug/syntran` or `fpm run` in the rest of this documentation, replace that with `/path/to/syntran` or `C:\path\to\syntran.exe` appropriately, depending on your operating system and where you downloaded the binary.

## Build the interpreter from source

Using cmake:

```
./build.sh
```

If you installed syntran as a binary, you can skip this section.

A [Fortran compiler](https://fortran-lang.org/en/compilers/) and either [CMake](https://cmake.org/download/) or [FPM](https://fpm.fortran-lang.org/index.html) are required.  Supported compilers are gfortran 10 through 14, or Intel 2023.1 through 2024.  Also check `matrix.gfortran` in the [github actions workflow ](.github/workflows/main.yml) to see which compilers are regularly tested in CI/CD.  For performance, gfortran is recommended over Intel.

Two independent build systems are provided for syntran.  You can either use cmake, which is run by `build.sh` as shown above, or you can use the Fortran Package Manager `fpm`:

```
fpm build
```

Other `fpm` commands are available, such as `fpm test`, `fpm run`, `fpm
install`, etc.  Most of the example commands in this documentation will assume
that cmake was used, but there is usually an fpm alternative.

As an alternative to installing dependencies yourself, you can run syntran in a
docker container using the included [Dockerfile](Dockerfile).  See the
[instructions for docker](doc/docker.md).

You shouldn't need to worry about this, but note that some of the source code is
auto-generated.  [Go here for details](doc/src-gen.md).

## Run

Start the interpreter.  Adjust the path if you built with fpm or installed a binary:

```
./build/Debug/syntran
```

Then enter arithmetic expressions like `1 + 2 * 3;` in the interpreter.  Semicolons are required at the end of statements!

<!-- cpp is the closest match I can find for markdown syntax highlighting -->
```cpp
syntran$ 1 + 2 * 3;
7
syntran$ (1 + 2) * 3;
9
```

Expressions are evaluated immediately and the result is printed to the console.  In the rest of this documentation, we will hide the `syntran$` prompt and show the result as a `// comment`, so you can copy and paste code blocks straight into the interpreter.

Use two asterisks for exponent powers, like Fortran and Scilab:

```cpp
5 ** 2;
// 25
```

There's no need to [import `math.h`](https://en.cppreference.com/w/c/numeric/math/pow) and call the `pow()` function!

## Variables, Booleans, and type checking

Variable declarations use the [`let` keyword](https://doc.rust-lang.org/std/keyword.let.html) as in Rust.  This is also similar to JavaScript, except there is no `var` keyword.  Variables are mutable.

Integer `i32` and `i64`, float `f32` and `f64`, string `str`, and Boolean `bool` types are supported.  Attempting operations on the wrong types yields an error, e.g. trying to add a bool or use logical `and` on an int or float.

```cpp
let foo = 1;
let bar = 2;
let baz = 4;

baz = 3;

foo + bar * baz;
// 7

let p = true;
let q = false;

p or q;
// true

not q and foo + bar == baz;
// true

foo and p;
// Error: binary operator `and` is not defined for types i32_type and bool_type
//   --> <stdin>:1:5
//    |
//  1 | foo and p;
//    |     ^^^ bad types for this binary operator
```

Logical keywords `true`, `false`, `not`, `and`, and `or` are like Fortran's (e.g. `.true.`) but without the dots.  Note that they are lower case-sensitive, unlike Python (e.g. `True`).

### Floating point types

The default floating point type is `f64`, i.e. double precision.  To make an `f32` float literal, append an `'f32` suffix, like in the C language.  For example:
- the literal `1.0` is of type `f64`
- the literal `1.0'f32` is of type `f32`
- the literal `6.0221408e+23` (Avogadro's number) is of type `f64`
- the literal `6.0221408e+23'f32` is of type `f32`

When float types are mixed in an arithmetic expression, the result is casted up to the higher precision of the operands.  For example:
```rust
let x = 1.0 + 1.0'f32;
```
Here, the variable `x` is of type `f64`.

There is also an option to denote `f32` types with a simple `f` suffix like in
C, e.g. `1.0f`.  However, the full type name after an apostrophe is preferred,
as this extends to other types.

### Integer types

There isn't really a _default_ integer type in the same way that `f64` is the
default float type.

Small integer literals, in an absolute sense of being close to 0, are `i32`.
Larger integer literals outside the `i32` range are automatically inferred as
`i64`.  For example:
- the literal `2000000000` is of type `i32`
- the literal `3000000000` is of type `i64`
- the literal `-2000000000` is of type `i32`
- the literal `-3000000000` is of type `i64`

Small literals can be explicitly cast up to `i64` either by using a type suffix
or by using the [`i64()` function](doc/README.md#i64):
- `42'i64` is of type `i64`
- `i64(42)` is a function call with a literal argument `42` of type `i32`, and
  the function returns type `i64`

Be careful with integer overflow.  For example, `2000000000 + 2000000000` will
overflow silently and without warning.  If you expect large values, cast at
least one of the operands up to `i64`.

## Comments

Only single-line `// comments` are supported.  There are _no_ multi-line `/*comments*/`.

## Arrow keys and command history

In many shells such as `bash`, the up and down arrow keys can be used to scroll through the command history.  For example, hit the up arrow key and then ENTER to repeat the previous command, or hit the up arrow key twice to go two commands back.

Arrow keys do _not_ work by default in syntran running within bash, but there is a simple and powerful workaround with `rlwrap`.  If you run `syntran` by itself, it processes text in cooked mode:

```
./build/Debug/syntran
```

Cooked mode means that `syntran` does not read or process any text until after you press the ENTER key.  Further, when you use an arrow key, you will see escape sequences like this:

```
^[[A
```

These keypresses may appear like the ANSI escape sequence above or other garbled text.

To overcome this, install and run `rlwrap` with `syntran`:

```
sudo apt install rlwrap
rlwrap ./build/Debug/syntran
```

With `rlwrap`, the arrow keys work as expected within the `syntran` interpreter.  Plus, you get `rlwrap`'s other powerful features, like Ctrl+R for history search and saved history across separate invocations of `syntran`.

Also see [`run.sh`](run.sh), which checks if you have `rlwrap` installed and then starts `syntran` appropriately.  You can make a bash function or alias in your `~/.bashrc` file to help with this:

```
alias syntran="rlwrap /path/to/bin/syntran"
```

The basic arrow key history should work in Windows terminal and Windows cmd out of the box, but running `rlwrap` in a Linux environment within Windows can still be advantageous.

### rlwrap workaround

As of rlwrap 0.43, there is a bug where it hides the `syntran$ ` prompt.  To workaround it, add this to your `~/.inputrc`:

```
set enable-bracketed-paste off
```

## Syntax highlighting

I do not plan on writing any syntax highlighting plugins.

The easiest way to get highlighting is to have your editor treat syntran as a similar language.  Rust is a pretty good match with keywords like `let`, `fn`, and type names `i32`, `f64`, etc.  C++ is also an ok match (it has `and` and `or` keywords).

For neovim, add this line to your `~/.config/nvim/ftdetect/syntran.lua` file:

```lua
vim.cmd.autocmd("BufRead,BufNewFile *.syntran set filetype=rust")
```

## Command-line usage

### Saving scripts in a file

As programs get longer and more complicated, it becomes difficult to enter them into the interactive interpreter.  To interpret a whole file, provide it as a command line argument:

```
./build/Debug/syntran samples/primes-1.syntran
```

<!--
Note: global block statement is not required as of 0.0.13.  Multiple statements (and functions) are parsed at the global scope.

Make sure to wrap the entire script in a main block with braces `{}`.  The global block `{}` is not required when interactively using the interpreter because it parses and evaluates one statement at a time.  However, if you forget the global block `{}` in a script file, only the first statement will be parsed and any trailing junk statements will be unexpected.
-->

### Other command-line arguments

Run `syntran -h` to see a comprehensive listing of syntran command-line arguments:

```
 syntran 0.0.48
 https://github.com/JeffIrwin/syntran

 Usage:
     syntran <file.syntran> [--fmax-errors <n>] [-i | --interactive]
     syntran
     syntran -c <cmd> | --command <cmd>
     syntran -h | --help
     syntran --version

 Options:
     -h --help           Show this help
     --version           Show version and build details
     -c --command <cmd>  Run program passed in as string
     -i --interactive    Interpret a file then start an interactive shell
     --fmax-errors <n>   Limit max error messages to <n> [default: 4]
```

Although the semantic version is always shown, `--version` shows more details:
```
 syntran 0.0.48
 https://github.com/JeffIrwin/syntran
 git commit = 6ab926d
 build date = Sep  7 2024
 fortran compiler = gfortran [11, 4, 0]
```
This can be helpful for binary installations.  If you built from source, most of the details are pointlessly redundant, and the `git commit` will not be shown.

## If statements and for loops

If, else if, and else statements work like you might expect for languages similar to C.  Like Rust, parentheses around the condition are optional:

```cpp
let condition = false;
let other_condition = true;

let foo = 0;
let bar = 0;
if condition {
    foo = 1;
    bar = 2;
} else if other_condition {
    foo = 3;
    bar = 4;
} else {
    foo = 5;
    bar = 6;
}

foo + bar;
// 7
```

When the clause of the if statement is only a single statement, braces `{}` are optional.

The bounds of for loops, like ranges in Rust and Python, are inclusive of the starting bound and exclusive of the ending bound:
```cpp
for i in [0: 5]
    println(i);
// 0
// 1
// 2
// 3
// 4
```
I will often refer to the *starting bound* and *ending bound* as the lower
and upper bounds respectively.  This is how the variables are named in the
interpreter.  However, it is a poor choice of words for negative or downward
steps:
```rust
for i in [5: -1: 0]
    println(i);
// 5
// 4
// 3
// 2
// 1
```

## Example:  calculating prime numbers inefficiently

With only these language features, we can make a short program to find prime numbers:

```cpp
// Get the largest prime number less than n
let n = 100;

// Initialize the largest prime found so far
let prime = 0;

// This check is O(n**2) time, which might be the best we can do without
// arrays

// If we had while loops or `break` statements, we could loop from n downards
// and stop as soon as we find the first prime
for i in [0: n]
{
    // Check if i is composite, i.e. not prime
    let is_composite = false;

    // Largest possible divisor of i is i/2.  Actually it's sqrt(i) but
    // I don't have a sqrt fn yet
    for j in [2: i/2 + 1]
    {
        // Is i divisible by j?
        let divisible = i % j == 0;
        is_composite = is_composite or divisible;
    }

    if not is_composite
        prime = i;
}

// Final result
println(prime);
// 97
```

## Variable scoping

Each block statement has its own scope for variables.  [Inner blocks can shadow](https://en.wikipedia.org/wiki/Variable_shadowing) outer blocks:

```cpp
let expect_2a = 0;
let expect_4a = 0;
let expect_2b = 0;
let expect_1a = 0;

let v = 1;
{
    // The LHS variable shadows and is initialized to the RHS value from the outer block
    let v = v + 1;
    expect_2a = v;

    {
        let v = v * 2;
        expect_4a = v;
    }

    expect_2b = v;
}

expect_1a = v;

expect_2a == 2 and
expect_4a == 4 and
expect_2b == 2 and
expect_1a == 1;
// true
```

## While loops

With the addition of while loops to the language, we can make some optimizations to the simple prime number sieve from above.  We can break the outer loop as soon as the first prime number is found, and we can break the inner loop as soon as we find out a number is _not_ prime:

```cpp
// Get the largest prime number less than n
let n = 1000000;

// Initialize
let prime = 0;
let i = n;

while prime == 0
{
    i = i - 1;  // loop from n downwards

    let is_composite = false;

    let j = 1;
    while j < i/2 + 1 and not is_composite
    {
        j = j + 1;

        let divisible = i % j == 0;
        is_composite = is_composite or divisible;
    }

    if not is_composite
        prime = i;
}

println(prime);
// 999983
```

With this method we can search for primes near 1 million in less than a second.  How long does the for loop version take to find primes up to a million?

## Example:  calculating π and a sine function inefficiently

With the addition of a 32-bit floating point type to the language, we can start to do some more interesting numerical work.

We can calculate the mathematical constant π [exteremely inefficiently](https://github.com/JeffIrwin/syntran/blob/main/samples/pi-1.syntran) using the slowly converging [Madhava-Leibniz series](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80), or we can use a slightly less inefficient [BBP-type formula](https://mathworld.wolfram.com/PiFormulas.html).  Just don't try to use π as a variable identifier name, because syntran source code is ASCII.

Then we can calculate a sine function using its [Taylor series expansion](https://en.wikipedia.org/wiki/Taylor_series):

```cpp
// Calculate π
let pi = 0.0;

for k in [0: 10]
{
    pi += 1 / (16.0 ** k) *
        (
            4.0 / (8*k + 1) -
            2.0 / (8*k + 4) -
            1.0 / (8*k + 5) -
            1.0 / (8*k + 6)
        );
}

println(pi);
// 3.141593E+00

// x is 30 degrees (in radians)
let x = pi / 6;

// Calculate sin(x) using Taylor series

// Initialize Taylor series terms
let xpow = x;
let factorial = 1;
let sign = 1;

// Sum odd terms only
let sinx = 0.0;
for k in [1: 10]
{
    sinx += sign * xpow / factorial;
    xpow *= x ** 2;
    factorial *= (2*k) * (2*k + 1);
    sign = -sign;
}

println(sinx);
// 4.999999E-01
```

At the end of all those transcendental functions and numbers, we get the suprisingly rational result `sin(pi / 6) == 0.5`, or `4.999999E-01` with 32 bit floats.

### Notes on casting in arithmetic expressions

There are several operations you have to be careful with in the sine example above.

In the pi series, there is a float term `16.0 ** k`.  The loop iterator `k` is an integer, so if we used a literal integer `16` instead of the float `16.0`, that would quickly overflow even for the relatively small upper loop bound `k < 10`.  Hence, we raise a float base to an int power, which yields a float result that is safe from overflow for these values.

Similarly, the pi term `4.0 / (8*k + 1)` has a float numerator and int denominator.  Again we must use a float to avoid integer division.

Syntran is not a [nanny language](https://retrocomputing.stackexchange.com/a/15379/26435), but it allows you to do numeric work without constantly manually casting things [`as f64` like in Rust](https://doc.rust-lang.org/rust-by-example/types/cast.html).

## Arrays

Recall the syntax for a for-loop:
```rust
for i in [0: 5]
    println(i);
```

The expression `[0: 5]` is one of several array forms, which can also be assigned to variables:
```rust
let v0 = [0: 5];
// [0, 1, 2, 3, 4]
```

Array sizes do not need to be literals or constants.  Arrays are allocated dynamically at runtime.

Besides ranges of consecutive integers, there are other array forms.

To initialize an array to a range with a step:
```rust
let v1 = [10: -2: 0];
// [10, 8, 6, 4, 2]
```

To refer to an element of an array, place the index in square brackets:
```rust
v1[0];
// 10

v1[2];
// 6
```

To initialize an array to all zeros, or any other uniform scalar, use [Rust syntax](https://doc.rust-lang.org/std/primitive.array.html).  The size goes after the semicolon `;` inside the brackets:
```rust
let scalar = 0;
let v2 = [scalar; 5];
// [0, 0, 0, 0, 0]
```

To initialize an array with an explicit list of comma-separated values:
```rust
let v3 = [-5, 3+1, 1, 10, 7/2];
// [-5, 4, 1, 10, 3]
```

To concatenate rank-1 arrays, separate them by commas within an outer set of
brackets:
```rust
let v4 = [[0: 3], [10], [20: 22]];
// [0, 1, 2, 10, 20, 21]
```
Multi-rank arrays cannot be concatenated in a single statement.  Build them up
over several statements with slices or for loops.  If you want to concatenate
both scalars and vectors, you have to form the scalars into vectors of size 1,
like `[10]` above.

### Rank-2 and higher arrays

Syntran has a more compact syntax for multi-rank arrays than Rust, which requires nested rank-1 arrays of rank-1 arrays.  As above, the sizes go after the semicolon `;`.  To initialize a rank-2 array with size 3 by 4 to all zeros:
```rust
let matrix = [0; 3, 4];
// [
// 0, 0, 0,
// 0, 0, 0,
// 0, 0, 0,
// 0, 0, 0
// ]
```

Note that arrays are stored in [column-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order) as in Fortran, so they appear transposed when printing in the default format.

To initialize a rank-3 array with size rows by columns by sheets:
```rust
let rows = 5;
let cols = 3;
let shts = 4;
let array = [0; rows, cols, shts];
// [
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
//
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
//
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
//
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0,
// 0, 0, 0, 0, 0
// ]
```

Indices for multi-rank arrays are separated by commas:
```rust
array[3,2,1];
// 0
```

To initialize a multi-rank array with an explicit list of values, separate the values with commas and then provide the size after a semicolon:
```rust
let a = [1, 2, 3, 4, 5, 6;  2, 3];
// [
// 1, 2,
// 3, 4,
// 5, 6
// ]

a[1,2];
// 6
```

Many languages refer to these arrays as *multidimensional* arrays.  This can be
ambiguous, as a vector like `let v = [9, 16, 25]` is three-dimensional in at
least some sense, but only rank-1.  We will call these *multi-rank* arrays.

### Array slicing and indexing

Indexing an array with a range subscript, as opposed to a scalar subscript,
produces yet another array:
```rust
let v0 = [0: 5];
// [0, 1, 2, 3, 4]
v0[1: 4];
// [1, 2, 3]

let v1 = [0: 2: 10];
// [0, 2, 4, 6, 8];
v1[1: 4];
// [2, 4, 6]
```

As with other expressions, the range bounds are inclusive of the lower bound and
exclusive of the upper bound.

Subscripting with just a colon `:` and no bounds returns the whole array along
that dimension:
```rust
v1[:];
// [0, 2, 4, 6, 8];
```

For rank-1 arrays, boundless whole-array slicing is not useful and less
performant than omitting the subscript `[]` expression altogether:
```rust
v1;
// [0, 2, 4, 6, 8];
```

You can also slice with a step in the form `lower_bound: step: upper_bound`:
```rust
v1[0: 2: 5];
// [0, 4, 8];
```
Using negative steps, you can reverse a vector:
```rust
v1[size(v1,0)-1: -1: -1];
// [8, 6, 4, 2, 0]
```

Arbitrary elements of an array with non-uniform steps can be extracted by using
another rank-1 array (i.e. vector) as a subscript:
```rust
v1[[0, 1, 3]];
// [0, 2, 6]
```
The double brackets might look strange.  Here, the outer brackets denote a
subscript or index of `v1`, while the inner brackets denote an array literal. If
you wrote `v1[0, 1, 3]`, that would work on a rank-3 array, but it would throw a
parser error for a rank-1 array `v1`. This might be more clear if we have a
helper index array variable with the same effect as the last example:
```rust
let indices = [0, 1, 3];
v1[indices];
/// [0, 2, 6]
```
As in Fortran, only rank-1 arrays can be used as an index array.  Of course,
they can index into an array of any rank, as shown in the [multi-rank
subsection](#multi-rank-array-slicing) below.

#### LHS and RHS slicing

Arrays can be sliced whether they are on the left-hand side (LHS) or the
right-hand side (RHS) of an assignment operator.  The examples above show RHS
slicing.

Assigning to an LHS slice changes only the sliced part of the array:
```rust
let v2 = [0: 5];
// [0, 1, 2, 3, 4]
v2[1: 4] = 7;
// [0, 7, 7, 7, 4]
```
The value returned by the entire assignment expression above is the
whole v2 array, not just the assigned slice.  This distinction is important when
such an assignment expression is the return value of a function, or if an
assignment is nested on the RHS of another assignment:
```rust
let v3 = [0: 5];
let v4 = v3[1: 4] = 7;
v4;
// [0, 7, 7, 7, 4]
```
Note that `v4` is *neither* just the scalar `7` nor the slice `[7, 7, 7]`,
rather it is the whole `v3` array.  Nested subscripted assignments such as this
are [illegal in python](https://stackoverflow.com/a/60909096/4347028).

This behaviour is in contrast to non-nested assignment:
```rust
let v5 = [0: 5];
let v6 = v5[1: 4];
v6;
// [1, 2, 3]
```

#### Multi-rank array slicing

Rank-2 and higher arrays can also be sliced.  In general, this returns an array
of a different rank.  For example, slicing an array with 1 slice subscript `:`
and the rest scalar subscripts will return a rank-1 array.  Slicing an array
with 2 slice subscripts and the rest scalars will return a rank-2 array.

```rust
let matrix = [
     0,  1,  2,  // values
     3,  4,  5,
     6,  7,  8,
     9, 10, 11 ;
    3, 4         // size
];

let x_slice = matrix[:,0];
println("x_slice = ", x_slice);
// x_slice = [0, 1, 2]

let y_slice = matrix[1,:];
println("y_slice = ", y_slice);
// y_slice = [1, 4, 7, 10]

let mat_slice = matrix[0: 2, 1: 4];
println("mat_slice = ", mat_slice);
// mat_slice = [
// 3, 4,
// 6, 7,
// 9, 10
// ]
```

For every dimension of the array, you can mix and match scalar subscripts,
whole-array slices, range-based slices, stepped slices, or index-array slices:
```rust
matrix[:, [0, 1, 3]];  // all cols, rows 0, 1, and 3
// [
// 0, 1, 2,
// 3, 4, 5,
// 9, 10, 11
// ]

matrix[1, [0, 2, 3]];  // col 1, rows 0, 2, and 3
// [1, 7, 10]
```

## Functions

This section is about user-defined functions.  [See this page](doc/) for a list of intrinsic syntran functions.

Use the `fn` keyword to declare a function, as in Rust.  Unlike Rust, use a colon `:` before the return type instead of `->`:

```rust
fn add(a1: i32, a2: i32): i32
{
    return a1 + a2;
}
```

The function defined above could be used like this:

```rust
let a = 3;
let b = 4;
let c = add(a + 1, b + 2);
// 10
```

⚠ I highly suggest saving functions in a `.syntran` file, but you can do it in the REPL if you like pushing rocks up hills.

Functions must be defined before they are called.  That means that recursive functions are not possible currently, neither with a function directly calling itself, nor with two functions which both call each other.

Here's a function that performs matrix-vector multiplication:
```rust
fn mul_mat_vec(mat: [f64; :,:], vec: [f64; :]): [f64; :]
{
    // Matrix-vector multiplication.  Return mat * vec
    let ans =  [0.0; size(mat,0)];
    for     j in [0: size(mat,1)]
        for i in [0: size(mat,0)]
            ans[i] += mat[i,j] * vec[j];
    return ans;
}
```
Checking the inner dimensions is left as an exercise for the reader.

Note that array rank is specified in function signatures with comma-separated colons.  For example, `vec` is a rank-1 array `[f64; :]` and `mat` is a rank-2 array `[f64; :,:]`.  Arrays of any size can be passed to functions, but ranks and types must match.  The use of a colon as a wildcard like this is [borrowed from Fortran](https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/specification-statements/type-declarations/declarations-for-arrays/assumed-shape-specifications.html).

Here's a function that performs [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication) on two matrices `a` and `b`:

```rust
fn mul_mat(a: [f64; :,:], b: [f64; :,:]): [f64; :,:]
{
    if size(a,1) != size(b,0)
    {
        println("Error in mul_mat: inner dimensions do not agree!");
        exit(-1);
    }
    let c = [0.0; size(a,0), size(b,1)];
    for         k in [0: size(b,1)]
        for     j in [0: size(a,1)]
            for i in [0: size(a,0)]
                c[i,k] += a[i,j] * b[j,k];
    return c;
}
```

Then we can define [rotation matrices](https://en.wikipedia.org/wiki/Rotation_matrix) for 90 degree rotations about the _x_ and _y_ axes:

```rust
let rotx =
    [
        1.0,  0.0,  0.0,
        0.0,  0.0,  1.0,
        0.0, -1.0,  0.0 ;
        3, 3
    ];

let roty =
    [
        0.0,  0.0, -1.0,
        0.0,  1.0,  0.0,
        1.0,  0.0,  0.0 ;
        3, 3
    ];
```

Rotations can be composed by multiplying matrices, so we can apply a 180 degree _x_ rotation followed by a 180 degree _y_ rotation like this:

```rust
mul_mat(mul_mat(mul_mat(rotx, rotx), roty), roty);
// [
// -1.000000E+00, 0.000000E+00, 0.000000E+00,
// 0.000000E+00, -1.000000E+00, 0.000000E+00,
// 0.000000E+00, 0.000000E+00, 1.000000E+00
// ]
```

As expected, this is the same as a 180 degree _z_ rotation, i.e. the _x_ and _y_ components are negated while the _z_ component is unchanged.

### Passing by value or by reference

Syntran is pass-by-value by default, regardless of type.  Primitive scalars,
arrays, and structs are all copied and passed by value.  Modifying a parameter
within a function has no effect on the corresponding argument in the caller.
Copying values incurs an overhead for large arrays and structs.

To modify an argument as an extra function output, or to avoid the overhead of
copying a large amount of data for an array or large struct, pass by reference
instead.

Denote the reference with an ampersand `&`, both in the function declaration and
in the function call.  For example:
```rust
fn add_one(var_ref: &i32)
{
    var_ref += 1;
    return;
}

let x = 42;
println("x = ", x);
// x = 42

add_one(&x);
println("x = ", x);
// x = 43
```

If you make a reference `&` in the declaration but not the caller, or
vice-versa, the parser will throw an error.

Only variable identifiers can be used as a reference.  Literals cannot be
referenced, and array elements and struct members cannot currently be
referenced.

## Strings, printing, and file output

<!-- TODO: file input -->

The ASCII string type `str` uses `"`quotes`"` to assign literals:

```rust
let string0 = "hello world";
// hello world
```

To include an escaped quote literal, double it:

```rust
let string1 = "syntran is a ""programming language""";
// syntran is a "programming language"
```

Strings are concatenated with the `+` operator:

```rust
let string2 = "hello " + ("planet " + "earth");
// hello planet earth
```

There is no separate character type, only strings of length 1.  Characters of a string are indexed in the same way as arrays:

```rust
let string3 = "hello";
string3[0];
// h
string3[1];
// e
string3[2];
// l
```

<!-- TODO: not TBD anymore! -->
Slice indexing for substrings of length > 1 is TBD.
To slice a substring, use a range:
```rust
let string4 = "01234567";
string4[2:5];
// 234
string4[3:6];
// 345
```

The intrinsic function [`str()`](doc/README.md#str) converts other types to `str` and concatenates them:

```rust
let string5 = "testing " + str(1, " ", 2, " ", 1.0, " ", false);
// testing 1 2     1.000000E+00 false
```

Integers and bools are stringified without padding, so separate them with `" "` if that's what you want.

Use [`println()`](doc/README.md#println) to print to stdout:

```rust
println("hello world");
// hello world
```

Use [`open()`](doc/README.md#open), [`writeln()`](doc/README.md#writeln), and [`close()`](doc/README.md#close) to write to a file:

```rust
//// syntran prompt
let file = open("test.txt");
writeln(file, "hello world");
writeln(file, "here's a second line of text with a number ", 42);
close(file);
//// Ctrl+C to exit syntran prompt

//// shell prompt
cat test.txt
// hello world
// here's a second line of text with a number 42
```

Only ASCII strings are supported because syntran is interpretted in Fortran.  Unicode strings cannot be indexed properly:

```rust
let string6 = "🔥🥵💀";

string6;
// 🔥🥵💀 // YMMV

string6[0];
// mojibake
string6[1];
// mojibake
string6[2];
// mojibake
```

## Include files

Like the C language, the contents of one source file can be included in a higher-level source using the `#include` preprocessing directive.

Let's say you have a file named `header.syntran` which defines a global variable and a string helper function:

```rust
// header.syntran

let my_global = 42;

fn my_scan(str_: str, set: str): i32
{
    // Return the first substring index i of `str_` which matches any character
    // from the string `set`.
    //
    // c.f. Fortran intrinsic scan()

    let found = false;
    let i = 0;
    while not found and i < len(str_)
    {
        let j = 0;
        while not found and j < len(set)
        {
            found = str_[i] == set[j];
            j += 1;
        }
        i += 1;
    }

    if (found)
        i -= 1;
    else
        i = -1;
    return i;
}
```

This can be included in a main program, assuming `main.syntran` and `header.syntran` are in the same folder:

```rust
// main.syntran

#include("header.syntran");

fn main()
{
    println(my_global);
    // 42

    println(my_scan("012345", "2"));
    // 2

    println(my_scan("012345", "3"));
    // 3

    return;
}

main();
```

As in C, the effect of `#include` is as if the contents of the header are pasted into the *including* file.  The difference is that any error messages will still reference the correct line number and filename where they occur.

If included files are in a separate folder, the included filename is a path relative to the parent including file.

Note that the syntax of the `#include` directive looks like a function call, i.e. it has parentheses around the filename argument and a semicolon at the end.  This gives syntran a consistent feel throughout its features, unlike C.  However, be careful noting that `#include` is *not* a real function!  It is a preprocessing directive, indicated by the hash `#`, so the full evaluation facilities of syntran are not available at the time that the directive is processed.

For example, it is *not* possible to concatenate strings together into the include filename:

```rust
#include("header." + "syntran");
// Error: #include file `"header."` not found
//   --> main.syntran:3:10
//    |
//  3 | #include("header." + "syntran");
//    |          ^^^^^^^^^ file not found
//
// Error: unexpected token `+` of kind `plus_token`, expected `rparen_token`
//   --> main.syntran:3:20
//    |
//  3 | #include("header." + "syntran");
//    |                    ^ unexpected token
// ...
```

The argument must be a single static lex-time constant string.

## Structs

**Disclaimer**: structs currently do not work in the interactive interpretter
shell.  To use structs, you must save your syntran script in a file.

Structs, also known as user-defined types or derived types, can be used in
syntran.  The declaration or definition of a struct in syntran uses similar
syntax as rust:
```rust
struct Point
{
    x: [i32; :],
    name: str,
}
```

Here, a `Point` contains an array of integer coordinates `x` and a name.  In
other languages, you may see introductory struct examples represent a point with
separate scalar `x` and `y` members.  However, this is an insane way to
represent vector data.  As syntran is an array-oriented language, we use an
array in this example.  This will generalize nicely from 2D to 3D or any
dimension.

Note that each member of the struct is delimited by a comma `,` in the struct
declaration. The trailing comma after the last member `name: str,` is optional.

Also note that there is no semicolon at the end of the struct declaration `}`.
This makes struct declaration consistent with the way that functions are
declared.

Next, we can instantiate or initialize a couple variables `pt0` and `pt1` of the
`Point` type:
```rust
let pt0 = Point{x = [20, 10], name = "my-pt0"};
let pt1 = Point{x = [40, 50], name = "my-pt1"};
```

Unlike rust, members are assigned using an assignment operator `=`, not a colon
`:`.  Instantiating a struct, just like instantiating any other primitive type
variable such as `let x = 42;`, is a statement.  Hence, the instantiation
statement ends with a semicolon `;`.

When a struct is initialized, _all_ of its members must be initialized.  If you
want to have a struct with some default values that you don't want to explicitly
define every time, you can use a helper function to construct it.

Structs can be nested.  We can build upon the 1st order `Point` struct by
declaring a `Rect` struct, some of whose members are also structs:
```rust
// declare
struct Rect
{
    bottom_left: Point,
    upper_right: Point,
    name: str
}

// instantiate
let rect0 = Rect{bottom_left = pt0, upper_right = pt1, name = "my-rect"};
```

Functions can take struct arguments and return struct values.  For example, here
is a function that computes the area of a rectangle:
```rust
fn area(rect: Rect): i32
{
    let width =
        rect.upper_right.x[0] -
        rect.bottom_left.x[0];
    let height =
        rect.upper_right.x[1] -
        rect.bottom_left.x[1];

    // Taking the absolute value is left as an exercise for the reader
    return width * height;
}

println("area = ", area(rect0));
// area = 800
```

Here is a function that constructs and returns a `Rect`:
```rust
fn get_unit_rect(): Rect
{
    return Rect
    {
        bottom_left = Point{x = [0, 0], name = "bl"},
        upper_right = Point{x = [1, 1], name = "ur"},
        name = "unit-rect",
    };
}
println("unit area = ", area(get_unit_rect()));
// unit area = 1
```

Structs can be nested arbitrarily.  You can make structs of arrays and arrays of
structs.  One noteable missing feature currently is that structs of arrays (and
arrays of structs) cannot be sliced, only scalar subscripts are supported.

Here is a contrived example of a high-order struct, including arrays at various
levels:
```rust
struct A{_:  i32 }
struct B{a: [A;:]}
struct C{b:  B   }
struct D{c: [C;:]}
struct E{d:  D   }

let a =  A{_ = 1337};
let b =  B{a = [a] };
let c =  C{b =  b  };
let d =  D{c = [c] };
let e = [E{d =  d  }];

e[0].d.c[0].b.a[0]._ -= 1295;
println(e[0].d.c[0].b.a[0]._);
// 42
```

## Samples

Many syntran samples are provided in this repository and elsewhere:

1.  [Inline tests](src/tests/test.f90):  these are short one-to-few line syntran snippets, embedded in Fortran as a string and `eval`'d.  They are covered by the tests
2.  [Script tests](src/tests/test-src):  these are longer syntran scripts, organized into categories by directory.  They are covered by the tests
3.  [Samples](samples):  these are longer syntran scripts which can also take a while to run, e.g. the wave equation solvers.  They are *not* covered by the tests because of the time they take to run
4.  [Advent of Code](https://github.com/JeffIrwin/aoc-syntran):  these are syntran scripts which solve problems from the [Advent of Code](https://adventofcode.com/about).  They are *not* covered by tests and in a separate repository

