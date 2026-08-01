
![](https://github.com/JeffIrwin/syntran/workflows/CI/badge.svg)

# Syntran

<!-- [![](doc/imgs/logo-syntran-128p.png)](https://github.com/JeffIrwin/syntran/blob/main/samples/logo.syntran) -->
[![](doc/imgs/logo-syntran-128p.png)](samples/logo.syntran)

⚠️ Syntran is beta and I don't recommend using it for anything serious.  You may discover bugs, missing features, general pain points; and later updates may be incompatible.

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

### Linux
<!-- syntran-begin mode=skip reason="shell installation command, not syntran code" -->
```
curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-linux.zip"
unzip syntran-linux.zip
chmod +x ./syntran
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
./syntran -h
```
<!-- syntran-end -->

The binary asset `syntran-linux.zip` works on most distros:  alma, arch, debian, fedora, kali, rocky, and ubuntu.

<!--
// The rocky binary is the official binary now.  It used to be ubuntu, but the
// rocky binary works on ubuntu.  One downside of this is that rocky does not have
// libgfortran.a and thus cannot statically link to it like ubuntu can, but we also
// have the libquadmath.so too.  Could possibly get around this by building
// gfortran from source on rocky, but I'm not going to spend the ci/cd on that (it
// takes a couple hours locally, see docker/Dockerfile.gcc)
//
// The ubuntu binary is still built and uploaded, but I probably won't bother
// documenting it or copying it to the release assets

### Linux (rocky and alma)
```
curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-rocky.zip"
unzip syntran-rocky.zip
chmod +x ./syntran
./syntran -h
```

Note that there is an extra step to set the `LD_LIBRARY_PATH` here, because the
rocky build image cannot statically link libgfortran.  This works on ubuntu and
probably other distros too, or you can just use the more universal asset above.
-->

### Windows
<!-- syntran-begin mode=skip reason="shell installation command, not syntran code" -->
```
curl -LO "https://github.com/JeffIrwin/syntran/releases/latest/download/syntran-windows.zip"
tar -xvf syntran-windows.zip
.\syntran -h
```
<!-- syntran-end -->
Or, download and unzip it however you like.

More or less any terminal on Windows should work, but [Windows Terminal](https://aka.ms/terminal) supports colors and arrow key history better than Windows CMD.  A git bash shell is even better.  Also see the [section on arrow keys](#arrow-keys-and-command-history).

<!-- mac works too but i don't want to encourage apple usery -->

### Path and environment settings

Feel free to add the directory to your PATH environment variable, or type the
full path.

Whenever you see something like `./build/Debug/syntran` or `fpm run` in the rest
of this documentation, replace that with `/path/to/syntran` or
`C:\path\to\syntran.exe` appropriately, depending on your operating system and
where you downloaded the binary.

I recommend running `ulimit -s unlimited` in `~/.bashrc` or at least in your
current shell to remove the stack limit.  Otherwise, syntran can crash with
large recursion depths of recursive function calls (see issue
https://github.com/JeffIrwin/syntran/issues/28).

## Build the interpreter from source

Using cmake:

<!-- syntran-begin mode=skip reason="builds the interpreter itself; out of scope for output testing" -->
```
./build.sh
```
<!-- syntran-end -->

If you installed syntran as a binary, you can skip this section.

A [Fortran compiler](https://fortran-lang.org/en/compilers/) and either [CMake](https://cmake.org/download/) or [FPM](https://fpm.fortran-lang.org/index.html) are required.  Supported compilers are gfortran 10 through 15, or Intel 2025.2.  Also check `matrix.gfortran` in the [github actions workflow ](.github/workflows/main.yml) to see which compilers are regularly tested in CI/CD.  For performance, gfortran is recommended over Intel.

Two independent build systems are provided for syntran.  You can either use cmake, which is run by `build.sh` as shown above, or you can use the Fortran Package Manager `fpm`:

<!-- syntran-begin mode=skip reason="builds the interpreter itself; out of scope for output testing" -->
```
fpm build
```
<!-- syntran-end -->

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

<!-- syntran-begin mode=skip reason="starts an interactive REPL with no input; nothing to assert" -->
```
./build/Debug/syntran
```
<!-- syntran-end -->

Then enter arithmetic expressions like `1 + 2 * 3;` in the interpreter.  Semicolons are required at the end of statements!

<!-- cpp is the closest match I can find for markdown syntax highlighting -->
<!-- syntran-begin mode=skip reason="illustrates the syntran$ prompt convention itself, not a `// result` style block" -->
```cpp
syntran$ 1 + 2 * 3;
7
syntran$ (1 + 2) * 3;
9
```
<!-- syntran-end -->

Expressions are evaluated immediately and the result is printed to the console.  In the rest of this documentation, we will hide the `syntran$` prompt and show the result as a `// comment`, so you can copy and paste code blocks straight into the interpreter.

Use two asterisks for exponent powers, like Fortran and Scilab:

<!-- syntran-begin mode=repl group=intro-arith -->
```cpp
5 ** 2;
// 25
```
<!-- syntran-expect
25
-->
<!-- syntran-end -->

There's no need to [import `math.h`](https://en.cppreference.com/w/c/numeric/math/pow) and call the `pow()` function!

## Variables, Booleans, and type checking

Variable declarations use the [`let` keyword](https://doc.rust-lang.org/std/keyword.let.html) as in Rust.  This is also similar to JavaScript, except there is no `var` keyword.  Variables are mutable.

Integer `i32` and `i64`, float `f32` and `f64`, string `str`, and Boolean `bool` types are supported.  Attempting operations on the wrong types yields an error, e.g. trying to add a bool or use logical `and` on an int or float.

<!-- syntran-begin mode=repl group=vars-bool -->
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
// Error[E48]: binary operator `and` is not defined for types i32_type and bool_type
//   --> <stdin>:1:5
//    |
//  1 | foo and p;
//    |     ^^^ wrong types for this binary operator
```
<!-- syntran-expect
1
2
4
3
7
true
false
true
true
Error[E48]: binary operator `and` is not defined for types i32_type and bool_type
  --> <stdin>:1:5
   |
 1 | foo and p;
   |     ^^^ wrong types for this binary operator
-->
<!-- syntran-end -->

Logical keywords `true`, `false`, `not`, `and`, and `or` are like Fortran's (e.g. `.true.`) but without the dots.  Note that they are lower case-sensitive, unlike Python (e.g. `True`).

### Floating point types

The default floating point type is `f64`, i.e. double precision.  To make an `f32` float literal, append an `'f32` suffix, like in the C language.  For example:
- the literal `1.0` is of type `f64`
- the literal `1.0'f32` is of type `f32`
- the literal `6.0221408e+23` (Avogadro's number) is of type `f64`
- the literal `6.0221408e+23'f32` is of type `f32`

When float types are mixed in an arithmetic expression, the result is casted up to the higher precision of the operands.  For example:
<!-- syntran-begin mode=repl group=float-cast -->
```rust
let x = 1.0 + 1.0'f32;
```
<!-- syntran-expect
    2.000000000000000E+00
-->
<!-- syntran-end -->
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

Syntran's REPL supports this out of the box, on Linux, macOS, and Windows alike, using the vendored [isocline](https://github.com/daanx/isocline) line-editing library (see `external/isocline` and `src/line_edit.f90`).  When you run `syntran` interactively:

<!-- syntran-begin mode=skip reason="starts an interactive REPL with no input; nothing to assert" -->
```
./build/Debug/syntran
```
<!-- syntran-end -->

the arrow keys move the cursor and scroll through history, Ctrl+R starts a reverse history search, and command history is saved across separate invocations of `syntran` in `~/.syntran_history` (`%USERPROFILE%\.syntran_history` on Windows).  Hit Ctrl+D on an empty line to exit the REPL.

No extra install or alias is required; this replaces the old `rlwrap`-based workaround.

## Syntax highlighting

I do not plan on writing any syntax highlighting plugins.

The easiest way to get highlighting is to have your editor treat syntran as a similar language.  Rust is a pretty good match with keywords like `let`, `fn`, and type names `i32`, `f64`, etc.  C++ is also an ok match (it has `and` and `or` keywords).

For neovim, add this line to your `~/.config/nvim/ftdetect/syntran.lua` file:

<!-- syntran-begin mode=skip reason="neovim lua config snippet, not syntran code" -->
```lua
vim.cmd.autocmd("BufRead,BufNewFile *.syntran set filetype=rust")
```
<!-- syntran-end -->

## Command-line usage

### Saving scripts in a file

As programs get longer and more complicated, it becomes difficult to enter them into the interactive interpreter.  To interpret a whole file, provide it as a command line argument:

<!-- syntran-begin mode=skip reason="shell invocation example" -->
```
./build/Debug/syntran samples/primes-1.syntran
```
<!-- syntran-end -->

<!--
Note: global block statement is not required as of 0.0.13.  Multiple statements (and functions) are parsed at the global scope.

Make sure to wrap the entire script in a main block with braces `{}`.  The global block `{}` is not required when interactively using the interpreter because it parses and evaluates one statement at a time.  However, if you forget the global block `{}` in a script file, only the first statement will be parsed and any trailing junk statements will be unexpected.
-->

### Other command-line arguments

Run `syntran -h` to see a comprehensive listing of syntran command-line arguments:

<!-- syntran-begin mode=help group=help-usage -->
```
 syntran 1.4.0
 https://github.com/JeffIrwin/syntran

 Usage:
     syntran <file.syntran> [options] [-- <script args>...]
     syntran
     syntran -c <cmd> | --command <cmd>
     syntran -h | --help
     syntran --version

 Options:
     -h --help           Show this help
     --version           Show version and build details
     -c --command <cmd>  Run program passed in as string
     --color (off|on)    Set ANSI text color [default: auto]
     --fmax-errors <n>   Limit max error messages to <n> [default: 4]
     -i --interactive    Interpret a file then start an interactive shell
     -q --quiet          Don't print the banner, only errors and println calls
     --permissive-return Downgrade missing-return errors to warnings
     --cd                Resolve the script's relative file paths against its own directory
     -s --syntax-only    Parse and type check without running the program
     -- <args>...        Pass remaining arguments to script via std::args()
```
<!-- syntran-expect
 syntran X.Y.Z
 https://github.com/JeffIrwin/syntran
 Usage:
     syntran <file.syntran> [options] [-- <script args>...]
     syntran
     syntran -c <cmd> | --command <cmd>
     syntran -h | --help
     syntran --version
 Options:
     -h --help           Show this help
     --version           Show version and build details
     -c --command <cmd>  Run program passed in as string
     --color (off|on)    Set ANSI text color [default: auto]
     --fmax-errors <n>   Limit max error messages to <n> [default: 4]
     -i --interactive    Interpret a file then start an interactive shell
     -q --quiet          Don't print the banner, only errors and println calls
     --permissive-return Downgrade missing-return errors to warnings
     --cd                Resolve the script's relative file paths against its own directory
     -s --syntax-only    Parse and type check without running the program
     -- <args>...        Pass remaining arguments to script via std::args()
-->
<!-- syntran-end -->

Although the semantic version is always shown, `--version` shows more details:
<!-- syntran-begin mode=skip reason="--version output embeds git commit and build date, inherently unstable" -->
```
 syntran 0.0.48
 https://github.com/JeffIrwin/syntran
 git commit = 6ab926d
 build date = Sep  7 2024
 fortran compiler = gfortran [11, 4, 0]
```
<!-- syntran-end -->
This can be helpful for binary installations.  If you built from source, most of the details are pointlessly redundant, and the `git commit` will not be shown.

## If statements and for loops

If, else if, and else statements work like you might expect for languages similar to C.  Like Rust, parentheses around the condition are optional:

<!-- syntran-begin mode=repl group=if-else -->
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
<!-- syntran-expect
false
true
0
0
4
7
-->
<!-- syntran-end -->

When the clause of the if statement is only a single statement, braces `{}` are optional.

The bounds of for loops, like ranges in Rust and Python, are inclusive of the starting bound and exclusive of the ending bound:
<!-- syntran-begin mode=repl group=for-basic -->
```cpp
for i in [0: 5]
    println(i);
// 0
// 1
// 2
// 3
// 4
```
<!-- syntran-expect
0
1
2
3
4
-->
<!-- syntran-end -->
I will often refer to the *starting bound* and *ending bound* as the lower
and upper bounds respectively.  This is how the variables are named in the
interpreter.  However, it is a poor choice of words for negative or downward
steps:
<!-- syntran-begin mode=repl group=for-downward -->
```rust
for i in [5: -1: 0]
    println(i);
// 5
// 4
// 3
// 2
// 1
```
<!-- syntran-expect
5
4
3
2
1
-->
<!-- syntran-end -->

## Example:  calculating prime numbers inefficiently

With only these language features, we can make a short program to find prime numbers:

<!-- syntran-begin mode=repl group=primes-for -->
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
<!-- syntran-expect
100
0
97
-->
<!-- syntran-end -->

## Variable scoping

Each block statement has its own scope for variables.  [Inner blocks can shadow](https://en.wikipedia.org/wiki/Variable_shadowing) outer blocks:

<!-- syntran-begin mode=repl group=scoping -->
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
<!-- syntran-expect
0
0
0
0
1
2
1
true
-->
<!-- syntran-end -->

## While loops

With the addition of while loops to the language, we can make some optimizations to the simple prime number sieve from above.  We can break the outer loop as soon as the first prime number is found, and we can break the inner loop as soon as we find out a number is _not_ prime:

<!-- syntran-begin mode=repl group=primes-while -->
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
<!-- syntran-expect
1000000
0
1000000
999983
-->
<!-- syntran-end -->

With this method we can search for primes near 1 million in less than a second.  How long does the for loop version take to find primes up to a million?

## Example:  calculating π and a sine function inefficiently

With the addition of a 32-bit floating point type to the language, we can start to do some more interesting numerical work.

We can calculate the mathematical constant π [exteremely inefficiently](https://github.com/JeffIrwin/syntran/blob/main/samples/pi-1.syntran) using the slowly converging [Madhava-Leibniz series](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80), or we can use a slightly less inefficient [BBP-type formula](https://mathworld.wolfram.com/PiFormulas.html).  Just don't try to use π as a variable identifier name, because syntran source code is ASCII.

Then we can calculate a sine function using its [Taylor series expansion](https://en.wikipedia.org/wiki/Taylor_series):

<!-- syntran-begin mode=repl group=pi-sine -->
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
// 3.141592653589791E+00

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
// 4.999999999999908E-01
```
<!-- syntran-expect
    0.000000000000000E+00
    3.141592653589791E+00
    5.235987755982986E-01
    5.235987755982986E-01
1
1
    0.000000000000000E+00
    4.999999999999908E-01
-->
<!-- syntran-end -->

At the end of all those transcendental functions and numbers, we get the suprisingly rational result `sin(pi / 6) == 0.5`, or `4.999999E-01` with 32 bit floats.

### Notes on casting in arithmetic expressions

There are several operations you have to be careful with in the sine example above.

In the pi series, there is a float term `16.0 ** k`.  The loop iterator `k` is an integer, so if we used a literal integer `16` instead of the float `16.0`, that would quickly overflow even for the relatively small upper loop bound `k < 10`.  Hence, we raise a float base to an int power, which yields a float result that is safe from overflow for these values.

Similarly, the pi term `4.0 / (8*k + 1)` has a float numerator and int denominator.  Again we must use a float to avoid integer division.

Syntran is not a [nanny language](https://retrocomputing.stackexchange.com/a/15379/26435), but it allows you to do numeric work without constantly manually casting things [`as f64` like in Rust](https://doc.rust-lang.org/rust-by-example/types/cast.html).

## Arrays

Recall the syntax for a for-loop:
<!-- syntran-begin mode=repl group=arr-for -->
```rust
for i in [0: 5]
    println(i);
```
<!-- syntran-expect
0
1
2
3
4
-->
<!-- syntran-end -->

The expression `[0: 5]` is one of several array forms, which can also be assigned to variables:
<!-- syntran-begin mode=repl group=arr-v0 -->
```rust
let v0 = [0: 5];
// [0, 1, 2, 3, 4]
```
<!-- syntran-expect
[0, 1, 2, 3, 4]
-->
<!-- syntran-end -->

Array sizes do not need to be literals or constants.  Arrays are allocated dynamically at runtime.

Besides ranges of consecutive integers, there are other array forms.

To initialize an array to a range with a step:
<!-- syntran-begin mode=repl group=arr-v1 -->
```rust
let v1 = [10: -2: 0];
// [10, 8, 6, 4, 2]
```
<!-- syntran-end -->

To refer to an element of an array, place the index in square brackets:
<!-- syntran-begin mode=repl group=arr-v1 -->
```rust
v1[0];
// 10

v1[2];
// 6
```
<!-- syntran-expect
[10, 8, 6, 4, 2]
10
6
-->
<!-- syntran-end -->

To initialize an array to all zeros, or any other uniform scalar, use [Rust syntax](https://doc.rust-lang.org/std/primitive.array.html).  The size goes after the semicolon `;` inside the brackets:
<!-- syntran-begin mode=repl group=arr-v2 -->
```rust
let scalar = 0;
let v2 = [scalar; 5];
// [0, 0, 0, 0, 0]
```
<!-- syntran-expect
0
[0, 0, 0, 0, 0]
-->
<!-- syntran-end -->

To initialize an array with an explicit list of comma-separated values:
<!-- syntran-begin mode=repl group=arr-v3 -->
```rust
let v3 = [-5, 3+1, 1, 10, 7/2];
// [-5, 4, 1, 10, 3]
```
<!-- syntran-expect
[-5, 4, 1, 10, 3]
-->
<!-- syntran-end -->

To concatenate rank-1 arrays, separate them by commas within an outer set of
brackets:
<!-- syntran-begin mode=repl group=arr-v4 -->
```rust
let v4 = [[0: 3], [10], [20: 22]];
// [0, 1, 2, 10, 20, 21]
```
<!-- syntran-expect
[0, 1, 2, 10, 20, 21]
-->
<!-- syntran-end -->
Multi-rank arrays cannot be concatenated in a single statement.  Build them up
over several statements with slices or for loops.  If you want to concatenate
both scalars and vectors, you have to form the scalars into vectors of size 1,
like `[10]` above.

### Rank-2 and higher arrays

Syntran has a more compact syntax for multi-rank arrays than Rust, which requires nested rank-1 arrays of rank-1 arrays.  As above, the sizes go after the semicolon `;`.  To initialize a rank-2 array with size 3 by 4 to all zeros:
<!-- syntran-begin mode=repl group=arr-matrix -->
```rust
let matrix = [0; 3, 4];
// [
// 0, 0, 0,
// 0, 0, 0,
// 0, 0, 0,
// 0, 0, 0
// ]
```
<!-- syntran-expect
[
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0
]
-->
<!-- syntran-end -->

Note that arrays are stored in [column-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order) as in Fortran, so they appear transposed when printing in the default format.

To initialize a rank-3 array with size rows by columns by sheets:
<!-- syntran-begin mode=repl group=arr-rank3 -->
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
<!-- syntran-end -->

Indices for multi-rank arrays are separated by commas:
<!-- syntran-begin mode=repl group=arr-rank3 -->
```rust
array[3,2,1];
// 0
```
<!-- syntran-expect
5
3
4
[
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 0, 0, 0
]
0
-->
<!-- syntran-end -->

To initialize a multi-rank array with an explicit list of values, separate the values with commas and then provide the size after a semicolon:
<!-- syntran-begin mode=repl group=arr-rank2explicit -->
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
<!-- syntran-expect
[
1, 2,
3, 4,
5, 6
]
6
-->
<!-- syntran-end -->

Many languages refer to these arrays as *multidimensional* arrays.  This can be
ambiguous, as a vector like `let v = [9, 16, 25]` is three-dimensional in at
least some sense, but only rank-1.  We will call these *multi-rank* arrays.

### Array slicing and indexing

Indexing an array with a range subscript, as opposed to a scalar subscript,
produces yet another array:
<!-- syntran-begin mode=repl group=array-slice -->
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
<!-- syntran-end -->

As with other expressions, the range bounds are inclusive of the lower bound and
exclusive of the upper bound.

Subscripting with just a colon `:` and no bounds returns the whole array along
that dimension:
<!-- syntran-begin mode=repl group=array-slice -->
```rust
v1[:];
// [0, 2, 4, 6, 8];
```
<!-- syntran-end -->

For rank-1 arrays, boundless whole-array slicing is not useful and less
performant than omitting the subscript `[]` expression altogether:
<!-- syntran-begin mode=repl group=array-slice -->
```rust
v1;
// [0, 2, 4, 6, 8];
```
<!-- syntran-end -->

You can also slice with a step in the form `lower_bound: step: upper_bound`:
<!-- syntran-begin mode=repl group=array-slice -->
```rust
v1[0: 2: 5];
// [0, 4, 8];
```
<!-- syntran-end -->
Using negative steps, you can reverse a vector:
<!-- syntran-begin mode=repl group=array-slice -->
```rust
v1[size(v1,0)-1: -1: -1];
// [8, 6, 4, 2, 0]
```
<!-- syntran-end -->

Arbitrary elements of an array with non-uniform steps can be extracted by using
another rank-1 array (i.e. vector) as a subscript:
<!-- syntran-begin mode=repl group=array-slice -->
```rust
v1[[0, 1, 3]];
// [0, 2, 6]
```
<!-- syntran-end -->
The double brackets might look strange.  Here, the outer brackets denote a
subscript or index of `v1`, while the inner brackets denote an array literal. If
you wrote `v1[0, 1, 3]`, that would work on a rank-3 array, but it would throw a
parser error for a rank-1 array `v1`. This might be more clear if we have a
helper index array variable with the same effect as the last example:
<!-- syntran-begin mode=repl group=array-slice -->
```rust
let indices = [0, 1, 3];
v1[indices];
/// [0, 2, 6]
```
<!-- syntran-expect
[0, 1, 2, 3, 4]
[1, 2, 3]
[0, 2, 4, 6, 8]
[2, 4, 6]
[0, 2, 4, 6, 8]
[0, 2, 4, 6, 8]
[0, 4, 8]
[8, 6, 4, 2, 0]
[0, 2, 6]
[0, 1, 3]
[0, 2, 6]
-->
<!-- syntran-end -->
As in Fortran, only rank-1 arrays can be used as an index array.  Of course,
they can index into an array of any rank, as shown in the [multi-rank
subsection](#multi-rank-array-slicing) below.

#### LHS and RHS slicing

Arrays can be sliced whether they are on the left-hand side (LHS) or the
right-hand side (RHS) of an assignment operator.  The examples above show RHS
slicing.

Assigning to an LHS slice changes only the sliced part of the array:
<!-- syntran-begin mode=repl group=array-lhs-rhs-1 -->
```rust
let v2 = [0: 5];
// [0, 1, 2, 3, 4]
v2[1: 4] = 7;
// [7, 7, 7]
v2;
// [0, 7, 7, 7, 4]
```
<!-- syntran-expect
[0, 1, 2, 3, 4]
[7, 7, 7]
[0, 7, 7, 7, 4]
-->
<!-- syntran-end -->
The value returned by the entire assignment expression above is just the
assigned slice `[7, 7, 7]`, not the whole `v2` array and not the scalar `7`.
This distinction is important when such an assignment expression is the
return value of a function (note that a subscripted assignment must be
parenthesized to appear directly after `return`, e.g. `return (v[1: 4] = 7);`),
or if an assignment is nested on the RHS of another assignment:
<!-- syntran-begin mode=repl group=array-lhs-rhs-2 -->
```rust
let v3 = [0: 5];
let v4 = v3[1: 4] = 7;
// [7, 7, 7]
v4;
// [7, 7, 7]
v3;
// [0, 7, 7, 7, 4]
```
<!-- syntran-expect
[0, 1, 2, 3, 4]
[7, 7, 7]
[7, 7, 7]
[0, 7, 7, 7, 4]
-->
<!-- syntran-end -->
Note that `v4` is the slice `[7, 7, 7]`, not the whole `v3` array (`v3` is
still updated in place, as shown above).  Nested subscripted assignments such
as this are [illegal in python](https://stackoverflow.com/a/60909096/4347028).

This behaviour is consistent with non-nested assignment: a subscripted
assignment expression evaluates to the same thing that the same subscript
would return on the RHS, whether that's a slice or, as shown below, a scalar:
<!-- syntran-begin mode=repl group=array-lhs-rhs-3 -->
```rust
let v5 = [0: 5];
let v6 = v5[1: 4];
// [1, 2, 3]
v6;
// [1, 2, 3]
v5[2] = 9;
// 9
```
<!-- syntran-expect
[0, 1, 2, 3, 4]
[1, 2, 3]
[1, 2, 3]
9
-->
<!-- syntran-end -->

#### Multi-rank array slicing

Rank-2 and higher arrays can also be sliced.  In general, this returns an array
of a different rank.  For example, slicing an array with 1 slice subscript `:`
and the rest scalar subscripts will return a rank-1 array.  Slicing an array
with 2 slice subscripts and the rest scalars will return a rank-2 array.

<!-- syntran-begin mode=repl group=array-multirank-slice -->
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
<!-- syntran-end -->

For every dimension of the array, you can mix and match scalar subscripts,
whole-array slices, range-based slices, stepped slices, or index-array slices:
<!-- syntran-begin mode=repl group=array-multirank-slice -->
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
<!-- syntran-expect
[
0, 1, 2,
3, 4, 5,
6, 7, 8,
9, 10, 11
]
[0, 1, 2]
x_slice = [0, 1, 2]
[1, 4, 7, 10]
y_slice = [1, 4, 7, 10]
[
3, 4,
6, 7,
9, 10
]
mat_slice = [
3, 4,
6, 7,
9, 10
]
[
0, 1, 2,
3, 4, 5,
9, 10, 11
]
[1, 7, 10]
-->
<!-- syntran-end -->

## Functions

This section is about user-defined functions.  [See this page](doc/) for a list of intrinsic syntran functions.

Use the `fn` keyword to declare a function, as in Rust.  Unlike Rust, use a colon `:` before the return type instead of `->`:

<!-- syntran-begin mode=file group=fn-add -->
```rust
fn add(a1: i32, a2: i32): i32
{
    return a1 + a2;
}
```
<!-- syntran-end -->

The function defined above could be used like this:

<!-- syntran-begin mode=file group=fn-add -->
```rust
let a = 3;
let b = 4;
let c = add(a + 1, b + 2);
// 10
```
<!-- syntran-end -->

Functions must be defined before they are called.  That means that recursive functions are not possible currently, neither with a function directly calling itself, nor with two functions which both call each other.

Here's a function that performs matrix-vector multiplication:
<!-- syntran-begin mode=file group=fn-mulmatvec -->
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
<!-- syntran-end -->
Checking the inner dimensions is left as an exercise for the reader.

Note that array rank is specified in function signatures with comma-separated colons.  For example, `vec` is a rank-1 array `[f64; :]` and `mat` is a rank-2 array `[f64; :,:]`.  Arrays of any size can be passed to functions, but ranks and types must match.  The use of a colon as a wildcard like this is [borrowed from Fortran](https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/specification-statements/type-declarations/declarations-for-arrays/assumed-shape-specifications.html).

Here's a function that performs [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication) on two matrices `a` and `b`:

<!-- syntran-begin mode=file group=fn-matmul -->
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
<!-- syntran-end -->

Then we can define [rotation matrices](https://en.wikipedia.org/wiki/Rotation_matrix) for 90 degree rotations about the _x_ and _y_ axes:

<!-- syntran-begin mode=file group=fn-matmul -->
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
<!-- syntran-end -->

Rotations can be composed by multiplying matrices, so we can apply a 180 degree _x_ rotation followed by a 180 degree _y_ rotation like this:

<!-- syntran-begin mode=file group=fn-matmul -->
```rust
println(mul_mat(mul_mat(mul_mat(rotx, rotx), roty), roty));
// [
// -1.000000000000000E+00, 0.000000000000000E+00, 0.000000000000000E+00,
// 0.000000000000000E+00, -1.000000000000000E+00, 0.000000000000000E+00,
// 0.000000000000000E+00, 0.000000000000000E+00, 1.000000000000000E+00
// ]
```
<!-- syntran-expect
[
-1.000000000000000E+00, 0.000000000000000E+00, 0.000000000000000E+00,
0.000000000000000E+00, -1.000000000000000E+00, 0.000000000000000E+00,
0.000000000000000E+00, 0.000000000000000E+00, 1.000000000000000E+00
]
-->
<!-- syntran-end -->

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
<!-- syntran-begin mode=file group=fn-ref -->
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
<!-- syntran-expect
x = 42
x = 43
-->
<!-- syntran-end -->

If you make a reference `&` in the declaration but not the caller, or
vice-versa, the parser will throw an error.

Only variable identifiers can be used as a reference.  Literals cannot be
referenced, and array elements and struct members cannot currently be
referenced.

### Function pointers (callbacks)

A function's name, used without a trailing `(...)`, evaluates to a **function
pointer** value.  Its type is written `fn(paramtypes): rettype`, mirroring the
`fn` declaration syntax itself.  Function pointers can be passed as arguments,
stored in variables, and called just like any other value:

<!-- syntran-begin mode=file group=fn-ptr -->
```rust
fn dbl(n: i32): i32
{
    return 2 * n;
}

fn apply(f: fn(i32): i32, x: i32): i32
{
    return f(x);
}

println(apply(dbl, 21));
// 42

let g = dbl;
println(g(10));
// 20
```
<!-- syntran-expect
42
20
-->
<!-- syntran-end -->

Omit the `: rettype` suffix for a `void`-returning function pointer, same as
for an ordinary `fn` declaration with no return type:

<!-- syntran-begin mode=file group=fn-ptr-void -->
```rust
fn hello()
{
    println("hi");
}

fn call_it(f: fn())
{
    f();
}

call_it(hello);
// hi
```
<!-- syntran-expect
hi
-->
<!-- syntran-end -->

A function pointer's signature is checked like any other argument type: the
number and types of parameters, and the return type, must match exactly
(no implicit numeric casting, same as elsewhere in syntran).

Limitations of the current implementation:
- Only user-defined functions can be pointed to.  Intrinsic functions
  (`abs`, `size`, `println`, ...) and struct methods cannot.
- A function with any `&`-reference parameter cannot be pointed to, since a
  function-pointer signature has no way to express reference-ness.
- Function-pointer parameters are always passed by value.
- The callee in an indirect call (`f(...)`) must be a plain variable name,
  not a more general expression like `arr[i](...)`.
- There are no closures or anonymous (lambda) functions; only a named,
  already-declared `fn` can be pointed to.
- Fn pointers cannot be stored in an array (`E89`) or a struct member (`E90`).

## Strings, printing, and file output

<!-- TODO: file input -->

The ASCII string type `str` uses `"`quotes`"` to assign literals:

<!-- syntran-begin mode=repl group=str-0 -->
```rust
let string0 = "hello world";
// hello world
```
<!-- syntran-expect
hello world
-->
<!-- syntran-end -->

To include an escaped quote literal, double it:

<!-- syntran-begin mode=repl group=str-1 -->
```rust
let string1 = "syntran is a ""programming language""";
// syntran is a "programming language"
```
<!-- syntran-expect
syntran is a "programming language"
-->
<!-- syntran-end -->

Alternatively, use a **raw string literal** (Rust-style) when a string contains
many quotes.  A raw string begins with `r` followed by zero or more `#` characters
and a `"`, and ends with a `"` followed by the same number of `#` characters.
Content is taken verbatim — no escape processing:

<!-- syntran-begin mode=repl group=str-1r -->
```rust
let string1r = r#"syntran is a "programming language""#;
// syntran is a "programming language"
```
<!-- syntran-expect
syntran is a "programming language"
-->
<!-- syntran-end -->

Choose a hash count that does not appear in the string content:

<!-- syntran-begin mode=repl group=str-json -->
```rust
let json = r##"{"key": "#value"}"##;
// {"key": "#value"}
```
<!-- syntran-expect
{"key": "#value"}
-->
<!-- syntran-end -->

The zero-hash form `r"..."` is also valid and ends at the first `"`:

<!-- syntran-begin mode=repl group=str-raw0 -->
```rust
let raw0 = r"no hashes needed here";
// no hashes needed here
```
<!-- syntran-expect
no hashes needed here
-->
<!-- syntran-end -->

Raw strings may span multiple lines; the newline characters become part of the
value:

<!-- syntran-begin mode=repl group=str-multi -->
```rust
let multi = r#"line one
line two"#;
```
<!-- syntran-expect
line one
line two
-->
<!-- syntran-end -->

Strings are concatenated with the `+` operator:

<!-- syntran-begin mode=repl group=str-concat -->
```rust
let string2 = "hello " + ("planet " + "earth");
// hello planet earth
```
<!-- syntran-expect
hello planet earth
-->
<!-- syntran-end -->

There is no separate character type, only strings of length 1.  Characters of a string are indexed in the same way as arrays:

<!-- syntran-begin mode=repl group=str-index -->
```rust
let string3 = "hello";
string3[0];
// h
string3[1];
// e
string3[2];
// l
```
<!-- syntran-expect
hello
h
e
l
-->
<!-- syntran-end -->

<!-- TODO: not TBD anymore! -->
Slice indexing for substrings of length > 1 is TBD.
To slice a substring, use a range:
<!-- syntran-begin mode=repl group=str-slice -->
```rust
let string4 = "01234567";
string4[2:5];
// 234
string4[3:6];
// 345
```
<!-- syntran-expect
01234567
234
345
-->
<!-- syntran-end -->

The intrinsic function [`str()`](doc/README.md#str) converts other types to `str` and concatenates them:

<!-- syntran-begin mode=repl group=str-str-fn -->
```rust
let string5 = "testing " + str(1, " ", 2, " ", 1.0, " ", false);
// testing 1 2     1.000000E+00 false
```
<!-- syntran-expect
testing 1 2     1.000000000000000E+00    false
-->
<!-- syntran-end -->

Integers and bools are stringified without padding, so separate them with `" "` if that's what you want.

Use [`println()`](doc/README.md#println) to print to stdout:

<!-- syntran-begin mode=repl group=str-println -->
```rust
println("hello world");
// hello world
```
<!-- syntran-expect
hello world
-->
<!-- syntran-end -->

Use [`open()`](doc/README.md#open), [`writeln()`](doc/README.md#writeln), and [`close()`](doc/README.md#close) to write to a file:

<!-- syntran-begin mode=file group=file-io -->
```rust
let file = open("test.txt", "w");
writeln(file, "hello world");
writeln(file, "here's a second line of text with a number ", 42);
close(file);

let fi = open("test.txt", "r");
println(readln(fi));
println(readln(fi));
close(fi);
```
<!-- syntran-expect
hello world
here's a second line of text with a number 42
-->
<!-- syntran-end -->

Only ASCII strings are supported because syntran is interpretted in Fortran.  Unicode strings cannot be indexed properly:

<!-- syntran-begin mode=skip reason="unicode indexing is explicitly documented as YMMV / mojibake" -->
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
<!-- syntran-end -->

## Modules

A module is any `.syntran` file containing functions, structs, or module-level
variables.  Modules are the recommended way to share code across syntran files.

### Defining a module

Here's a simple math module saved as `mymath.syntran`:

<!-- syntran-begin mode=file group=mod-basic-src dir=mod-basic file=mymath.syntran -->
```rust
// mymath.syntran

fn add(a: i32, b: i32): i32
{
    return a + b;
}

fn mul(a: i32, b: i32): i32
{
    return a * b;
}
```
<!-- syntran-end -->

### Importing a module

There are three import styles.

**Qualified import** — functions are called with the module name as a prefix:

<!-- syntran-begin mode=file group=mod-basic-q dir=mod-basic -->
```rust
use mymath;

println(mymath::add(2, 3));  // 5
println(mymath::mul(4, 5));  // 20
```
<!-- syntran-expect
5
20
-->
<!-- syntran-end -->

**Unqualified (glob) import** — all exported names are brought into scope
directly:

<!-- syntran-begin mode=file group=mod-basic-glob dir=mod-basic -->
```rust
use mymath::*;

println(add(2, 3));  // 5
println(mul(4, 5));  // 20
```
<!-- syntran-expect
5
20
-->
<!-- syntran-end -->

**Aliased import** — import the module under a shorter name:

<!-- syntran-begin mode=file group=mod-basic-alias dir=mod-basic -->
```rust
use mymath as mm;

println(mm::add(2, 3));  // 5
println(mm::mul(4, 5));  // 20
```
<!-- syntran-expect
5
20
-->
<!-- syntran-end -->

### Path resolution

Module paths are resolved relative to the **current source file's directory**.

<!-- syntran-begin mode=skip reason="demonstrates several module path-resolution forms against files outside this excerpt" -->
```rust
use ./mymath;            // explicit current directory
use ../mymath;           // parent directory
use math/vectors;        // subdirectory — qualified as math::vectors::fn_name
use math/vectors as vec; // subdirectory with alias — qualified as vec::fn_name
use math/vectors::*;     // subdirectory with glob
```
<!-- syntran-end -->

### Module variables

A module may declare top-level variables with `let`.  These are shared across
all imports of that module and can be read or reassigned by the importer.

<!-- syntran-begin mode=file group=mod-counter-src dir=mod-counter file=counter.syntran -->
```rust
// counter.syntran

let count = 0;

fn increment(): i32
{
    count = count + 1;
    return count;
}

fn get_count(): i32
{
    return count;
}
```
<!-- syntran-end -->

<!-- syntran-begin mode=file group=mod-counter-main dir=mod-counter -->
```rust
// main program
use counter::*;

println(count);         // 0
println(increment());   // 1
println(count);         // 1
count = 10;
println(get_count());   // 10
```
<!-- syntran-expect
0
1
1
10
-->
<!-- syntran-end -->

### Exported structs

Structs defined in a module are exported along with functions:

<!-- syntran-begin mode=file group=mod-struct-src dir=mod-struct file=struct_mod.syntran -->
```rust
// struct_mod.syntran

struct Point
{
    x: i32,
    y: i32,
}

fn make_point(x: i32, y: i32): Point
{
    return Point{x = x, y = y};
}
```
<!-- syntran-end -->

<!-- syntran-begin mode=file group=mod-struct-main dir=mod-struct -->
```rust
use struct_mod::*;

let p = make_point(3, 4);
println(p.x);  // 3
println(p.y);  // 4
```
<!-- syntran-expect
3
4
-->
<!-- syntran-end -->

### Exported enums

Enums defined in a module are exported the same way, and can be accessed
either unqualified (via a glob import) or qualified with the module name:

<!-- syntran-begin mode=file group=mod-enum-src dir=mod-enum file=suit_mod.syntran -->
```rust
// suit_mod.syntran

enum Suit
{
    Hearts,
    Diamonds,
    Clubs,
    Spades,
}
```
<!-- syntran-end -->

<!-- syntran-begin mode=file group=mod-enum-glob dir=mod-enum -->
```rust
// unqualified (glob import)
use suit_mod::*;
println(Suit.Clubs);  // Suit.Clubs
```
<!-- syntran-expect
Suit.Clubs
-->
<!-- syntran-end -->

<!-- syntran-begin mode=file group=mod-enum-qualified dir=mod-enum -->
```rust
// qualified
use suit_mod;
println(suit_mod::Suit.Spades);  // suit_mod::Suit.Spades
```
<!-- syntran-expect
suit_mod::Suit.Spades
-->
<!-- syntran-end -->

### Valid module names

Module names follow the same rules as identifiers: they may contain letters,
digits, and underscores, but must not start with a digit.  Hyphens, spaces, and
language keywords are not allowed as module names or aliases.  The name `std` is
additionally reserved for the syntran standard library.

<!-- syntran-begin mode=skip reason="illustrates module-naming-rule errors; the first `ok` import depends on a my_math module file not shown in this excerpt" -->
```rust
use my_math;      // ok
use my-math;      // Error: hyphens not allowed in module names
use fn;           // Error: `fn` is a keyword
use std;          // Error: `std` is reserved
```
<!-- syntran-end -->

---

For the older `#include` preprocessing directive (still supported but
superseded by modules), see [doc/include-files.md](doc/include-files.md).

## Structs

Structs, also known as user-defined types or derived types, can be used in
syntran.  The declaration or definition of a struct in syntran uses similar
syntax as rust:
<!-- syntran-begin mode=file group=structs -->
```rust
struct Point
{
    x: [i32; :],
    name: str,
}
```
<!-- syntran-end -->

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
<!-- syntran-begin mode=file group=structs -->
```rust
let pt0 = Point{x = [20, 10], name = "my-pt0"};
let pt1 = Point{x = [40, 50], name = "my-pt1"};
```
<!-- syntran-end -->

Unlike rust, members are assigned using an assignment operator `=`, not a colon
`:`.  Instantiating a struct, just like instantiating any other primitive type
variable such as `let x = 42;`, is a statement.  Hence, the instantiation
statement ends with a semicolon `;`.

When a struct is initialized, _all_ of its members must be initialized.  If you
want to have a struct with some default values that you don't want to explicitly
define every time, you can use a helper function to construct it.

Structs can be nested.  We can build upon the 1st order `Point` struct by
declaring a `Rect` struct, some of whose members are also structs:
<!-- syntran-begin mode=file group=structs -->
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
<!-- syntran-end -->

Functions can take struct arguments and return struct values.  For example, here
is a function that computes the area of a rectangle:
<!-- syntran-begin mode=file group=structs -->
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
<!-- syntran-end -->

Here is a function that constructs and returns a `Rect`:
<!-- syntran-begin mode=file group=structs -->
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
<!-- syntran-expect
area = 800
unit area = 1
-->
<!-- syntran-end -->

### Methods

Instead of writing a free function that takes a struct argument, you can
attach behavior directly to a struct by declaring a method.  Methods use the
same `fn` syntax as free functions, but they are declared *inside* the struct
body, after the data members.  There is no explicit `self` or receiver
parameter -- a method's body refers to the struct's own members directly by
name.  Methods are called using dot syntax on an instance, just like accessing
a member:
<!-- syntran-begin mode=file group=struct-methods -->
```rust
struct Counter
{
    n: i32,

    fn inc()
    {
        n += 1;
    }

    const fn get(): i32
    {
        return n;
    }
}

let c = Counter{n = 0};
c.inc();
c.inc();
println(c.get());
// 2
```
<!-- syntran-expect
2
-->
<!-- syntran-end -->
Note that instantiation still uses the members-only initializer syntax
`Counter{n = 0}` -- methods are not listed there, only data members.

A method that does not modify any of the struct's members, like `get` above,
is declared `const fn`.  A method that mutates a member, like `inc`, is
declared as a plain `fn`.  Just like free functions, a method with no return
value omits the `: type` return annotation, while a method that returns a
value declares one with `fn name(args): type`.

Methods can take arguments just like free functions, including by-reference
arguments marked with `&`:
<!-- syntran-begin mode=file group=struct-methods-args -->
```rust
struct Acc
{
    val: i32,

    fn add(x: i32)
    {
        val += x;
    }

    fn add_ref(x: &i32)
    {
        val += x;
        x   += 1;
    }

    const fn scaled(x: i32): i32
    {
        return val * x;
    }
}

let a = Acc{val = 0};
a.add(7);

let x = 3;
a.add_ref(&x);
// x is incremented inside the method, just like a normal by-ref argument
println(a.val, " ", x);
// 10 4

// Return values can be used in a larger expression
println(a.scaled(2));
// 20
```
<!-- syntran-expect
10 4
20
-->
<!-- syntran-end -->

There are a couple of restrictions on methods.  A mutating (non-`const`)
method cannot be called on a temporary struct returned directly from a
function call, e.g. `make_acc(5).add(1);`, since the mutation would have
nowhere to persist and be silently discarded.  Also, a method cannot share
its name with one of the struct's data members.

Structs can be nested arbitrarily.  You can make structs of arrays and arrays of
structs.  One noteable missing feature currently is that structs of arrays (and
arrays of structs) cannot be sliced, only scalar subscripts are supported.

Here is a contrived example of a high-order struct, including arrays at various
levels:
<!-- syntran-begin mode=skip reason="current interpreter rejects a bare `_` identifier as a struct member name (E2); tracked as an interpreter bug, not a doc issue" -->
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
<!-- syntran-end -->

## Enums

Enums declare a named type with a fixed set of integer-valued variants:
<!-- syntran-begin mode=file group=enums -->
```rust
enum Suit
{
    Hearts,
    Diamonds,
    Clubs,
    Spades,
}
```
<!-- syntran-end -->

Just like struct members, variants are delimited by a comma `,` and the
trailing comma after the last variant is optional.  There is no semicolon
after the closing `}`.

Variants are accessed with dot syntax on the enum's type name, not on an
instance:
<!-- syntran-begin mode=file group=enums -->
```rust
let s = Suit.Clubs;
```
<!-- syntran-end -->

By default, variants are assigned consecutive integer values starting at `0`
in declaration order (`Hearts` is `0`, `Diamonds` is `1`, etc.).  A variant can
pin an explicit value with `= <intlit>`; later variants without an explicit
value continue counting up from there:
<!-- syntran-begin mode=file group=enums -->
```rust
enum Card
{
    Two,        // 0
    Three,      // 1
    Jack = 10,  // 10
    Queen,      // 11
    King,       // 12
}
```
<!-- syntran-end -->

The explicit value may be negative, e.g. `Below = -1`, and auto-increment
continues from it normally:
<!-- syntran-begin mode=file group=enums -->
```rust
enum Temp
{
    Below = -1,  // -1
    Freezing,    //  0
    Above,       //  1
}
```
<!-- syntran-end -->

Each enum is a distinct type: two enums are never interchangeable, even if
they happen to declare the same variant names.  Comparing or passing a
mismatched enum type is a compile-time error, just like any other type
mismatch:
<!-- syntran-begin mode=file group=enums -->
```rust
enum Dir{North, South}
enum Signal{North, South}

let d = Dir.North;
// d == Signal.North;  // error: mismatched types
```
<!-- syntran-end -->

Enum values only support equality comparison, `==` and `!=`.  To use a
variant as an integer -- e.g. for ordering, arithmetic, or array indexing --
convert it explicitly with `i32()`:
<!-- syntran-begin mode=file group=enums -->
```rust
let a = [10, 20, 30, 40];
println(a[i32(Suit.Clubs)]);
// 30
```
<!-- syntran-end -->

The reverse cast, from an integer ordinal back to an enum variant, uses the
enum's type name as a call: `EnumName(ordinal)`.  It's an error if no variant
has that value -- at parse time (`E96`) for a constant literal argument, or
at runtime (`R32`) otherwise:
<!-- syntran-begin mode=file group=enums -->
```rust
println(Suit(2));
// Suit.Clubs

// Suit(99);  // error: no variant with value 99 in enum `Suit`
```
<!-- syntran-end -->

Printing an enum value shows its qualified name, `EnumType.Variant`:
<!-- syntran-begin mode=file group=enums -->
```rust
println(Suit.Diamonds);
// Suit.Diamonds
```
<!-- syntran-end -->

Enums can be used anywhere a type is expected: as function parameters and
return types, and as struct members:
<!-- syntran-begin mode=file group=enums -->
```rust
struct Player
{
    name: str,
    suit: Suit,
}

fn describe(s: Suit): str
{
    return str(s);
}

let p = Player{name = "Alice", suit = Suit.Spades};
println(describe(p.suit));
// Suit.Spades
```
<!-- syntran-end -->

Arrays of enums work like arrays of any other type, including indexing,
assignment, `[value; n]` uniform arrays, and iterating a literal array with
`for`:
<!-- syntran-begin mode=file group=enums -->
```rust
let hand = [Suit.Hearts, Suit.Clubs, Suit.Spades];
hand[0] = Suit.Diamonds;
println(hand);
// [Suit.Diamonds, Suit.Clubs, Suit.Spades]
```
<!-- syntran-end -->

A bare enum type name acts as an array of all its variants, in declaration
order (aliases included), but only in the handful of places that consume it
directly -- there's no separate syntax to learn for the enum's variant count
or iteration, they fall out of the same array machinery as above:
<!-- syntran-begin mode=file group=enums -->
```rust
println(size(Suit));
// 4

for s in Suit
{
    println(s);
}
// Suit.Hearts
// Suit.Diamonds
// Suit.Clubs
// Suit.Spades
```
<!-- syntran-end -->

Note that `len()` is not overloaded for enums (or arrays in general) -- it's
reserved for `str`.  Use `size()` for a container's length, same as for any
other array.

Outside of `for`'s iterable and an argument to `size()`/`str()`/`println()`/
`writeln()`, a bare enum name is *not* a value: it can't be bound with
`let`/`const`, assigned, returned, passed to a user function, or stored in an
array or struct literal (`E99`).  Write out the variants explicitly wherever
an actual array value is needed:
<!-- syntran-begin mode=file group=enums -->
```rust
let all = [Suit.Hearts, Suit.Diamonds, Suit.Clubs, Suit.Spades];
                  // an ordinary [Suit; 4] array value: assignable, passable,
                  // indexable, iterable -- `let all = Suit;` is E99
```
<!-- syntran-expect
30
Suit.Clubs
Suit.Diamonds
Suit.Spades
[Suit.Diamonds, Suit.Clubs, Suit.Spades]
4
Suit.Hearts
Suit.Diamonds
Suit.Clubs
Suit.Spades
-->
<!-- syntran-end -->

A bare enum name can't be subscripted, either -- `Suit[0]` is a compile-time
error (`E97`), since it would disagree with the by-value reverse cast
`Suit(0)` whenever any variant has an explicit value.  Use `Suit(ordinal)` to
go from an integer ordinal to a variant.

A variable can never share a name with an enum or struct type -- both a bare
enum reference and a struct instantiator depend on resolving a bare name
against the type namespaces, so declaring either in either order is a
compile-time error (`E98`):
<!-- syntran-begin mode=file group=enum-name-clash -->
```rust
enum Suit{Hearts, Clubs}
let Suit = 5;
// Error[E98]: variable `Suit` conflicts with the enum of the same name
```
<!-- syntran-expect
Error[E98]: variable `Suit` conflicts with the enum of the same name
  --> main_enum-name-clash.syntran:2:5
   |
 2 | let Suit = 5;
   |     ^^^^ name already used by an enum
-->
<!-- syntran-end -->

## Samples

Many syntran samples are provided in this repository and elsewhere:

1.  [Inline tests](src/tests/test.f90):  these are short one-to-few line syntran snippets, embedded in Fortran as a string and `eval`'d.  They are covered by the tests
2.  [Script tests](src/tests/test-src):  these are longer syntran scripts, organized into categories by directory.  They are covered by the tests
3.  [Samples](samples):  these are longer syntran scripts which can also take a while to run, e.g. the wave equation solvers.  They are *not* covered by the tests because of the time they take to run
4.  [Advent of Code](https://github.com/JeffIrwin/aoc-syntran):  these are syntran scripts which solve problems from the [Advent of Code](https://adventofcode.com/about).  They are *not* covered by tests and in a separate repository

