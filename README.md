
![](https://github.com/JeffIrwin/syntran/workflows/CI/badge.svg)

# Syntran

[Syntax translator](https://www.practo.com/medicine-info/syntran-100-mg-capsule-18930)

An interpreter written in Fortran, I guess

> Jerry: Fungicide. I mean what could she have?
>
> Elaine: I don't know.
>
> Kramer: Fungus.

This is a sandbox for me to play in as I follow along with [Immo Landwerth's _building a compiler_ series](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y)

## Build the interpreter

Using cmake:

```
./build.sh
```

A [Fortran compiler](https://fortran-lang.org/en/compilers/) and either [CMake](https://cmake.org/download/) or [FPM](https://fpm.fortran-lang.org/index.html) are required

Two independent build systems are provided for syntran.  You can either use cmake, which is run by `build.sh` as shown above, or you can use the Fortran Package Manager `fpm`:

```
fpm build
```

Other `fpm` commands are available, such as `fpm test`, `fpm run`, `fpm install`, etc.  Most of the example commands in this documentation will assume that cmake was used, but there is usually (always?) an fpm alternative.

## Run

Start the interpreter:

```
./build/syntran
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

Integer `i32` and `i64`, float `f32`, string `str`, and Boolean `bool` types are supported.  Attempting operations on the wrong types yields an error, e.g. trying to add a bool or use logical `and` on an int or float.

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

## Comments

Only single-line `// comments` are supported.  There are _no_ multi-line `/*comments*/`.

## Arrow keys and command history

In many shells such as `bash`, the up and down arrow keys can be used to scroll through the command history.  For example, hit the up arrow key and then ENTER to repeat the previous command, or hit the up arrow key twice to go two commands back.

Arrow keys do _not_ work by default in syntran running within bash, but there is a simple and powerful workaround with `rlwrap`.  If you run `syntran` by itself, it processes text in cooked mode:

```
./build/syntran
```

Cooked mode means that `syntran` does not read or process any text until after you press the ENTER key.  Further, when you use an arrow key, you will see escape sequences like this:

```
^[[A
```

These keypresses may appear like the ANSI escape sequence above or other garbled text.

To overcome this, install and run `rlwrap` with `syntran`:

```
sudo apt install rlwrap
rlwrap ./build/syntran
```

With `rlwrap`, the arrow keys work as expected within the `syntran` interpretter.  Plus, you get `rlwrap`'s other powerful features, like Ctrl+R for history search and saved history across separate invocations of `syntran`.

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

## Saving scripts in a file

As programs get longer and more complicated, it becomes difficult to enter them into the interactive interpreter.  To interpret a whole file, provide it as a command line argument:

```
./build/syntran samples/primes-1.syntran
```

<!--
Note: global block statement is not required as of 0.0.13.  Multiple statements (and functions) are parsed at the global scope.

Make sure to wrap the entire script in a main block with braces `{}`.  The global block `{}` is not required when interactively using the interpreter because it parses and evaluates one statement at a time.  However, if you forget the global block `{}` in a script file, only the first statement will be parsed and any trailing junk statements will be unexpected.
-->

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

When the clause of the if statement is only a single line, braces `{}` are optional.

The bounds of for loops, like ranges in Rust and Python, are inclusive of the lower bound and exclusive of the upper bound:

```cpp
for i in [0: 5]
	i;
// 0, 1, 2, 3, 4
```

## Example:  calculating prime numbers inefficiently

With only these language features, we can make a short program to find prime numbers:

```cpp
// Get the largest prime number less than n
let n = 100;

// Initialize the largest prime found so far
let prime = 0;

// This check is O(n**2) time, which might be the best we can do without
// having arrays in the language yet

// If we had while loops, we could loop from n downards and stop as soon as
// we find the first prime
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
prime;
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

prime;
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
	pi = pi + 1 / (16.0 ** k) *
		(
			4.0 / (8*k + 1) -
			2.0 / (8*k + 4) -
			1.0 / (8*k + 5) -
			1.0 / (8*k + 6)
		);
}

pi;
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
	sinx = sinx + sign * xpow / factorial;
	xpow = xpow * x ** 2;
	factorial = factorial * (2*k) * (2*k + 1);
	sign = -sign;
}

sinx;
// 4.999999E-01
```

At the end of all those transcendental functions and numbers, we get the suprisingly rational result `sin(pi / 6) == 0.5`, or `4.999999E-01` with 32 bit floats.

### Notes on casting in arithmetic expressions

There are several operations you have to be careful with in the sine example above.

In the pi series, there is a float term `16.0 ** k`.  The loop iterator `k` is an integer, so if we used a literal integer `16` instead of the float `16.0`, that would quickly overflow even for the relatively small upper loop bound `k < 10`.  Hence, we raise a float base to an int power, which yields a float result that is safe from overflow for these values.

Similarly, the pi term `4.0 / (8*k + 1)` has a float numerator and int denominator.  Again we must use a float to avoid integer division.

Syntran is not a [nanny language](https://retrocomputing.stackexchange.com/a/15379/26435), but it allows you to do numeric work without constantly manually casting things [`as f32` like in Rust](https://doc.rust-lang.org/rust-by-example/types/cast.html).

## Arrays

Recall the syntax for a for-loop:
```rust
for i in [0: 5]
	i;
// 0, 1, 2, 3, 4
```

The expression `[0: 5]` is one of several array forms, which can also be assigned to variables:
```rust
let v0 = [0: 5];
// [0, 1, 2, 3, 4]
```

Array sizes do not need to be literals or constants.  Arrays are allocated dynamically at runtime.

Besides ranges of consecutive integers, which is the only form that can be used in a for-loop for now, there are other array forms.

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

### Rank-2 and higher arrays

Syntran has a more compact syntax for higher-rank arrays than Rust, which requires nested rank-1 arrays of rank-1 arrays.  As above, the sizes go after the semicolon `;`.  To initialize a rank-2 array with size 3 by 4 to all zeros:
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

Indices for higher-rank arrays are separated by commas:
```rust
array[3,2,1];
// 0
```

To initialize a higher-rank array with an explicit list of values, separate the values with commas and then provide the size after a semicolon:
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

## Functions

This section is about user-defined functions.  [See this page](doc/) for a list of intrinsic syntran functions.

Use the `fn` keyword to declare a function, as in Rust.  Unlike Rust, use a colon `:` before the return type instead of `->`:

```rust
fn add(a1: i32, a2: i32): i32
{
	a1 + a2;
}
```

The final statement of a function implicitly defines the return value.  The function defined above could be used like this:

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
fn mul_mat_vec(mat: [f32; :,:], vec: [f32; :]): [f32; :]
{
	// Matrix-vector multiplication.  Return mat * vec
	let ans =  [0.0; size(mat,0)];
	for     j in [0: size(mat,1)]
		for i in [0: size(mat,0)]
			ans[i] = ans[i] + mat[i,j] * vec[j];
	ans;
}
```

Note that array rank is specified in function signatures with comma-separated colons.  For example, `vec` is a rank-1 array `[f32; :]` and `mat` is a rank-2 array `[f32; :,:]`.  Arrays of any size can be passed to functions, but ranks and types must match.  The use of a colon as a wildcard like this is [borrowed from Fortran](https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/specification-statements/type-declarations/declarations-for-arrays/assumed-shape-specifications.html).

Here's a function that performs [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication) on two matrices `a` and `b`, without checking that the inner dimensions agree:

```rust
fn mul_mat(a: [f32; :,:], b: [f32; :,:]): [f32; :,:]
{
	let c = [0.0; size(a,0), size(b,1)];
	for         k in [0: size(b,1)]
		for     j in [0: size(a,1)]
			for i in [0: size(a,0)]
				c[i,k] = c[i,k] + a[i,j] * b[j,k];
	c;
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

	// return
	if (found)
		i -= 1;
	else
		i = -1;
}
```

This can be included in a main program, assuming `main.syntran` and `header.syntran` are in the same folder:

```rust
// main.syntran

#include("header.syntran");

fn main()
{
	my_global;
	// 42

	my_scan("012345", "2");
	// 2

	my_scan("012345", "3");
	// 3
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

## Samples

Many syntran samples are provided in this repository and elsewhere:

1.  [Inline tests](src/tests/test.f90):  these are short one-to-few line syntran snippets, embedded in Fortran as a string and `eval`'d.  They are covered by the tests
2.  [Script tests](src/tests/test-src):  these are longer syntran scripts, organized into categories by directory.  They are covered by the tests
3.  [Samples](samples):  these are longer syntran scripts which can also take a while to run, e.g. the wave equation solvers.  They are *not* covered by the tests because of the time they take to run
4.  [Advent of Code](https://github.com/JeffIrwin/aoc-syntran):  these are syntran scripts which solve problems from the [Advent of Code](https://adventofcode.com/about).  They are not covered by tests and in a separate repository

