
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

## Build

    ./build.sh

A [Fortran compiler](https://fortran-lang.org/en/compilers/) and [CMake](https://cmake.org/download/) are required

## Run

Start the interpreter:

    ./build/syntran

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

### Variables, booleans, and type checking

Variable declarations use the [`let` keyword](https://doc.rust-lang.org/std/keyword.let.html) as in Rust.  This is also similar to JavaScript, except there is no `var` keyword.  Variables are mutable.

Integer and boolean types are supported.

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
//
// foo and p
//     ^^^
// Error: binary operator "and" is not defined for types num_expr and bool_expr
// 

```

Logical keywords `true`, `false`, `not`, `and`, and `or` are like Fortran's (e.g. `.true.`) but without the dots.  Note that they are lower case-sensitive, unlike Python (e.g. `True`).

### Comments

Only single-line comments are supported.  There are _no_ multi-line `/*comments*/`.

### Saving scripts in a file

As programs get longer and more complicated, it becomes difficult to enter them into the interactive interpreter.  To interpret a whole file, provide it as a command line argument:

    ./build/syntran samples/primes1.syntran

Make sure to wrap the entire script in a main block with braces `{}`.

### If statements and for loops

If, else if, and else statements work like you might expect for languages similar to C.  Like Rust, parentheses around the if condition are optional:

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
let i = 0;
for i in [0: 5]
    i;
// 0, 1, 2, 3, 4
```

## Calculating prime numbers inefficiently 

With only these language features, we can make a short program to find prime numbers:

```cpp
{
	// Get the largest prime number less than n
	let n = 100;

	// Initialize the largest prime found so far
	let prime = 0;

	// This check is O(n**2) time, which might be the best we can do without
	// having arrays in the language yet

	// If we had while loops, we could loop from n downards and stop as soon as
	// we find the first prime
	let i = 0;
	for i in [0: n]
	{
		let j = 0;

		// Check if i is composite, i.e. not prime
		let is_composite = false;

		// Largest possible divisor of i is i/2.  Actually it's sqrt(i) but
		// I don't have a sqrt fn yet
		for j in [2: i/2 + 1]
		{
			// Is i divisible by j?
			let divisible = j * (i / j) == i;  // poor man's modulo == 0
			is_composite = is_composite or divisible;
		}

		if not is_composite
			prime = i;
	}

	// Final result
	prime;
}
```
