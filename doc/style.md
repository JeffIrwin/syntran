# Syntran style guide

This is the suggested style of syntran source code.  Follow it in the
self-hosted compiler in [`compiler/`](../compiler/) and in new code.  The unit
tests under `src/tests/test-src/` are exempt, since many of them exist to
exercise odd syntax on purpose.

The checkable rules are enforced by a [linter](#the-linter), which is written in
syntran.

The examples below are indented with tabs, like real source.

## Files and layout

- A file starts with a comment that says what the file is and how it relates to
  the rest.  The file ends with a newline
- Then `use` lines, then the code
- A script ends with `fn main()`, then a call to it on the last line, `main();`
- Filenames are lowercase.  A module file has to be a valid identifier, so it
  uses underscores, e.g. `str_vec.syntran`.  Scripts and tests, which nothing
  imports, can use hyphens, e.g. `lex-dump.syntran` and `test-01.syntran`

## Formatting

- **Tabs** indent.  Spaces are fine after the first non-whitespace character,
  e.g. to align columns on consecutive lines, but they never indent a line
- **Allman braces**: `{` goes on its own line, at the indent of the statement
  that it belongs to.  A short body on one line is fine as long as the `{` is
  not the last thing on a line that has code before it:

  ```rust
  switch text
  {
  	case "true"  { return Keyword.true_; }
  	case "false" { return Keyword.false_; }
  }
  ```

- No parentheses around the condition of an `if` or `while`, and no
  parentheses around a `for` range
- A body that is one statement doesn't need braces.  A body of several
  statements does.  If any branch of an `if`/`else` chain needs braces, they
  all have them:

  ```rust
  if cap > 0
  	new_cap = cap * 2;

  if float and current() == "f"
  {
  	float32 = true;
  	pos += 1;
  }
  else if float
  {
  	float64 = true;
  }
  ```

- Slices and ranges have a space after the colon, like `s[a: b]` and
  `[0: len(s)]`.  A colon is spaced like a comma, and like the colon in a type
  annotation or struct declaration.  An open end leaves it off, like `s[i:]`
- A line is at most 80 columns.  A tab is 4 columns wide for counting
- To wrap a fn signature or a call that is too long, open the paren, put the
  params or args on indented lines, and close with `)` at the indent of the
  `fn` or the statement:

  ```rust
  fn lex_radix(
  	start: i32, radix_name: &const str, k: i32,
  	err32: &const str, err64: &const str,
  ): Token
  ```

- A struct literal is one field per line, with a trailing comma, unless it is
  short enough to stay on one line:

  ```rust
  return TokenVec
  {
  	len = 0,
  	cap = 0,
  	data = [new_token(TokenKind.eof, 0, ""); 0],
  };
  ```

  The same goes for a multi-line array literal
- No trailing whitespace

## Naming

| What                              | Case                     | Examples                              |
|-----------------------------------|--------------------------|---------------------------------------|
| struct, enum                      | `CamelCase`              | `Token`, `Lexer`, `TokenKind`         |
| enum variant                      | `snake_case`             | `double_equals`, `Keyword.let_`       |
| fn, method                        | `snake_case`             | `lex_all`, `push_diag`                |
| variable, param, struct member    | `snake_case`             | `new_cap`, `has_val`                  |
| `const`                           | `snake_case` or `UPPER_SNAKE_CASE` | `filename`, `STACK_CAP`      |
| module (the filename)            | `snake_case`             | `lex`, `utils`                        |

A `const` may be `UPPER_SNAKE_CASE`, at the top level or in a fn, but it doesn't
have to be.  A `let` is always `snake_case`.

Besides the case:

- A trailing `_` avoids a clash with a keyword, a type name, or an intrinsic,
  like `Keyword.let_`, `str_`, `i32_`, and `sum_`
- A container that is specialized by its element puts the element first:
  `StrVec`, `TokenVec`
- A predicate starts with `is_` or `has_`, like `is_digit()` and
  `has_radix_digit()`
- A constant that is a capacity ends in `_CAP`, like `STACK_CAP`
- When code is a port of the Fortran source, keep the Fortran name (without the
  `_t` of a type), so that the two can be read side by side, like
  `compiler/lex.syntran` and `src/lex.f90`

## Declarations

- Variables have no type annotation.  Types are inferred
- Use `const` for anything that isn't reassigned, and `let` for what is
- A parameter that a fn only reads is `&const`, which avoids a copy.  A
  parameter that it modifies is `&`.  Small scalars are passed by value
- A method that doesn't change any member is a `const fn`
- Prefer `std::move()` to a copy when handing over an array that you're done
  with, like when growing a vector
- Always call the standard library with the `std::` prefix, e.g.
  `std::parse_i64()`.  Don't `use std::*;`, since a future version of syntran
  may add a function that clashes with one of yours

## Modules, structs, and constructors

- A module has **one main struct**, like a class, and the module is named
  after it in `snake_case`.  `token.syntran` has `struct Token`.  Enums,
  helpers, and smaller structs that the main struct needs can sit beside it
- The main struct's constructor is called **`new()`**, with no type in the name,
  since the module already says it.  A smaller struct's constructor is
  `new_<type>()`, like `new_span()` for a `Span`
- A consumer imports the module with a **qualified** `use token;` and refers to
  everything through the module name.  Avoid `use token::*;`

```rust
// token.syntran

enum Kind
{
	num,
	word,
}

struct Token
{
	kind: Kind,
	text: str,

	const fn is_word(): bool
	{
		return kind == Kind.word;
	}
}

fn new(kind: Kind, text: str): Token
{
	return Token
	{
		kind = kind,
		text = text,
	};
}
```

```rust
// main.syntran

use token;

fn show(t: &const token::Token)
{
	println(t.text, " ", t.is_word());
}

const t = token::new(token::Kind.word, "hi");
show(&t);
```

Qualified names are the point: they say where a thing is from at the call site,
they can't clash, and one main struct per module keeps them short enough that
nobody wants the glob.

## Comments

- Use `//` only.  There are no block comments, and the language is not going to
  get them.  Comment out a range of lines with block insertion in your editor
- Put a comment inside every fn, struct, and enum whose purpose or edge cases
  aren't obvious from the name, as the first thing in its body, not on the line
  above it.  Describe what it returns or does:

  ```rust
  fn keyword_of(text: &const str): Keyword
  {
  	// The keyword that `text` is, or Keyword.none for an identifier
  	switch text
  	{
  		...
  	}
  }
  ```

- Write sentences, with two spaces after a period.  A comment of one sentence
  doesn't need a period at the end.  Quote code in `backticks`
- A comment on the same line as code is separated from it by two spaces, like
  `pos += 2;  // skip the prefix`
- Say why, or what is surprising.  The code already says what.  For example the
  `at()` helper in `compiler/utils.syntran` is there because `and` and `or`
  don't short-circuit

## The linter

[`compiler/lint.syntran`](../compiler/lint.syntran) checks the rules below with
the syntran lexer, [`compiler/lex.syntran`](../compiler/lex.syntran).  Run it
with `utils/lint.sh`, which finds the interpreter, takes files or dirs (default
`compiler/`), and can compile the linter natively:

```bash
bash utils/lint.sh                              # lints compiler/
bash utils/lint.sh build/v1/bin/syntran samples/foo.syntran
bash utils/lint.sh --native build/native/bin/syntran compiler
```

It prints `path:line:col: rule: message` for each violation, sorted, and exits
1 if there are any.  Directly, with no wrapper:
`syntran -q compiler/lint.syntran -- <file>...`.  CI keeps `compiler/` clean.

| Rule             | Checks                                                                 |
|------------------|------------------------------------------------------------------------|
| `struct-name`    | name after `struct` is `CamelCase`                                     |
| `enum-name`      | name after `enum` is `CamelCase`                                       |
| `variant-name`   | enum variants are `snake_case`                                         |
| `fn-name`        | name after `fn` is `snake_case`                                        |
| `param-name`     | fn params are `snake_case`                                             |
| `member-name`    | struct members are `snake_case`                                        |
| `var-name`       | `let`, `const`, and `for` names are `snake_case`, and a `const` may be `UPPER_SNAKE_CASE` too |
| `brace-eol`      | `{` is not at the end of a line that has code before it                |
| `indent`         | a line doesn't start with a space                                      |
| `trailing-ws`    | no whitespace at the end of a line                                     |
| `crlf`           | no CR LF line endings, reported once per file                          |
| `line-length`    | at most 80 columns, with a tab as 4                                    |
| `final-newline`  | the file ends with a newline                                           |

Not checked, because they take judgment: comments, braces on single statements,
`const` vs `let`, `&const` params, the `std::` prefix, and everything in
[the section on modules](#modules-structs-and-constructors).

Names in a test that exists to exercise odd syntax, such as the files in
`src/tests/test-src/errors/`, will trip it.  That's expected, and they are not
meant to be linted.
