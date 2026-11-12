# Port the parser to syntran: stage 1 (valid programs, exact typed tree)

## Context

`scratch/self-host.md` item 4: after the lexer (`compiler/lex.syntran`, done and
diff-tested on 688 files), port the parser.  The Fortran "parser" is really the
whole front end: two full passes (pass 0 throws its result away, pass 1 reparses
with the tables it filled), with name resolution, slot allocation, type
propagation, desugaring, module loading, and ~2/3 of its ~10k lines in semantic
checks.  Syntax depends on semantics: `Foo {` is a struct literal only if `Foo`
is a struct, `.` is parsed only after a struct-typed expr, method receivers move
into `args(1)`, bare fields in methods become `0self.field`, intrinsic calls get
overload-mangled (`0min_i32`), and printf becomes `print(format(...))`.

Decisions you made: port it faithfully, in stages.  **Stage 1** (this plan):
every valid program gives the **identical typed tree** to the Fortran parser,
plus the syntax diagnostics.  **Stage 2** (a later plan) adds the semantic
diagnostics (type mismatches, `check_*` routines, suggestions, the `errors/`
corpus).  Stage 1 is big, so it lands in milestones, each one a commit, gated by
a pass list like `utils/native-pass.txt`.

## Approach

Copy the lexer port's method: a hidden Fortran dump, a syntran driver that
prints the same format, and a bash script that diffs the two.

### Fortran side: hidden `syntran --parse <file>`

- Wire it like `--lex`: `args%parse` in `src/app.f90` (~285, plus the same
  validation at ~446 and exclusions at ~480/490), dispatched in `src/main.f90`
  (~53) to a new `syntran_parse_file()` in `src/syntran.f90`, next to
  `syntran_lex_file()` (~1046).  It runs `syntax_parse()` (`src/core.f90:~576`)
  in file mode (not the REPL), then the dump.
- New `parse_dump()`.  Put the walker beside `syntax_node_str()` in
  `src/types_ops.f90`, with a `module function` interface in `src/types.f90`.
  `syntax_node_str` is too lossy to reuse: it doesn't print names, args, or dot
  members.  The format is one node per line:
  `<2*depth spaces><role> <kind_name> [field=value ...]`.
  - `role` is the parent's member name (`left`, `right`, `members`, `args`,
    `condition`, ...)
  - Fields are printed only when set, in one fixed order: `id=<text>@<pos0>`,
    `op=<text>@<pos0>`, `type=<value_type_name(val)>` (`src/value.f90:360`),
    the slot (`g<id>`/`l<id>` from `id_index` + `is_loc`), the literal value
    (the source text for f32/f64, like `expr%identifier`), `sub_kind`, the omit
    flags, the array kind, `is_ref`, `root_kind`, `is_enum_name`, `num_locs`,
    `params`, and `src_pos`
  - A generic walker visits every allocated child member in a fixed order, so
    it can't go stale.  Text is escaped like `escape_dump()` (`src/lex.f90:1130`),
    and `pos` is 0-based like the lex dump
- After the tree, a trailer prints the symbol tables that later stages read:
  user fns (name, param types and ref flags, return type), structs (fields and
  types), and enums (variants), in id order.  Calls print their resolved
  (mangled) name, not the fn table index, so the port's intrinsic table doesn't
  have to match the Fortran table's order.
- **If there are any diagnostics, print only the `diag <code>` lines and no
  tree.**  That keeps error-recovery tree shapes out of the comparison.
  `diag_code()` (`src/lex.f90:1170`) already extracts the codes.
- Unit test `unit_test_parse_dump` in `src/tests/test.f90`, modeled on
  `unit_test_lex_dump` (~7871): a few small inline programs with exact expected
  dumps.

### Syntran side: `compiler/`

- `compiler/ast.syntran`: `enum NodeKind` (Fortran names, minus the
  `_expr`/`_statement` suffixes that lex-dump also drops), the `Type` struct
  (kind, element kind, rank, struct/enum name, fn signature index), and the
  fat `Node` struct mirroring `syntax_node_t`.  Children are `i32` indices
  into a `NodeVec` arena, and list members (`members`, `args`, `elems`,
  subscripts) are `[i32; :]`.  That's the decision already recorded in
  self-host.md: structs have value semantics, and whole-struct moves aren't done.
  Also the symbol tables: scoped var tables (vars/locs, slot counters, const
  flags), fn, struct, and enum tables, and a str→i32 hash dict modeled on
  `src/tests/long/aoc/dicts/dict_i64.syntran`.
- `compiler/intr.syntran`: the intrinsic signatures (from
  `src/intr_fns_*.f90`, ~3k Fortran lines of `fn_t` boilerplate, so write it as
  a compact table), `resolve_overload()` (`src/intr_fns.f90:142`), and the std
  constants from `declare_intr_vars`, which pre-seed the global slots.
- `compiler/parse.syntran`: the parser, `use lex::*;`.  Its procedure names
  match the Fortran ones, so the two read side by side.  The parser is mutually
  recursive, so it's one module.  If `#include` works inside a `use`d module,
  split it into `parse_expr`/`parse_fn`/`parse_control`/... files that mirror
  `src/parse_*.f90`.  Otherwise it stays one file.  The two-pass driver is
  faithful: pass 0 parses the whole unit, then the arena and slot counters are
  reset, the tables are kept, and pass 1 reparses, with the same hide-later-globals
  rule as `search()` (`src/parse_misc.f90:62-76`).
- `compiler/parse-dump.syntran`: the driver.  It prints the same format, with
  the escape and kind-name helpers copied from `compiler/lex-dump.syntran`.
- Header comment: list the differences from the Fortran, like lex.syntran does
  (0-based pos, half-open ranges, diags are codes only, arena indices instead of
  allocatable members).

### Test script: `utils/test-parse.sh`

Copy `utils/test-lex.sh` (`--native`, `-v`, bin and file arguments), plus a pass
list `utils/parse-pass.txt` with `--update-pass-list`, as in
`utils/test-transpile.sh` (~232/292).  Any file in the list that fails is a
failure.  Default inputs are `src/tests/test-src`, `samples`, and `src/tests/long`.
Add CI steps in `.github/workflows/main.yml` next to the lex ones (~362 on the
VM, ~795 native).

## Milestones (each one is a commit, and the pass list grows)

0. **Infrastructure.** (Done.)  The `--parse` dump with its unit test,
   `test-parse.sh` with a pass list, the CI steps, and skeletons for `ast`/
   `parse`/`parse-dump` that handle an empty file.  No file in the corpora is
   empty, so the pass list starts with the two that only have lexer
   diagnostics.  As built, the fn, struct, and enum ids are not dumped on the
   tree, since the trailer lists those tables in id order and a call is named
   by its `id=`.  A token with no `pos` has no `@pos`, and the empty subscript
   lists are left out.
1. **Core.** The arena, `Type`, scoped vars with `push_var`/`search`
   (`src/parse_misc.f90:14-76`), std constants, the two-pass driver, `match`
   with E20 de-duping, and `preprocess` minus includes.  Literals, unary and
   binary exprs with result types (`get_binary_op_kind` in `src/types_ops.f90`),
   let and const, assignment and compound assignment (`parse_expr_statement`'s
   speculative LHS and rewind), blocks, if/while/for (ranges), break/continue,
   and `src_pos` stamping.
2. **Fns.** The `parse_type` grammar, fn declarations (params, `&`/`&const`,
   `num_locs`, return checks needed for the tree), user calls with
   forward-reference tolerance in pass 0, fn refs, fn-typed vars and ptr calls,
   intrinsic calls with overload mangling, `std::` qualification,
   `use std::*`/`use std::name`, and the printf/writef desugaring.
3. **Arrays.** Every `array_expr` form (unif/expl/bound/step/len/size), typed
   arrays (`at_typed_array`, `src/parse_array.f90:937`), and subscripts
   (scalar/range/step/all, omits), with rank and type propagation.
4. **Structs and methods.** Declarations, instances, `parse_dot` chains and
   splices (`src/parse_expr.f90:1463-1931`), methods and implicit self,
   method calls (`build_method_call_node`), and file members.
5. **Enums and switch.** Enum declarations, access, casts, the bare-enum-name
   array, switch/case/case_range/guards.
6. **`#include` and `use`.** Token splicing in `preprocess()`, and module
   parsing with shared counters, `prefix::` renaming, and same-id copying
   (`src/parse_control.f90:131-658`).

Done means every file in the three corpora either passes, or has diagnostics
that are semantic (stage 2).  The pass list then holds every file whose Fortran
dump has no semantic `diag` codes.

Along the way, as with the lexer: record language gaps in
`scratch/self-host.md`.  Fix a Fortran parser bug in both parsers only if it
blocks the diff, and list it in self-host.md otherwise (keep the scope tight).
Update the self-host memory after each milestone.

## Verification

- `fpm test test --profile debug` (includes `unit_test_parse_dump`), and the
  suite with `--features debug,noblas` and `--features debug,llvm-blas`, since
  only Fortran files change on that side
- `bash utils/test-parse.sh -v build/v1/bin/syntran`: every file in
  `utils/parse-pass.txt` must pass, and the list grows each milestone
- `bash utils/test-parse.sh --native -v build/native/bin/syntran`: the port
  must also compile and run natively (it's the fast path, and the bootstrap
  target)
- `bash utils/test-lex.sh build/v1/bin/syntran` still passes, since the parser
  uses the lexer port
