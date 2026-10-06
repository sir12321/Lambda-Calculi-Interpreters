# LITHP Type Inference

**Author:** Manya Jain

A modular type inference engine for LITHP, a Lisp-like language. Parses a LITHP program and infers one or more possible types for each top-level expression, including higher-order forms such as `lambda`, `label`, and `defun`.

## Overview

The inference is set-based and conservative — every expression gets a `typ list`. Multiple possible typings are preserved rather than forcing a single answer early, and recursive functions use a seeded-guess / stabilization loop to handle back-tracking.

## Files

| File | Role |
|---|---|
| `main.ml` | Driver — parses input, calls inferencer, pretty-prints type sets |
| `ast.ml` | Core data model: `atom`, `exp`, `program`, `typ`, `type_env` |
| `type_support.ml` | Helpers: name extraction, primitive signatures, quoting, set operations on type lists |
| `type_rules.ml` | Typing rules for built-in primitives (`+`, `car`, `cond`, `lambda`, `label`, `defun`, …) |
| `type.ml` | Main inference engine: atomic inference, application, environment, recursion, back-tracking |
| `grammar.mly` | Parser specification (ocamlyacc) |
| `lexer.mll` | Tokenizer (ocamllex) |
| `symbol.ml`, `variable.ml`, `names.mli`, `bigint.ml` | Support modules |
| `Makefile` | Build and run targets |
| `input.txt` | Sample input |

## Module Design

### `ast.ml`
Holds the shared data model for both the parser and inferencer:
- `atom`, `exp`, `program` for syntax
- `typ` for inferred types: `Tint`, `Tbool`, `Tvar`, `Tlist of int`, `TlistAny`, `Tfunc of int * typ`, `TfuncMin of int * typ`
- `type_env` for identifier and function bindings

### `type_support.ml`
Reusable support logic kept separate to avoid cluttering the inferencer:
- Symbol/identifier name extraction from expressions
- Primitive name recognition and `c[ad]+r` expansion (`cadr` → `(car (cdr …))`)
- Set-like operations and compatibility checks on type lists
- Environment lookup and parameter binding

### `type_rules.ml`
Typing rules for built-in constructs, isolated here because primitive typing is rule-heavy and orthogonal to the generic recursion in `type.ml`:
- Arithmetic: `+`, `*`, `-`, `div`, `mod`
- Comparisons: `>`, `<`, `<=`, `>=`, `eq`
- List ops: `quote`, `atom`, `car`, `cdr`, `cons`
- Control: `cond`, `lambda`, `label`, `defun`

### `type.ml`
The inference control module:
- Infers atomic expressions and list-form function applications
- Traverses a whole program, extending the environment after each `defun`
- Handles recursive inference for `label` and named `defun` via seeded return-type guesses that are re-evaluated until the result stabilizes

## Inference Strategy

- `[]` means no valid typing (type error)
- `NIL` is treated as both `Bool` and `List(0)`
- Quoted lists get `List(n)` for their literal length
- Function values are `Tfunc(arity, return_type)` or `TfuncMin(min_arity, return_type)`
- Unbound variable identifiers default to `Tvar`

## Lexer and Parser Notes

The lexer emits whitespace as `WS` tokens (rather than discarding it) so the grammar can handle separators explicitly. Quote shorthand `'expr` is desugared to `(quote expr)` in the parser. Top-level input is parsed as `program = Program of exp list`, allowing the inferencer to process expressions one by one and accumulate the environment across `defun` definitions.

## Build and Run

```bash
make
./lithp_parser input.txt
# or: make run
```

Sample output for the included `input.txt`:
```
List(2) -> Any
```

```bash
make clean
```
