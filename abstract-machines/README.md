# Lambda Calculi Interpreters

**Author:** Manya Jain

Two abstract machine interpreters for the lambda calculus, implemented in OCaml:

- **Krivine machine** — call-by-name (lazy) evaluation
- **SECD machine** — call-by-value (eager) evaluation

Each machine is implemented twice, once with function-based environments and once with list-based environments, giving four total implementations that are cross-checked against the same test suite.

The core pure lambda calculus (`V`, `Abs`, `App`) is extended with integers, booleans, arithmetic, pairs, and conditionals.

## Files

| File | Role |
|---|---|
| `declarations.ml` | Shared AST, helper constructors, pretty-printer, Church-encoding helpers |
| `krivine.ml` | Krivine machine — `FunTable` and `ListTable` variants |
| `secd.ml` | SECD compiler and machine — `FunTable` and `ListTable` variants |
| `input.txt` | Test expressions (compiled as the `Input` module) |
| `main.ml` | Test driver |
| `Makefile` | Build commands |

## Language

### Core (pure lambda calculus)
```
V of string          (* variable *)
Abs of string * exp  (* abstraction: λx.e *)
App of exp * exp     (* application: (e1 e2) *)
```

### Extensions
```
Const of int | bool          (* integer and boolean literals *)
Let of string * exp * exp    (* let x = e1 in e2 *)
IfTE of exp * exp * exp      (* if-then-else *)
Pair of exp * exp            (* primitive pairs *)
Fst of exp | Snd of exp      (* projections *)
Add | Sub | Mul | Div        (* arithmetic *)
Eq | Not | And | Or          (* boolean ops *)
```

Church booleans (`tru = λt.λf.t`, `fls = λt.λf.f`) and encoded conditionals are also included in `declarations.ml`.

## Krivine Machine (call-by-name)

Closures have the form `Clos of exp * table`. The three core transition rules are:

| Rule | Transition |
|---|---|
| `(Op)` | Decompose application, push argument closure onto stack |
| `(Var)` | Look up variable in environment |
| `(App)` | Apply abstraction by extending the environment |

Evaluation is **weak** — it stops at a lambda abstraction, constant, or pair when the stack is empty, without reducing inside the body. For example, `(λx.(λy.x)) (1 + 2)` evaluates to `λy.(1 + 2)` under Krivine, while SECD returns `λy.3`.

Additional stack frames handle `Let`, `IfTE`, primitive ops, and pair projections.

## SECD Machine (call-by-value)

The compiler translates expressions to an opcode list; the machine executes the opcodes against a (Stack, Environment, Code, Dump) tuple.

Core opcodes:
```
LOOKUP   (* push value of variable from environment *)
MKCLOS   (* build a closure value *)
APP      (* apply a closure *)
RET      (* return from a function call *)
```

Extension opcodes: `CONST`, `IF`, `MKPAIR`, `FST`, `SND`, `ADD`, `SUB`, `MUL`, `DIV`, `EQ`, `NOT`, `AND`, `OR`.

## Tests

Test expressions live in `input.txt` and are compiled as the `Input` module. The driver in `main.ml` runs each expression through all four implementations and prints the results side by side.

Core tests cover:
- identity and application
- call-by-name vs. call-by-value divergence
- closure unpacking back to source syntax

Extension tests cover:
- Church-encoding examples
- arithmetic and boolean primitives
- pairs and projections
- `Let`, `IfTE`, mixed lambda+primitive expressions
- error/invalid cases

## Build and Run

```bash
make                # compile
./lambda_interp     # run
make clean          # clean build artifacts
```
