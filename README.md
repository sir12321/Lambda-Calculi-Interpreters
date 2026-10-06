# Interpreters and Type Inference Engine

[![CI](https://github.com/sir12321/Interpreters-and-Type-Inference/actions/workflows/ci.yml/badge.svg)](https://github.com/sir12321/Interpreters-and-Type-Inference/actions/workflows/ci.yml)

**Author:** Manya Jain

OCaml implementations of programming-language tooling:

| Project | Description |
|---|---|
| [`abstract-machines/`](abstract-machines/) | Krivine (call-by-name) and SECD (call-by-value) abstract machines for an extended lambda calculus, with the SECD compiler emitting stack opcodes |
| [`lithp-type-inference/`](lithp-type-inference/) | Set-based type inferencer for the LITHP Lisp dialect, with ocamllex/ocamlyacc parsers and seeded-guess stabilization for recursive `label`/`defun` forms |

Each subproject has its own README with design notes.

## Requirements

OCaml (tested with 4.13 and 4.14) with `ocamlc`, `ocamllex`, and `ocamlyacc` — e.g. `sudo apt install ocaml-nox` or `opam switch create 4.14.1` — plus GNU `make`.

## Build and Test

```bash
make            # build both projects
make test       # run both test suites against their expected output
make clean      # remove build artifacts
```

Or build one project:

```bash
make -C abstract-machines && ./abstract-machines/lambda_interp
make -C lithp-type-inference && ./lithp-type-inference/lithp_parser lithp-type-inference/input.txt
```

## Tests

Each project has a golden-output test: `make test` runs the program on its sample input and diffs the result against `expected_output.txt`. CI runs `make test` on every push and pull request. After an intentional behavior change, regenerate the expected output and review the diff before committing.

## License

MIT — see [LICENSE](LICENSE).
