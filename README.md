# Interpreters and Type Inference Engine

**Author:** Manya Jain

OCaml implementations of programming-language tooling:

| Project | Description |
|---|---|
| [`abstract-machines/`](abstract-machines/) | Krivine (call-by-name) and SECD (call-by-value) abstract machines for an extended lambda calculus, with the SECD compiler emitting stack opcodes |
| [`lithp-type-inference/`](lithp-type-inference/) | Set-based type inferencer for the LITHP Lisp dialect, with ocamllex/ocamlyacc parsers and seeded-guess stabilization for recursive `label`/`defun` forms |

Each subproject has its own README with design notes.

## Build

```bash
make            # build both projects
make clean      # remove build artifacts
```

Or build one project:

```bash
make -C abstract-machines && ./abstract-machines/lambda_interp
make -C lithp-type-inference && ./lithp-type-inference/lithp_parser <file>
```

## License

MIT — see [LICENSE](LICENSE).
