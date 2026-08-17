# OCaml FlatBuffers

[![OCaml Build](https://github.com/TheCBaH/flatbuffers/actions/workflows/ocaml-build.yml/badge.svg)](https://github.com/TheCBaH/flatbuffers/actions/workflows/ocaml-build.yml)
[Open in GitHub Codespaces](https://github.com/codespaces/new?hide_repo_select=true&ref=master&repo=167849851)

OCaml support for [FlatBuffers](https://flatbuffers.dev/), including:

- an OCaml runtime for reading and building FlatBuffers;
- an OCaml backend for the `flatc` schema compiler;
- JavaScript targets through `js_of_ocaml` and Melange.

The upstream FlatBuffers source is included as the `flatbuffers/` submodule. The
OCaml generator in `src/` and the integration patch in `patches/` are applied
when `flatc` is built.

## Requirements

- OCaml 4.13 or newer
- opam and Dune
- CMake and a C++ compiler
- Git submodules initialized

Initialize the upstream dependency and install development dependencies:

```bash
git submodule update --init --recursive
make deps
```

## Build and test

Build the patched compiler and run the test suite:

```bash
make test
```

Other useful commands:

```bash
make flatc       # Build flatc with OCaml support
make bench       # Run benchmarks
make generate    # Regenerate promoted test bindings
make clean       # Remove build artifacts
```

To run the OCaml tests using the checked-in generated files:

```bash
opam exec -- dune test --ignore-promoted-rules
```

## Generate OCaml bindings

After building the compiler, pass an `.fbs` schema to `flatc.ocaml`:

```bash
./flatc.ocaml --ocaml path/to/schema.fbs
```

This generates OCaml implementation (`.ml`) and interface (`.mli`) files for
the schema. The generated bindings use the runtime library provided by this
repository.

## Repository layout

| Path | Purpose |
| --- | --- |
| `ocaml/lib/` | OCaml runtime library |
| `src/` | OCaml `flatc` generator backend |
| `ocaml/test/` | Runtime and generator tests |
| `flatbuffers/` | Upstream FlatBuffers submodule |
| `patches/` | Upstream integration patch |

## License

Apache License 2.0. See the upstream FlatBuffers project for its licensing
information and attribution.
