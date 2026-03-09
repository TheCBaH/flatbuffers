# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OCaml support for [FlatBuffers](https://github.com/google/flatbuffers) serialization. The upstream repo is a git submodule at `flatbuffers/`; this repo contains the OCaml runtime library, a C++ code generator for `flatc`, and a minimal integration patch.

## Build Commands

All OCaml commands require `opam exec --` prefix unless `eval $(opam env)` has been run.

```bash
make flatc          # Patch submodule, build flatc with OCaml support
make deps           # Install OCaml dependencies via opam
make test           # Build flatc + run dune tests
make bench          # Run benchmarks
make clean          # Clean everything (dune + flatc + reset submodule)
make clean-flatc    # Clean only flatc build and reset submodule

# Run tests without flatc (uses pre-generated promoted files)
opam exec -- dune test --ignore-promoted-rules

# Generate OCaml bindings from a schema
./flatc --ocaml path/to/schema.fbs
```

## Architecture

### Submodule + Patch Model

- `flatbuffers/` — git submodule pointing to google/flatbuffers
- `src/bfbs_gen_ocaml.{cpp,h}` — OCaml code generator, maintained here, copied into submodule at build time
- `patches/ocaml-integration.patch` — 9-line patch that registers the OCaml generator with flatc (adds `kOCaml` language flag, CMake source entries, and generator registration in `flatc_main.cpp`)
- `Makefile` orchestrates: copy generator → apply patch → cmake → build flatc

### OCaml Runtime (`ocaml/lib/`)

The runtime uses a polymorphic buffer representation supporting Bytes, String, and Bigstring (via `bigstringaf`). Key modules:

- `primitives.ml` — Scalar type definitions and low-level buffer read/write functions, uses `cppo` preprocessing for OCaml version compatibility
- `runtime.ml` — Core virtual table (vt) type and field accessor patterns
- `builder.ml` — FlatBuffer construction API with offset tracking
- `read.ml` — Table/vector/struct reader functions, offset computation
- `flatbuffers.ml` — Top-level module re-exporting Runtime and Primitives

### Code Generator (`src/bfbs_gen_ocaml.cpp`)

Inherits from `BaseBfbsGenerator`, uses BFBS (binary schema) reflection. Generates `.ml` and `.mli` files. Follows the same pattern as Lua/Nim generators in upstream.

### Generated Code (`ocaml/test/generated/`)

Dune uses `(mode promote)` — generated outputs are checked into the repo. The generation rule in `ocaml/test/generated/dune` runs `flatc --ocaml` against schemas from `flatbuffers/tests/` and `flatbuffers/samples/`. Paths reference the submodule: `%{workspace_root}/flatbuffers/tests/`.

### Test Data

Tests in `ocaml/test/monster_test.ml` read binary test data from the submodule: `../../flatbuffers/tests/monsterdata_test.mon`. Both the dune `(deps (file ...))` declarations and the ML source paths must stay in sync.

## Key Conventions

- Runtime library uses `cppo` preprocessor for OCaml version detection (`ocaml/lib/dune`)
- `dune-workspace` adds `.` to PATH so dune can find the `flatc` binary at project root
- The integration patch touches stable registration APIs (enum flags, CMake source lists) that rarely change structurally across upstream updates
