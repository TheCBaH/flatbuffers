# Flatbuffers OCaml Build Instructions

## Step 1: Clone with Submodule

```bash
git clone --recurse-submodules <repo-url>
```

Or if already cloned:

```bash
git submodule update --init
```

## Step 2: Install OCaml Dependencies

All commands must be prefixed with `opam exec --` (or run `eval $(opam env)` first).

```bash
make deps
```

Key dependencies (from `flatbuffers.opam`):
- `bigstringaf` - runtime dependency
- `cppo` - build-time preprocessor
- `alcotest` - test framework
- `benchmark` - benchmarking library
- `memtrace` - memory profiling (test only)

## Step 3: Build and Test

Build `flatc` (applies patch + copies generator into submodule) and run tests:

```bash
make
```

Or step by step:

```bash
make flatc    # patch submodule, build flatc, copy binary to repo root
make test     # run dune test
```

Without `flatc` (uses pre-generated/promoted files):

```bash
opam exec -- dune test --ignore-promoted-rules
```

## Step 4: Run Benchmarks (Optional)

```bash
make bench
```

## Using `flatc` for OCaml Code Generation

Generate OCaml bindings from a `.fbs` schema file:

```bash
./flatc --ocaml ./flatbuffers/samples/monster.fbs
```

This outputs `.ml` and `.mli` files in the working directory.

## Project Structure

- `flatbuffers/` - git submodule (google/flatbuffers upstream)
- `src/bfbs_gen_ocaml.{cpp,h}` - OCaml code generator (copied into submodule at build time)
- `patches/ocaml-integration.patch` - minimal patch to register OCaml generator with flatc
- `ocaml/lib/` - Runtime library source
- `ocaml/test/` - Tests and benchmarks
- `ocaml/test/generated/` - Pre-generated (promoted) bindings from test schemas
- `flatbuffers.opam` - Package metadata and dependencies
- `dune-project` / `dune-workspace` - Build system configuration
- `Makefile` - Build orchestration

## Important Notes

- Always prefix OCaml/dune commands with `opam exec --` unless you've run `eval $(opam env)`
- The `dune` build uses `(mode promote)` for generated files, meaning generated outputs are checked into the repo
- Using `--ignore-promoted-rules` skips re-generation and uses the checked-in files
- The `make clean` target resets the submodule to its pristine state
