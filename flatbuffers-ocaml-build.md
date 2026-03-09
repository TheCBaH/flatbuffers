# Flatbuffers OCaml Build Instructions

## Step 1: Build the `flatc` Compiler (C++)

```bash
cd /workspaces/flatbuffers
mkdir -p build && cd build
cmake .. -DFLATBUFFERS_BUILD_TESTS=OFF
make -j$(nproc) flatc
```

Copy the binary to the project root so dune can find it:

```bash
cp build/flatc flatc
```

## Step 2: Install OCaml Dependencies

All commands must be prefixed with `opam exec --` (or run `eval $(opam env)` first).

```bash
opam install . --deps-only -t -y
```

Key dependencies (from `flatbuffers.opam`):
- `bigstringaf` - runtime dependency
- `cppo` - build-time preprocessor
- `alcotest` - test framework
- `benchmark` - benchmarking library
- `memtrace` - memory profiling (test only)

## Step 3: Build and Test

With `flatc` available in the project root:

```bash
opam exec -- dune test
```

Without `flatc` (uses pre-generated/promoted files):

```bash
opam exec -- dune test --ignore-promoted-rules
```

## Step 4: Run Benchmarks (Optional)

```bash
opam exec -- dune exec --profile=release --display=quiet ocaml/test/bench/fb_bench.exe
```

## Using `flatc` for OCaml Code Generation

Generate OCaml bindings from a `.fbs` schema file:

```bash
./flatc --ocaml ./samples/monster.fbs
```

This outputs `.ml` and `.mli` files in the working directory.

## Project Structure

- `ocaml/lib/` - Runtime library source
- `ocaml/test/` - Tests and benchmarks
- `ocaml/test/generated/` - Pre-generated (promoted) bindings from test schemas
- `src/bfbs_gen_ocaml.cpp` - The OCaml code generator in `flatc`
- `flatbuffers.opam` - Package metadata and dependencies
- `dune-project` / `dune-workspace` - Build system configuration

## Important Notes

- Always prefix OCaml/dune commands with `opam exec --` unless you've run `eval $(opam env)`
- The `dune` build uses `(mode promote)` for generated files, meaning generated outputs are checked into the repo
- Using `--ignore-promoted-rules` skips re-generation and uses the checked-in files
