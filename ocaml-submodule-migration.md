# OCaml FlatBuffers: Submodule Migration Analysis

## Overview

This document analyzes the feasibility of restructuring the OCaml FlatBuffers
fork into a standalone repository that uses `google/flatbuffers` as a git
submodule, with a minimal patch applied on top.

## Current Fork Structure

The fork adds OCaml support to FlatBuffers. Changes fall into two categories:

### 1. OCaml-Specific Files (can live outside upstream)

These are pure additions with no upstream dependencies:

- `ocaml/` — runtime library, tests, benchmarks (~50 files)
- `dune-project`, `dune-workspace`, `flatbuffers.opam` — OCaml build config
- `.devcontainer/` — development environment
- `.github/workflows/ocaml-*` — CI/CD workflows
- `flatbuffers-ocaml-build.md` — build instructions

### 2. Code Generator (maintained separately, copied into source tree)

- `src/bfbs_gen_ocaml.cpp` (912 lines) — the OCaml code generator
- `src/bfbs_gen_ocaml.h` (33 lines) — header file

These follow the same pattern as Lua/Nim generators. They inherit from
`BaseBfbsGenerator` and use the BFBS (binary schema) reflection system.

FlatBuffers has no plugin mechanism — all generators are statically compiled
into `flatc`. These files must be copied into the submodule source tree before
building.

### 3. Integration Patch (9 lines across 5 files)

A minimal patch is required to register the OCaml generator with `flatc`:

```diff
--- a/.gitignore
+++ b/.gitignore
@@ -153,6 +153,7 @@
 cmake-build-debug/
 _deps/
+_build/
 **/.gradle/**

--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -184,6 +184,7 @@
   src/bfbs_gen_lua.h
   src/bfbs_gen_nim.h
+  src/bfbs_gen_ocaml.h
   src/bfbs_namer.h
@@ -196,6 +197,7 @@
   src/bfbs_gen_lua.cpp
   src/bfbs_gen_nim.cpp
+  src/bfbs_gen_ocaml.cpp
   src/code_generators.cpp

--- a/include/flatbuffers/idl.h
+++ b/include/flatbuffers/idl.h
@@ -769,6 +769,7 @@
     kKotlinKmp = 1 << 19,
+    kOCaml = 1 << 20,
     kMAX

--- a/src/flatc_main.cpp
+++ b/src/flatc_main.cpp
@@ -21,6 +21,7 @@
 #include "bfbs_gen_nim.h"
+#include "bfbs_gen_ocaml.h"
 #include "flatbuffers/base.h"
@@ -182,6 +183,11 @@
       flatbuffers::NewTsCodeGenerator());
+
+  flatc.RegisterCodeGenerator(
+      flatbuffers::FlatCOption{ "", "ocaml", "",
+                                "Generate OCaml code for tables/structs" },
+      flatbuffers::NewOCamlBfbsGenerator(flatbuffers_version));

--- a/src/idl_parser.cpp
+++ b/src/idl_parser.cpp
@@ -2799,7 +2799,7 @@
-      IDLOptions::kJson | IDLOptions::kNim;
+      IDLOptions::kJson | IDLOptions::kNim | IDLOptions::kOCaml;
```

## Proposed Repository Structure

```
ocaml-flatbuffers/
├── flatbuffers/                  # git submodule → google/flatbuffers
├── src/
│   ├── bfbs_gen_ocaml.cpp        # code generator (maintained here)
│   └── bfbs_gen_ocaml.h
├── patches/
│   └── ocaml-integration.patch   # 9-line patch shown above
├── ocaml/                        # runtime library
│   ├── lib/                      # runtime modules
│   └── test/                     # tests and benchmarks
├── .devcontainer/
├── .github/workflows/
├── dune-project
├── dune-workspace
├── flatbuffers.opam
└── Makefile                      # orchestrates: patch → copy → cmake → build
```

## Build Process

1. Initialize/update the submodule
2. Copy `src/bfbs_gen_ocaml.{cpp,h}` into `flatbuffers/src/`
3. Apply `patches/ocaml-integration.patch` to the submodule
4. Build `flatc` via cmake inside the submodule
5. Copy `flatc` binary to project root
6. Run `opam install . --deps-only -t` and `dune test`

## Maintenance Considerations

**Updating upstream:** Bump the submodule pointer and re-check that the patch
applies cleanly. The patch touches stable registration APIs that follow the
same pattern as Lua/Nim — these rarely change structurally. When they do (e.g.,
the `Namer::Config` or `GenerateFromSchema` signature changes that occurred in
this merge), `bfbs_gen_ocaml.cpp` will need updating but the integration patch
will likely still apply.

**Patch conflict risk:** Low. The 5 touchpoints are:
- `.gitignore` — adding one line
- `CMakeLists.txt` — adding source files to a list
- `idl.h` — appending to a language enum
- `flatc_main.cpp` — adding a generator registration block
- `idl_parser.cpp` — adding a flag to a bitmask

These are all append-style changes in well-structured locations.
