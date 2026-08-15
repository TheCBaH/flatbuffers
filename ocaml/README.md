# Flatbuffers

Runtime library for flatbuffers in ocaml.

## Building
Either compile the flatbuffers project first, so the `flatc` binary is available in
the parent directory, or run dune with `--ignore-promoted-rules`.

    opam install . --deps-only -t
    dune test

Includes an ocaml version of the cpp encode/decode benchmark (see `fb_bench.cpp`).

    dune exec --profile=release --display=quiet test/bench/fb_bench.exe

The struct-vector microbenchmark covers indexed traversal, iteration, and
array/list conversion for both `vector` and `vector64` on Bytes and
Bigstringaf buffers:

    dune exec --profile=release --display=quiet test/bench/struct_vector_bench.exe

Read performance appears to be close to Java (openjdk 11) using the baseline
compiler and close to 2x faster with flambda.

## Usage
Run flatc with the `--ocaml` flag.

    ./flatc --ocaml ./samples/monster.fbs

This will output `monster.ml` and `monster.mli` in the working directory. No
additional flags are currently supported.

The generated code covers tables, structs, enums, unions, fixed-size arrays,
`offset64`/`vector64` fields, and `nested_flatbuffer` fields (which get an
`<field>_as_<type>` accessor returning a root of the nested type). Alongside
the zero-copy accessors it emits a builder, an allocating object API
(`unpack`/`pack`), `lookup_by_key` for tables and structs carrying a `key`
field, and the verification entry points described below.

### Custom builders

`Flatbuffers.Builder` exposes complete constructors for tables, vectors, and
strings. Build referenced objects before the table or vector that contains
them, and use every returned offset only with the same builder before its next
`finish` or `reset`.

Custom inline-struct writers can use the explicitly low-level
`Builder.Unsafe` setters. The callback receives the start of a region already
reserved by `create_vector_struct` or `push_slot_struct`; it must stay within
that region and must not otherwise advance the builder:

```ocaml
let set_pair b i (x, y) =
  Flatbuffers.Builder.Unsafe.set_scalar Flatbuffers.Primitives.TInt b i x;
  Flatbuffers.Builder.Unsafe.set_scalar Flatbuffers.Primitives.TInt b (i + 4) y

let pairs =
  Flatbuffers.Builder.create_vector_struct set_pair ~size:8 builder values
```

`Builder.Unsafe.reserve` and its manual vector operations are available for
specialized generators. They expose the backwards-growing buffer model and
can corrupt output if indices escape the reserved region, so application code
should prefer the complete constructors.

Builders retain their largest backing buffer across `finish` and `reset`,
which avoids allocation for steady-size reuse. After an exceptional one-off
large message, call `Builder.trim builder` to return to the capacity requested
at creation, or `Builder.trim ~capacity:n builder` to retain a different upper
bound. `Builder.capacity` reports the current retained size. Trimming also
starts a new build cycle and clears shared-string and vtable caches; like
`reset`, it requires an idle builder.

### Union vectors

For a field such as `items:[Item]`, the generated table module exposes
`items_length`, an indexed `items` callback dispatcher, and `items_iter`,
`items_to_list`, and `items_to_array`. The callbacks have the same shape as a
scalar union accessor, including `none` and `default` handling.

The generated builder keeps the parallel discriminator and value vectors
paired behind an opaque prepared value:

```ocaml
let items =
  Inventory.Builder.create_items builder
    [| `None_; `Sword sword; `Label label |]
in
Inventory.Builder.(start builder |> add_items items |> finish)
```

The object API represents the field as an array of the union's polymorphic
variants and packs or unpacks both vectors together. FlatBuffers rejects the
`vector64` attribute on union vectors, so this paired API intentionally uses
the format's 32-bit union-value offsets and vector lengths.

Struct union variants are standalone referenced payloads, unlike ordinary
struct fields that are written inline in a table. Generated struct modules
therefore expose `create`; call it before starting the containing table or
union vector, then pass the returned offset to the generated union builder.
The object API handles that distinction automatically.

Union callback labels follow the schema's union member names. An unaliased
qualified member therefore keeps its namespace in the label (for example,
`MyGame.Example2.Monster` becomes `~my_game_example2_monster`), which avoids a
collision with another `Monster` member. Give members explicit FlatBuffers
aliases when a shorter public OCaml label is preferred.

### Key-sorted vectors

For a table or struct with a `key` field, the generated module exposes
`create_sorted_vector` alongside the order-preserving `Vector.create`:

```ocaml
let stats = Stat.create_sorted_vector builder unsorted_stat_offsets
let abilities = Ability.create_sorted_vector builder unsorted_ability_values
```

The input array is copied, so sorting does not mutate caller-owned data.
Table keys are read from the already-built objects without exposing the
builder's backing buffer; struct values are sorted before serialization.
Signed, unsigned, and string keys use their FlatBuffers ordering. Generated
object `pack` calls the sorted constructor automatically for keyed vectors.

### FlexBuffers

`Flatbuffers.Flexbuffers` provides a zero-copy reader for roots, scalars,
strings, keys, blobs, maps, and untyped, typed, and fixed vectors. It works on
the same `Bytes`, `String`, `Bigstringaf`, and JavaScript `DataView` backends as
the FlatBuffers runtime:

```ocaml
match Flatbuffers.Flexbuffers.root_verified Flatbuffers.Primitives.Bytes encoded with
| Error e -> prerr_endline (Flatbuffers.Flexbuffers.error_to_string e)
| Ok root ->
  match Flatbuffers.Flexbuffers.as_map root with
  | None -> ()
  | Some map -> ignore (Flatbuffers.Flexbuffers.Map.find map "name")
```

The exact scalar accessors return options instead of applying the coercions of
some other FlexBuffers runtimes. `as_uint64_bits` returns the format's complete
unsigned 64-bit bit pattern in an `int64`, including values above
`Int64.max_int`.

A schema field annotated `flexbuffer` additionally gains a
`<field>_flexbuffer_root` accessor. Generated verification checks the complete
dynamic value inside the exact bounds of the containing byte vector. The
standalone verifier checks widths and types, backward offsets, alignment,
terminators and UTF-8, map key/value agreement and ordering, plus configurable
depth, value-count, and apparent-work limits. A FlexBuffers builder is not
currently part of the OCaml API; encoded values can be created by any
conforming implementation and stored with the ordinary byte-vector builder.

## Verification

### Trust model

The generated accessors are zero-copy and perform **no bounds checking**: they
read offsets straight out of the buffer and follow them. That is what makes
them fast, and it is safe only for buffers you produced yourself or received
over a trusted channel. Handing a hostile or corrupted buffer to `root` and the
field accessors can raise arbitrary backend exceptions, read unrelated parts of
the buffer, or loop.

For untrusted input, verify first. The root table module of every schema gains
two entry points:

```ocaml
val verify
  :  ?options:Flatbuffers.Verifier.options
  -> ?size_prefixed:bool
  -> ?off:int
  -> 'b Flatbuffers.Primitives.t
  -> 'b
  -> (unit, Flatbuffers.Verifier.error) result

val root_verified
  :  ?options:Flatbuffers.Verifier.options
  -> ?size_prefixed:bool
  -> ?off:int
  -> 'b Flatbuffers.Primitives.t
  -> 'b
  -> (t Rt.root, Flatbuffers.Verifier.error) result
```

`root_verified` verifies once and then returns exactly the same root and
accessor types as `root`, so nothing downstream changes and no per-access
checks are added:

```ocaml
match Monster.root_verified Flatbuffers.Primitives.Bytes buf with
| Error e -> prerr_endline (Flatbuffers.Verifier.error_to_string e)
| Ok (Rt.Root (b, m)) -> print_int (Monster.hp b m)
```

Use `verify` instead when validation and access happen in different layers —
for example when a network layer admits a buffer and a later stage builds the
root:

```ocaml
let admit buf =
  match Monster.verify Flatbuffers.Primitives.Bytes buf with
  | Ok () -> Ok buf
  | Error e -> Error (Flatbuffers.Verifier.error_to_string e)
```

`root` is unchanged: it is still the unchecked fast path, and code that uses it
compiles and performs exactly as before.

Verification works with every buffer backend — `bytes`, `string`,
`Bigstringaf.t`, and the JavaScript `DataView` used by the js_of_ocaml and
Melange runtimes — and all of them accept and reject the same buffers with the
same error.

### What is checked

A single traversal proves that every offset, length and vtable in the buffer
is in range before anything dereferences it:

* the root offset, the optional size prefix, and the file identifier;
* table positions, signed vtable displacements, vtable sizes, and field slots;
* inline scalars, enums, structs and fixed arrays, including alignment;
* `required` fields are present;
* strings, including the length prefix and the trailing NUL byte;
* vectors of scalars, structs, strings and tables, with overflow-checked
  length arithmetic and per-element offset validation;
* unions and union vectors, as a discriminator/value pair;
* nested FlatBuffers, verified inside a region narrowed to the containing byte
  vector, without copying it;
* annotated FlexBuffers, semantically verified inside their containing byte
  vector;
* `offset64` fields and `vector64` vectors, with 64-bit values rejected before
  they are converted to an OCaml `int`.

Invalid input always comes back as `Error`. It never escapes as a `Bytes`,
`String`, `Bigstringaf`, `DataView`, or integer-conversion exception.

An error carries a kind, the absolute byte offset in the buffer you supplied,
and the schema path that led there:

```
missing string terminator at offset 148 (.testarrayofstring[1])
```

### Options

```ocaml
type options =
  { max_depth : int                    (* default 64 *)
  ; max_tables : int                    (* default 1_000_000 *)
  ; max_apparent_size : int             (* default 2^34, or max_int on 32-bit *)
  ; check_alignment : bool              (* default true *)
  ; check_string_terminator : bool      (* default true *)
  ; check_nested_flatbuffers : bool     (* default true *)
  ; check_flexbuffers : bool            (* default true *)
  ; reject_unknown_union_tags : bool    (* default false *)
  }
```

`max_depth` and `max_tables` match the upstream C++ defaults and bound
recursion and total work. `max_apparent_size` bounds the total number of bytes
the traversal claims to visit; because a DAG can reference the same sub-object
many times, `max_tables` alone does not bound that. All three are configurable,
and each produces its own error kind so a rejection can be attributed.

The four `check_*` flags exist because those checks are format policy rather
than memory safety; turning them off keeps the traversal safe but accepts more
buffers.

### Deliberate differences from the upstream C++ verifier

The upstream verifier (`include/flatbuffers/verifier.h`) is the behavioral
reference. This implementation differs in a few places on purpose:

* **Size prefix.** The prefix must fit inside the input, and it *bounds* the
  message: nothing outside `[off + 4, off + 4 + prefix)` is reachable. Trailing
  bytes after that are allowed, which supports framed and concatenated streams.
  Upstream only checks that the prefix fits.
* **Union pairing.** A discriminator naming a variant with no value slot, or a
  value slot with no discriminator, or a `NONE` discriminator with a value
  slot, is rejected as `Inconsistent_union`. Upstream accepts all three. The
  first of them is not merely untidy here: the generated OCaml union reader
  raises when a caller handles the named variant and the value slot is absent,
  so accepting it would break the guarantee that a verified buffer is safe to
  read.
* **Unknown union tags** are accepted by default, matching upstream, so that
  buffers written against a newer schema still verify. The discriminator and
  the value slot are still checked structurally, but an unknown payload cannot
  be traversed without a schema. Set `reject_unknown_union_tags` to reject
  them.
* **UTF-8 is not checked.** The OCaml runtime exposes FlatBuffer strings as
  arbitrary OCaml byte strings, so their encoding is not part of the contract.
* **No differential test harness** against the C++ verifier is included. It
  would need a C++ program built against generated headers alongside the
  submodule, which is more build machinery than it is worth here. The
  cross-checking is instead between the OCaml backends, plus the invariant that
  anything verification accepts can be fully walked by the unchecked readers —
  the corruption sweeps in `test/verifier_test.ml` assert exactly that.

### Cost

Verification is a single traversal that touches every reachable byte range
once. It allocates a fixed ~1 KB of working state per call, independent of
buffer size, plus a little more for deeply nested data. On the benchmark shapes
in `test/bench/verify_bench.ml` it runs at roughly a quarter to a fifth of the
speed of an unchecked traversal of the same data:

    dune exec --profile=release --display=quiet test/bench/verify_bench.exe

The unchecked reader is unaffected: no accessor gained a check, and
`test/bench/fb_bench.ml` and `test/bench/monster_bench.ml` measure the same
code paths as before.

## Platform support

32-bit targets are supported and covered by CI: `linux/i386` and `linux/arm/v7`
run `make flatc`, `make test`, `make test-jsoo` and `make generate-check` on
OCaml 4.14. They also run the native benchmark suite; checksum expectations and
allocation totals stay in `int64` where a 31-bit OCaml `int` cannot hold the
value. Melange remains skipped because it does not support 32-bit architectures.

Big-endian hosts are supported and covered by `linux/s390x` CI. FlatBuffers
remain little-endian on the wire: native s390x builds select unconditional byte
swaps at preprocessing time, while the JavaScript backends use explicit
little-endian access independent of the compiler host.

Code that has to care about the width of an `int` is selected at preprocessing
time using `TARGET_INT_SIZE`: 31 for native 32-bit targets, 63 for native
64-bit targets, and 32 for the JavaScript backends. Such code must not write
integer literals above `max_int` for the narrowest supported target
(1073741823) — those are a compile error on 31-bit, not a truncation. The
verifier follows both rules: a 32- or 64-bit value from the buffer that cannot
be represented as a non-negative `int` on the current platform is rejected
rather than truncated, so a 32-bit build simply refuses buffers with offsets
or lengths it could not address anyway.
