(* Structural verification tests.

   The central invariant exercised here is that verification of an arbitrary
   byte sequence terminates and returns either [Ok ()] or a structured
   [Error], and never escapes as a backend-specific exception. *)

module P = Flatbuffers.Primitives
module V = Flatbuffers.Verifier
open Fixtures.Monster_test
open MyGame.Example

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let kind_of = function
  | Ok () -> None
  | Error (e : V.error) -> Some e.kind
;;

let path_of = function
  | Ok () -> []
  | Error (e : V.error) -> e.path
;;

(* Verification must never raise, whatever the input. *)
let verify_exn_safe ?options ?size_prefixed ?off what buf =
  try Monster.verify ?options ?size_prefixed ?off P.Bytes buf with
  | exn -> Alcotest.failf "%s: verification raised %s" what (Printexc.to_string exn)
;;

let check_ok what r =
  match r with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: expected Ok, got %s" what (V.error_to_string e)
;;

let check_error what r =
  match r with
  | Ok () -> Alcotest.failf "%s: expected Error, got Ok" what
  | Error _ -> ()
;;

let check_kind what expected r =
  match kind_of r with
  | Some k when k = expected -> ()
  | Some k ->
    Alcotest.failf
      "%s: expected %s, got %s"
      what
      (Format.asprintf "%a" V.pp_error_kind expected)
      (Format.asprintf "%a" V.pp_error_kind k)
  | None -> Alcotest.failf "%s: expected an error, got Ok" what
;;

(* --- buffers ------------------------------------------------------- *)

let example_monster ?(size_prefixed = false) () =
  let b = Rt.Builder.create () in
  Fixtures.create_example_monster b |> Monster.finish_buf P.Bytes ~size_prefixed b
;;

let minimal_monster () =
  let b = Rt.Builder.create () in
  let name = Rt.String.create b "m" in
  Monster.Builder.(start b |> add_name name |> add_hp 7 |> finish)
  |> Monster.finish_buf P.Bytes b
;;

(* A chain of [n] monsters linked through the `enemy` field. *)
let monster_chain n =
  let b = Rt.Builder.create () in
  let rec build i =
    let name = Rt.String.create b (string_of_int i) in
    let inner = if i >= n then None else Some (build (i + 1)) in
    let t = Monster.Builder.(start b |> add_name name) in
    let t =
      match inner with
      | None -> t
      | Some e -> Monster.Builder.add_enemy e t
    in
    Monster.Builder.finish t
  in
  build 1 |> Monster.finish_buf P.Bytes b
;;

let with_nested_monster () =
  let b = Rt.Builder.create () in
  let inner = minimal_monster () in
  let name = Rt.String.create b "outer" in
  let nested = Rt.create_nested_vector b inner in
  Monster.Builder.(start b |> add_name name |> add_testnestedflatbuffer nested |> finish)
  |> Monster.finish_buf P.Bytes b
;;

(* --- byte surgery -------------------------------------------------- *)

let copy = Bytes.copy

(* Offsets and lengths read back out of a well-formed buffer are always small
   and positive, so a plain conversion is enough and stays valid where [int] is
   31 bits wide. Values that do not fit in an [int] are written as [int32]
   through [set_i32]. *)
let u32 buf i = Int32.to_int (Bytes.get_int32_le buf i)
let set_u32 buf i x = Bytes.set_int32_le buf i (Int32.of_int x)
let set_i32 buf i x = Bytes.set_int32_le buf i x

(* Position of the root table in a non size-prefixed buffer. *)
let root_pos buf = u32 buf 0

(* Position of the root table's vtable. *)
let vtable_pos buf =
  let r = root_pos buf in
  r - Int32.to_int (Bytes.get_int32_le buf r)
;;

(* Absolute position of field [voff] of the root table, or [-1] if absent. *)
let root_field buf voff =
  let vt = vtable_pos buf in
  let vtsize = Bytes.get_uint16_le buf vt in
  if voff >= vtsize
  then -1
  else (
    let foff = Bytes.get_uint16_le buf (vt + voff) in
    if foff = 0 then -1 else root_pos buf + foff)
;;

(* ------------------------------------------------------------------ *)
(* Positive cases, across every buffer backend                          *)
(* ------------------------------------------------------------------ *)

let check_valid_all_backends () =
  let buf = example_monster () in
  check_ok "bytes" (Monster.verify P.Bytes buf);
  check_ok "string" (Monster.verify P.String (Bytes.to_string buf));
  check_ok
    "bigstring"
    (Monster.verify
       P.Bigstring
       (Bigstringaf.of_string ~off:0 ~len:(Bytes.length buf) (Bytes.to_string buf)))
;;

let check_valid_size_prefixed () =
  let buf = example_monster ~size_prefixed:true () in
  check_ok "size prefixed" (Monster.verify ~size_prefixed:true P.Bytes buf);
  (* Trailing data after the prefixed message is allowed. *)
  let padded = Bytes.cat buf (Bytes.make 16 '\xEE') in
  check_ok "trailing data" (Monster.verify ~size_prefixed:true P.Bytes padded)
;;

let check_valid_nonzero_off () =
  let buf = example_monster () in
  let padded = Bytes.cat (Bytes.make 8 '\xAA') buf in
  check_ok "off=8" (Monster.verify ~off:8 P.Bytes padded);
  check_error "off=8 read as off=0" (Monster.verify P.Bytes padded)
;;

let check_valid_wire_fixtures path () =
  let buf = Fixtures.bytes_of_file path in
  check_ok path (Monster.verify P.Bytes buf);
  check_ok (path ^ " (string)") (Monster.verify P.String (Bytes.to_string buf))
;;

let check_root_verified () =
  let buf = example_monster () in
  match Monster.root_verified P.Bytes buf with
  | Error e -> Alcotest.failf "root_verified: %s" (V.error_to_string e)
  | Ok (Rt.Root (b, m)) ->
    Alcotest.(check int) "hp" 80 (Monster.hp b m);
    Alcotest.(check string) "name" "MyMonster" (Monster.name b m |> Rt.String.to_string b)
;;

let check_root_verified_rejects () =
  let buf = example_monster () in
  Bytes.set buf 4 'X';
  match Monster.root_verified P.Bytes buf with
  | Ok _ -> Alcotest.fail "root_verified accepted a bad identifier"
  | Error _ -> ()
;;

let check_offset64_valid () =
  let open Fixtures.Test64_bit in
  let b = Rt.Builder.create () in
  let far_vec = Rt.UByte.Vector.create b [| '\x01'; '\x02'; '\x03' |] in
  let far_str = Rt.String.create b "hello64" in
  let big_vec = Rt.UByte.Vector64.create b [| '\xAA'; '\xBB' |] in
  let near_str = Rt.String.create b "near" in
  let far_structs = LeafStruct.Vector.create b [| 42l, 3.14; 99l, 2.71 |] in
  let big_structs = LeafStruct.Vector64.create b [| 10l, 1.0; 20l, 2.0 |] in
  let buf =
    RootTable.Builder.(
      start b
      |> add_far_vector far_vec
      |> add_a 123l
      |> add_far_string far_str
      |> add_big_vector big_vec
      |> add_near_string near_str
      |> add_far_struct_vector far_structs
      |> add_big_struct_vector big_structs
      |> finish)
    |> RootTable.finish_buf P.Bytes b
  in
  check_ok "64-bit buffer" (RootTable.verify P.Bytes buf);
  (* Truncating or mutating a 64-bit buffer must never raise, and anything the
     verifier accepts must be fully readable. *)
  let oracle what t =
    match
      try RootTable.verify P.Bytes t with
      | exn -> Alcotest.failf "%s: verification raised %s" what (Printexc.to_string exn)
    with
    | Error _ -> ()
    | Ok () ->
      let (Rt.Root (b, r)) = RootTable.root P.Bytes t in
      (try ignore (RootTable.unpack b r) with
       | exn ->
         Alcotest.failf "%s: accepted but unreadable (%s)" what (Printexc.to_string exn))
  in
  for i = 0 to Bytes.length buf - 1 do
    oracle (Printf.sprintf "truncation %d" i) (Bytes.sub buf 0 i)
  done;
  for i = 0 to Bytes.length buf - 1 do
    let t = Bytes.copy buf in
    Bytes.set t i (Char.chr (Char.code (Bytes.get t i) lxor 0xFF));
    oracle (Printf.sprintf "flip %d" i) t
  done
;;

(* ------------------------------------------------------------------ *)
(* Roots, identifiers and size prefixes                                 *)
(* ------------------------------------------------------------------ *)

let check_short_buffers () =
  check_error "empty" (Monster.verify P.Bytes Bytes.empty);
  check_error "3 bytes" (Monster.verify P.Bytes (Bytes.make 3 '\x00'));
  check_error "4 zero bytes" (Monster.verify P.Bytes (Bytes.make 4 '\x00'));
  check_error "8 zero bytes" (Monster.verify P.Bytes (Bytes.make 8 '\x00'))
;;

let check_bad_identifier () =
  let buf = copy (example_monster ()) in
  Bytes.blit_string "XXXX" 0 buf 4 4;
  check_kind
    "identifier"
    (V.Invalid_identifier { expected = "MONS"; actual = Some "XXXX" })
    (Monster.verify P.Bytes buf)
;;

let check_bad_root_offset () =
  let buf = copy (example_monster ()) in
  set_u32 buf 0 0;
  check_kind "zero root offset" V.Invalid_offset (Monster.verify P.Bytes buf);
  let buf = copy (example_monster ()) in
  set_u32 buf 0 0xFFFFFFF;
  check_error "huge root offset" (Monster.verify P.Bytes buf);
  let buf = copy (example_monster ()) in
  set_i32 buf 0 0xFFFFFFFFl;
  check_error "negative root offset" (Monster.verify P.Bytes buf);
  let buf = copy (example_monster ()) in
  set_u32 buf 0 (root_pos buf + 1);
  check_kind
    "misaligned root"
    (V.Invalid_alignment { alignment = 4 })
    (Monster.verify P.Bytes buf);
  (* With alignment checking off the same buffer fails for another reason. *)
  let r =
    Monster.verify ~options:{ V.default_options with check_alignment = false } P.Bytes buf
  in
  check_error "misaligned root, unchecked alignment" r;
  Alcotest.(check bool)
    "not an alignment failure"
    true
    (match kind_of r with
     | Some (V.Invalid_alignment _) -> false
     | _ -> true)
;;

let check_size_prefix () =
  let buf = example_monster ~size_prefixed:true () in
  let big = copy buf in
  set_u32 big 0 (Bytes.length buf);
  check_kind
    "prefix too large"
    V.Invalid_size_prefix
    (Monster.verify ~size_prefixed:true P.Bytes big);
  let small = copy buf in
  set_u32 small 0 8;
  check_error "prefix too small" (Monster.verify ~size_prefixed:true P.Bytes small);
  check_error "size-prefixed read as plain" (Monster.verify P.Bytes buf);
  check_error
    "plain read as size-prefixed"
    (Monster.verify ~size_prefixed:true P.Bytes (example_monster ()))
;;

let check_bad_off () =
  let buf = example_monster () in
  check_error "negative off" (Monster.verify ~off:(-1) P.Bytes buf);
  check_error "off past end" (Monster.verify ~off:(Bytes.length buf + 1) P.Bytes buf);
  check_error "off at end" (Monster.verify ~off:(Bytes.length buf) P.Bytes buf)
;;

(* ------------------------------------------------------------------ *)
(* Tables and vtables                                                   *)
(* ------------------------------------------------------------------ *)

let check_bad_vtable () =
  let base = example_monster () in
  let buf = copy base in
  (* vtable displacement pointing forward past the end *)
  Bytes.set_int32_le buf (root_pos buf) (Int32.of_int (-1_000_000));
  check_kind "vtable out of range" V.Invalid_vtable (Monster.verify P.Bytes buf);
  let buf = copy base in
  (* vtable displacement pointing before the buffer *)
  Bytes.set_int32_le buf (root_pos buf) (Int32.of_int 1_000_000);
  check_kind "vtable before start" V.Invalid_vtable (Monster.verify P.Bytes buf);
  let buf = copy base in
  (* odd vtable size *)
  let vt = vtable_pos buf in
  Bytes.set_uint16_le buf vt (Bytes.get_uint16_le buf vt + 1);
  check_kind "odd vtable" V.Invalid_vtable (Monster.verify P.Bytes buf);
  let buf = copy base in
  (* vtable claiming to be larger than the buffer *)
  Bytes.set_uint16_le buf (vtable_pos buf) 0xFFFE;
  check_error "oversized vtable" (Monster.verify P.Bytes buf);
  let buf = copy base in
  (* empty vtable: every field reads as absent, so the required `name` fails *)
  Bytes.set_uint16_le buf (vtable_pos buf) 0;
  check_kind "empty vtable" V.Missing_required_field (Monster.verify P.Bytes buf)
;;

let check_missing_required () =
  let b = Rt.Builder.create () in
  let buf =
    Monster.Builder.(start b |> add_hp 3 |> finish) |> Monster.finish_buf P.Bytes b
  in
  let r = Monster.verify P.Bytes buf in
  check_kind "missing name" V.Missing_required_field r;
  Alcotest.(check bool) "path names the field" true (path_of r = [ V.Field "name" ])
;;

let check_field_out_of_range () =
  let buf = copy (example_monster ()) in
  (* Point the `hp` field entry far past the end of the buffer. *)
  let vt = vtable_pos buf in
  Bytes.set_uint16_le buf (vt + 8) 0xFFFE;
  check_error "field past end" (Monster.verify P.Bytes buf)
;;

(* ------------------------------------------------------------------ *)
(* Strings and vectors                                                  *)
(* ------------------------------------------------------------------ *)

let check_string_terminator () =
  let buf = copy (example_monster ()) in
  (* `name` is at vtable offset 10 *)
  let f = root_field buf 10 in
  let str = f + u32 buf f in
  let len = u32 buf str in
  Bytes.set buf (str + 4 + len) '\x41';
  check_kind "missing terminator" V.Missing_string_terminator (Monster.verify P.Bytes buf);
  check_ok
    "terminator check disabled"
    (Monster.verify
       ~options:{ V.default_options with check_string_terminator = false }
       P.Bytes
       buf)
;;

let check_string_length () =
  let base = example_monster () in
  let buf = copy base in
  let f = root_field buf 10 in
  let str = f + u32 buf f in
  set_u32 buf str 0xFFFFFF;
  check_error "huge string length" (Monster.verify P.Bytes buf);
  let buf = copy base in
  let f = root_field buf 10 in
  set_u32 buf f 0;
  check_kind "zero string offset" V.Invalid_offset (Monster.verify P.Bytes buf)
;;

let check_vector_length () =
  let base = example_monster () in
  let buf = copy base in
  (* `vector_of_doubles` is at vtable offset 70; 8 bytes per element *)
  let f = root_field buf 70 in
  let vec = f + u32 buf f in
  set_u32 buf vec 0xFFFFFFF;
  let r = Monster.verify P.Bytes buf in
  check_error "huge vector length" r;
  Alcotest.(check bool)
    "path names the vector"
    true
    (path_of r = [ V.Field "vector_of_doubles" ]);
  let buf = copy base in
  let f = root_field buf 70 in
  let vec = f + u32 buf f in
  set_i32 buf vec 0x7FFFFFFFl;
  check_error "overflowing vector length" (Monster.verify P.Bytes buf)
;;

(* A 32-bit length or offset with the top bit set is out of range for any
   FlatBuffer, and must be rejected everywhere. This gets its own case because
   where OCaml's int is 31 bits wide, Int32.to_int turns 0x80000000 into 0 — a
   perfectly plausible length — so a verifier that converts before checking
   accepts a buffer that the reader then refuses to read. *)
let check_high_bit_u32 () =
  let base = example_monster () in
  (* `vector_of_doubles` is at vtable offset 70 *)
  let f = root_field base 70 in
  let vec = f + u32 base f in
  List.iter
    (fun w ->
       let t = copy base in
       set_i32 t vec w;
       check_error (Printf.sprintf "length %lx" w) (Monster.verify P.Bytes t))
    [ 0x80000000l; 0xC0000000l; 0xFFFFFFFFl ];
  List.iter
    (fun w ->
       let t = copy base in
       set_i32 t f w;
       check_error (Printf.sprintf "offset %lx" w) (Monster.verify P.Bytes t))
    [ 0x80000000l; 0xC0000000l; 0xFFFFFFFFl ]
;;

let check_vector_element_error_path () =
  let buf = copy (example_monster ()) in
  (* `testarrayofstring` is at vtable offset 24 *)
  let f = root_field buf 24 in
  let vec = f + u32 buf f in
  let n = u32 buf vec in
  Alcotest.(check int) "two strings" 2 n;
  (* break the offset of the last element *)
  set_u32 buf (vec + 4 + 4) 0;
  let r = Monster.verify P.Bytes buf in
  check_kind "bad element offset" V.Invalid_offset r;
  Alcotest.(check bool)
    "path has field and index"
    true
    (path_of r = [ V.Field "testarrayofstring"; V.Index 1 ])
;;

(* ------------------------------------------------------------------ *)
(* Unions                                                               *)
(* ------------------------------------------------------------------ *)

let check_union_ok () =
  (* `create_example_monster` sets the `test` union to a Monster. *)
  check_ok "union payload" (Monster.verify P.Bytes (example_monster ()))
;;

let check_union_unknown_tag () =
  let buf = copy (example_monster ()) in
  (* union discriminator `test_type` is at vtable offset 18 *)
  let f = root_field buf 18 in
  Bytes.set_uint8 buf f 99;
  check_ok "unknown tag accepted by default" (Monster.verify P.Bytes buf);
  check_kind
    "unknown tag rejected in strict mode"
    (V.Unknown_union_tag 99L)
    (Monster.verify
       ~options:{ V.default_options with reject_unknown_union_tags = true }
       P.Bytes
       buf)
;;

let check_union_inconsistent () =
  let base = example_monster () in
  (* discriminator present, value slot removed *)
  let buf = copy base in
  let vt = vtable_pos buf in
  Bytes.set_uint16_le buf (vt + 20) 0;
  check_kind "value missing" V.Inconsistent_union (Monster.verify P.Bytes buf);
  (* value present, discriminator removed *)
  let buf = copy base in
  let vt = vtable_pos buf in
  Bytes.set_uint16_le buf (vt + 18) 0;
  check_kind "discriminator missing" V.Inconsistent_union (Monster.verify P.Bytes buf);
  (* discriminator NONE with a value present *)
  let buf = copy base in
  let f = root_field buf 18 in
  Bytes.set_uint8 buf f 0;
  check_kind "NONE with payload" V.Inconsistent_union (Monster.verify P.Bytes buf)
;;

let check_union_bad_payload () =
  let buf = copy (example_monster ()) in
  let f = root_field buf 20 in
  set_u32 buf f 0;
  let r = Monster.verify P.Bytes buf in
  check_kind "zero payload offset" V.Invalid_offset r;
  Alcotest.(check bool)
    "path records the variant"
    true
    (path_of r = [ V.Field "test"; V.Union_variant "monster" ])
;;

(* A union whose payload is a string rather than a table. *)
let check_string_union () =
  let open Fixtures.String_union in
  let envelope f =
    let b = Rt.Builder.create () in
    f b |> Envelope.finish_buf P.Bytes b
  in
  let with_name =
    envelope (fun b ->
      let s = Rt.String.create b "hi" in
      Envelope.Builder.(start b |> add_content_name s |> finish))
  in
  let with_table =
    envelope (fun b ->
      let msg = Rt.String.create b "hello" in
      let g = Greeting.Builder.(start b |> add_message msg |> finish) in
      Envelope.Builder.(start b |> add_content_greeting g |> finish))
  in
  let empty = envelope (fun b -> Envelope.Builder.(start b |> finish)) in
  check_ok "string variant" (Envelope.verify P.Bytes with_name);
  check_ok "table variant" (Envelope.verify P.Bytes with_table);
  check_ok "NONE variant" (Envelope.verify P.Bytes empty);
  List.iter
    (fun buf ->
       for i = 0 to Bytes.length buf - 1 do
         let t = copy buf in
         Bytes.set t i (Char.chr (Char.code (Bytes.get t i) lxor 0xFF));
         match
           try Envelope.verify P.Bytes t with
           | exn -> Alcotest.failf "mutation %d raised %s" i (Printexc.to_string exn)
         with
         | Ok () | Error _ -> ()
       done)
    [ with_name; with_table; empty ]
;;

(* ------------------------------------------------------------------ *)
(* Union vectors                                                        *)
(* ------------------------------------------------------------------ *)

(* The generator emits union-vector verification, but the reader generator does
   not yet produce usable accessors for `[SomeUnion]` fields, so there is no
   schema whose generated code exercises it. Drive the runtime directly instead,
   with the same shape the generator would emit. *)

module Bld = Flatbuffers.Runtime.Builder

let rec uv_table v pos =
  V.enter_table v pos
  && V.exit_table
       v
       (V.field_union_vector
          v
          ~name:"characters"
          ~type_voff:4
          ~voff:6
          ~required:false
          ~tag_size:1
          uv_dispatch)

and uv_dispatch v tag slot =
  match tag with
  | 0L -> V.union_none v slot
  | 1L -> V.union_string v slot ~variant:"other"
  | _ -> V.union_unknown v tag slot
;;

(* [tags] and [values] are written as two parallel vectors, exactly as a
   `[SomeUnion]` field is encoded. Either may be omitted. *)
let union_vector_buf ?tags ?values () =
  let b = Bld.create () in
  let vals =
    Option.map
      (fun vs -> Bld.create_vector_ref b (Array.map (fun s -> Bld.create_string b s) vs))
      values
  in
  let tagv =
    Option.map (fun ts -> Bld.create_vector P.TUByte b (Array.map Char.chr ts)) tags
  in
  let t = Bld.start_table b ~n_fields:2 in
  let t = Option.fold ~none:t ~some:(fun o -> Bld.push_slot_ref 0 o t) tagv in
  let t = Option.fold ~none:t ~some:(fun o -> Bld.push_slot_ref 1 o t) vals in
  Bld.finish P.Bytes b (Bld.end_table t)
;;

let verify_uv ?options buf = V.verify_root ?options P.Bytes buf uv_table

let check_union_vector () =
  check_ok
    "two string variants"
    (verify_uv (union_vector_buf ~tags:[| 1; 1 |] ~values:[| "a"; "bb" |] ()));
  check_ok "empty vectors" (verify_uv (union_vector_buf ~tags:[||] ~values:[||] ()));
  check_ok "both absent" (verify_uv (union_vector_buf ()));
  check_ok
    "NONE element is not dereferenced"
    (verify_uv (union_vector_buf ~tags:[| 0; 1 |] ~values:[| "x"; "y" |] ()));
  (* Unknown tags follow the same policy as scalar unions. *)
  let unknown = union_vector_buf ~tags:[| 1; 9 |] ~values:[| "a"; "b" |] () in
  check_ok "unknown tag accepted" (verify_uv unknown);
  check_kind
    "unknown tag rejected in strict mode"
    (V.Unknown_union_tag 9L)
    (verify_uv
       ~options:{ V.default_options with reject_unknown_union_tags = true }
       unknown)
;;

let check_union_vector_inconsistent () =
  check_kind
    "mismatched lengths"
    V.Inconsistent_union
    (verify_uv (union_vector_buf ~tags:[| 1; 1; 1 |] ~values:[| "a"; "b" |] ()));
  check_kind
    "values without tags"
    V.Inconsistent_union
    (verify_uv (union_vector_buf ~values:[| "a" |] ()));
  check_kind
    "tags without values"
    V.Inconsistent_union
    (verify_uv (union_vector_buf ~tags:[| 1 |] ()))
;;

let check_union_vector_bad_element () =
  let buf = union_vector_buf ~tags:[| 1; 1 |] ~values:[| "a"; "bb" |] () in
  (* The value vector is the field at vtable offset 6 of the root table. *)
  let f = root_field buf 6 in
  let vec = f + u32 buf f in
  Alcotest.(check int) "two elements" 2 (u32 buf vec);
  let t = copy buf in
  set_u32 t (vec + 4 + 4) 0;
  let r = verify_uv t in
  check_kind "zero element offset" V.Invalid_offset r;
  Alcotest.(check bool)
    "path has field, index and variant"
    true
    (path_of r = [ V.Field "characters"; V.Index 1; V.Union_variant "other" ]);
  (* And nothing about any mutation of this shape may raise. *)
  for i = 0 to Bytes.length buf - 1 do
    let t = copy buf in
    Bytes.set t i (Char.chr (Char.code (Bytes.get t i) lxor 0xFF));
    match
      try verify_uv t with
      | exn ->
        Alcotest.failf "union vector mutation %d raised %s" i (Printexc.to_string exn)
    with
    | Ok () | Error _ -> ()
  done
;;

(* ------------------------------------------------------------------ *)
(* Nested FlatBuffers                                                   *)
(* ------------------------------------------------------------------ *)

let check_nested_ok () =
  check_ok "nested buffer" (Monster.verify P.Bytes (with_nested_monster ()))
;;

let check_nested_corrupt () =
  let buf = copy (with_nested_monster ()) in
  (* `testnestedflatbuffer` is at vtable offset 30 *)
  let f = root_field buf 30 in
  let vec = f + u32 buf f in
  let data = vec + 4 in
  (* break the nested root offset *)
  set_u32 buf data 0;
  let r = Monster.verify P.Bytes buf in
  check_kind "nested root offset" V.Invalid_offset r;
  Alcotest.(check bool)
    "path enters the nested buffer"
    true
    (path_of r = [ V.Field "testnestedflatbuffer"; V.Nested_buffer ]);
  check_ok
    "nested checking disabled"
    (Monster.verify
       ~options:{ V.default_options with check_nested_flatbuffers = false }
       P.Bytes
       buf)
;;

let check_nested_truncated () =
  let buf = copy (with_nested_monster ()) in
  let f = root_field buf 30 in
  let vec = f + u32 buf f in
  (* shrink the nested payload without shrinking the buffer *)
  set_u32 buf vec 3;
  check_error "nested payload too small" (Monster.verify P.Bytes buf)
;;

(* ------------------------------------------------------------------ *)
(* Resource limits                                                      *)
(* ------------------------------------------------------------------ *)

let check_depth_limit () =
  let buf = monster_chain 5 in
  check_ok "default depth" (Monster.verify P.Bytes buf);
  let at n =
    Monster.verify ~options:{ V.default_options with max_depth = n } P.Bytes buf
  in
  check_ok "depth 5" (at 5);
  check_kind "depth 4" V.Depth_limit_exceeded (at 4);
  check_kind "depth 1" V.Depth_limit_exceeded (at 1)
;;

let check_table_limit () =
  let buf = monster_chain 5 in
  let at n =
    Monster.verify ~options:{ V.default_options with max_tables = n } P.Bytes buf
  in
  check_ok "tables 5" (at 5);
  check_kind "tables 4" V.Table_limit_exceeded (at 4);
  check_kind "tables 1" V.Table_limit_exceeded (at 1)
;;

let check_apparent_size_limit () =
  let buf = example_monster () in
  check_kind
    "apparent size"
    V.Apparent_size_limit_exceeded
    (Monster.verify
       ~options:{ V.default_options with max_apparent_size = 16 }
       P.Bytes
       buf);
  check_ok
    "apparent size, generous"
    (Monster.verify
       ~options:{ V.default_options with max_apparent_size = 1 lsl 20 }
       P.Bytes
       buf)
;;

(* ------------------------------------------------------------------ *)
(* Differential and mutation sweeps                                     *)
(* ------------------------------------------------------------------ *)

let backends buf =
  let s = Bytes.to_string buf in
  [ "bytes", Monster.verify P.Bytes buf
  ; "string", Monster.verify P.String s
  ; ( "bigstring"
    , Monster.verify P.Bigstring (Bigstringaf.of_string ~off:0 ~len:(String.length s) s) )
  ]
;;

(* Every backend must agree on acceptance and on the error kind. *)
let check_backends_agree buf what =
  match backends buf with
  | (_, r0) :: rest ->
    List.iter
      (fun (name, r) ->
         if kind_of r <> kind_of r0
         then
           Alcotest.failf
             "%s: %s disagrees (%s vs %s)"
             what
             name
             (match r with
              | Ok () -> "Ok"
              | Error e -> V.error_to_string e)
             (match r0 with
              | Ok () -> "Ok"
              | Error e -> V.error_to_string e))
      rest
  | [] -> ()
;;

(* The real invariant behind every corruption sweep: verification either
   rejects the buffer, or the unchecked zero-copy readers can walk all of it
   without raising. [unpack] visits every reachable field. *)
let oracle_accepted = ref 0

let oracle what t =
  match verify_exn_safe what t with
  | Error _ -> ()
  | Ok () ->
    incr oracle_accepted;
    let (Rt.Root (b, m)) = Monster.root P.Bytes t in
    (try ignore (Monster.unpack b m) with
     | exn ->
       Alcotest.failf "%s: accepted but unreadable (%s)" what (Printexc.to_string exn))
;;

let check_truncation_sweep () =
  let buf = example_monster () in
  for i = 0 to Bytes.length buf - 1 do
    let t = Bytes.sub buf 0 i in
    let what = Printf.sprintf "truncation %d" i in
    oracle what t;
    if i mod 7 = 0 then check_backends_agree t what
  done
;;

let check_mutation_sweep () =
  let base = example_monster () in
  let n = Bytes.length base in
  (* Flip every byte of the buffer in turn, and separately overwrite every
     aligned 32-bit word with values that stress offset arithmetic. *)
  for i = 0 to n - 1 do
    let t = copy base in
    Bytes.set t i (Char.chr (Char.code (Bytes.get t i) lxor 0xFF));
    oracle (Printf.sprintf "flip %d" i) t;
    if i mod 11 = 0 then check_backends_agree t (Printf.sprintf "flip %d" i)
  done;
  (* Corrupting a byte the encoder left as padding is accepted, which is what
     keeps the read-back half of [oracle] live. Guard against the sweep
     silently degrading into an exception check only. *)
  Alcotest.(check bool) "some mutations are accepted" true (!oracle_accepted > 0);
  let words =
    [ 0l; 1l; 4l; 0x7FFFFFFFl; 0x80000000l; 0xFFFFFFFFl; 0xFFFFl; Int32.of_int n ]
  in
  for i = 0 to (n / 4) - 1 do
    List.iter
      (fun w ->
         let t = copy base in
         set_i32 t (i * 4) w;
         oracle (Printf.sprintf "word %d := %lx" i w) t)
      words
  done
;;

let check_random_mutation_sweep () =
  let base = example_monster () in
  let n = Bytes.length base in
  let st = Random.State.make [| 0x5EED |] in
  for iter = 0 to 999 do
    let t = copy base in
    for _ = 0 to Random.State.int st 6 do
      let i = Random.State.int st n in
      Bytes.set t i (Char.chr (Random.State.int st 256))
    done;
    oracle (Printf.sprintf "random %d" iter) t
  done
;;

let check_arbitrary_bytes () =
  let st = Random.State.make [| 0xC0FFEE |] in
  for len = 0 to 64 do
    for _ = 0 to 9 do
      let t = Bytes.init len (fun _ -> Char.chr (Random.State.int st 256)) in
      ignore (verify_exn_safe "arbitrary" t : (unit, V.error) result);
      ignore
        ((try Monster.verify ~size_prefixed:true P.Bytes t with
          | exn ->
            Alcotest.failf "arbitrary size-prefixed raised %s" (Printexc.to_string exn))
         : (unit, V.error) result)
    done
  done
;;

(* ------------------------------------------------------------------ *)

let test_cases =
  Alcotest.
    [ test_case "Valid buffer on all backends" `Quick check_valid_all_backends
    ; test_case "Valid size-prefixed buffer" `Quick check_valid_size_prefixed
    ; test_case "Valid buffer at nonzero off" `Quick check_valid_nonzero_off
    ; test_case
        "Valid gold wire fixture"
        `Quick
        (check_valid_wire_fixtures "monsterdata_test.mon")
    ; test_case
        "Valid python wire fixture"
        `Quick
        (check_valid_wire_fixtures "monsterdata_python_wire.mon")
    ; test_case "root_verified returns a usable root" `Quick check_root_verified
    ; test_case "root_verified rejects bad input" `Quick check_root_verified_rejects
    ; test_case "64-bit offsets and vectors" `Quick check_offset64_valid
    ; test_case "Short and empty buffers" `Quick check_short_buffers
    ; test_case "Bad file identifier" `Quick check_bad_identifier
    ; test_case "Bad root offset" `Quick check_bad_root_offset
    ; test_case "Size prefix bounds the message" `Quick check_size_prefix
    ; test_case "Out-of-range off" `Quick check_bad_off
    ; test_case "Corrupt vtables" `Quick check_bad_vtable
    ; test_case "Missing required field" `Quick check_missing_required
    ; test_case "Field offset out of range" `Quick check_field_out_of_range
    ; test_case "String terminator" `Quick check_string_terminator
    ; test_case "String length and offset" `Quick check_string_length
    ; test_case "Vector length" `Quick check_vector_length
    ; test_case "Vector element error path" `Quick check_vector_element_error_path
    ; test_case "High-bit 32-bit words" `Quick check_high_bit_u32
    ; test_case "Union payload" `Quick check_union_ok
    ; test_case "Unknown union tag" `Quick check_union_unknown_tag
    ; test_case "Inconsistent union" `Quick check_union_inconsistent
    ; test_case "Bad union payload offset" `Quick check_union_bad_payload
    ; test_case "String union mutations" `Quick check_string_union
    ; test_case "Union vector" `Quick check_union_vector
    ; test_case "Union vector inconsistency" `Quick check_union_vector_inconsistent
    ; test_case "Union vector bad element" `Quick check_union_vector_bad_element
    ; test_case "Nested flatbuffer" `Quick check_nested_ok
    ; test_case "Nested flatbuffer corruption" `Quick check_nested_corrupt
    ; test_case "Nested flatbuffer truncation" `Quick check_nested_truncated
    ; test_case "Depth limit" `Quick check_depth_limit
    ; test_case "Table limit" `Quick check_table_limit
    ; test_case "Apparent size limit" `Quick check_apparent_size_limit
    ; test_case "Truncation sweep" `Quick check_truncation_sweep
    ; test_case "Mutation sweep" `Quick check_mutation_sweep
    ; test_case "Random mutation sweep" `Quick check_random_mutation_sweep
    ; test_case "Arbitrary bytes" `Quick check_arbitrary_bytes
    ]
;;
