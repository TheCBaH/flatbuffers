module Rt = Flatbuffers.Runtime
module B = Rt.Builder
module P = Flatbuffers.Primitives

let finish_root b offset = B.finish P.Bytes b offset

let check_exact_capacity_scalar () =
  let check capacity expected =
    let b = B.create ~init_capacity:capacity () in
    let vector = Rt.UByte.Vector.create b expected in
    let buf = finish_root b vector in
    let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
    Alcotest.(check (array char))
      (Printf.sprintf "capacity %d, length %d" capacity (Array.length expected))
      expected
      (Rt.UByte.Vector.to_array view vector)
  in
  let expected = Array.init 16 Char.chr in
  for remaining = 0 to 8 do
    check (Array.length expected + remaining) expected
  done;
  check 16 [||];
  check 16 [| 'x' |]
;;

let check_exact_capacity_string () =
  let expected = "abcdefghijkl" in
  let b = B.create ~init_capacity:16 () in
  let string = Rt.String.create b expected in
  let buf = finish_root b string in
  let (Rt.Root (view, string)) = Rt.get_root P.Bytes buf in
  Alcotest.(check string) "string" expected (Rt.String.to_string view string)
;;

let check_exact_capacity_refs () =
  let expected = [| "abcdefgh"; "abcdefgh"; "abcdefgh"; "abcdefgh" |] in
  let b = B.create ~init_capacity:32 () in
  let string = Rt.String.create b expected.(0) in
  let vector = Rt.String.Vector.create b [| string; string; string; string |] in
  let buf = finish_root b vector in
  let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
  let actual =
    Array.map
      (fun string -> Rt.String.to_string view string)
      (Rt.String.Vector.to_array view vector)
  in
  Alcotest.(check (array string)) "references" expected actual
;;

let check_exact_capacity_structs () =
  let expected = [| 1, 2; 3, 4; 5, 6; 7, 8 |] in
  let b = B.create ~init_capacity:16 () in
  let set b i (a, b') =
    B.Unsafe.set_scalar P.TShort b i a;
    B.Unsafe.set_scalar P.TByte b (i + 2) b';
    B.Unsafe.set_padding b (i + 3) 1
  in
  let vector = B.create_vector_struct set ~size:4 b expected in
  let buf = finish_root b vector in
  let vector = P.get_uoffset P.Bytes buf 0 in
  let length = P.get_uoffset P.Bytes buf vector in
  let actual =
    Array.init length (fun i ->
      let pos = vector + 4 + (i * 4) in
      P.get_scalar P.TShort P.Bytes buf pos, P.get_scalar P.TByte P.Bytes buf (pos + 2))
  in
  Alcotest.(check (array (pair int int))) "structs" expected actual
;;

let check_exact_capacity_vector64 () =
  let expected = [| 1L; 2L |] in
  let b = B.create ~init_capacity:16 () in
  let vector = Rt.Long.Vector64.create b expected in
  let buf = finish_root b vector in
  let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
  Alcotest.(check (array int64))
    "vector64"
    expected
    (Rt.Long.Vector64.to_array view vector)
;;

let check_exact_capacity_nested () =
  let expected = Bytes.init 16 Char.chr in
  let b = B.create ~init_capacity:16 () in
  let vector = Rt.create_nested_vector b expected in
  let buf = finish_root b vector in
  let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
  let actual = Bytes.of_seq (Array.to_seq (Rt.UByte.Vector.to_array view vector)) in
  Alcotest.(check bytes) "nested bytes" expected actual
;;

let check_invalid_vector_sizes () =
  let b = B.create ~init_capacity:16 () in
  Alcotest.check_raises
    "negative count"
    (Invalid_argument "Builder.start_vector: element count must be non-negative")
    (fun () -> B.Unsafe.start_vector b ~n_elts:(-1) ~elt_size:1);
  Alcotest.check_raises
    "zero element size"
    (Invalid_argument "Builder.start_vector: element size must be positive")
    (fun () -> B.Unsafe.start_vector b ~n_elts:1 ~elt_size:0);
  Alcotest.check_raises
    "payload overflow"
    (Invalid_argument "Builder.start_vector: size overflow")
    (fun () -> B.Unsafe.start_vector b ~n_elts:max_int ~elt_size:2);
  let expected = [| 'x' |] in
  let vector = Rt.UByte.Vector.create b expected in
  let buf = finish_root b vector in
  let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
  Alcotest.(check (array char))
    "builder remains usable"
    expected
    (Rt.UByte.Vector.to_array view vector)
;;

let check_reusable label b =
  let expected = [| 'x' |] in
  let vector = Rt.UByte.Vector.create b expected in
  let buf = finish_root b vector in
  let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
  Alcotest.(check (array char)) label expected (Rt.UByte.Vector.to_array view vector)
;;

let finish_table b = ignore (finish_root b (B.end_table b))
let finish_vector b = ignore (finish_root b (B.Unsafe.end_vector b))

let check_nested_starts () =
  let b = B.create ~init_capacity:16 () in
  ignore (B.start_table b ~n_fields:0);
  let before = B.Unsafe.current_offset b in
  Alcotest.check_raises
    "table in table"
    (Invalid_argument
       "Builder.start_table: expected an idle builder, but builder is building a table")
    (fun () -> ignore (B.start_table b ~n_fields:0));
  Alcotest.check_raises
    "vector in table"
    (Invalid_argument
       "Builder.start_vector: expected an idle builder, but builder is building a table")
    (fun () -> B.Unsafe.start_vector b ~n_elts:1 ~elt_size:1);
  Alcotest.check_raises
    "string in table"
    (Invalid_argument
       "Builder.create_string: expected an idle builder, but builder is building a table")
    (fun () -> ignore (Rt.String.create b "x"));
  Alcotest.(check bool) "table unchanged" true (before = B.Unsafe.current_offset b);
  finish_table b;
  check_reusable "after nested table errors" b;
  B.Unsafe.start_vector b ~n_elts:1 ~elt_size:1;
  let before = B.Unsafe.current_offset b in
  Alcotest.check_raises
    "table in vector"
    (Invalid_argument
       "Builder.start_table: expected an idle builder, but builder is building a 32-bit \
        vector")
    (fun () -> ignore (B.start_table b ~n_fields:0));
  Alcotest.check_raises
    "vector in vector"
    (Invalid_argument
       "Builder.start_vector: expected an idle builder, but builder is building a 32-bit \
        vector")
    (fun () -> B.Unsafe.start_vector b ~n_elts:1 ~elt_size:1);
  Alcotest.(check bool) "vector unchanged" true (before = B.Unsafe.current_offset b);
  finish_vector b;
  check_reusable "after nested vector errors" b
;;

let check_mismatched_ends () =
  let b = B.create ~init_capacity:16 () in
  ignore (B.start_table b ~n_fields:0);
  Alcotest.check_raises
    "vector end for table"
    (Invalid_argument
       "Builder.end_vector: expected an open 32-bit vector, but builder is building a \
        table")
    (fun () -> ignore (B.Unsafe.end_vector b));
  finish_table b;
  check_reusable "after table end mismatch" b;
  B.Unsafe.start_vector b ~n_elts:1 ~elt_size:1;
  Alcotest.check_raises
    "table end for vector"
    (Invalid_argument
       "Builder.end_table: expected an open table, but builder is building a 32-bit \
        vector")
    (fun () -> ignore (B.end_table b));
  finish_vector b;
  check_reusable "after vector end mismatch" b;
  Alcotest.check_raises
    "table end while idle"
    (Invalid_argument "Builder.end_table: expected an open table, but builder is idle")
    (fun () -> ignore (B.end_table b));
  Alcotest.check_raises
    "vector end while idle"
    (Invalid_argument
       "Builder.end_vector: expected an open 32-bit vector, but builder is idle")
    (fun () -> ignore (B.Unsafe.end_vector b));
  check_reusable "after idle end errors" b
;;

let check_finish_and_reset_while_nested () =
  let b = B.create ~init_capacity:16 () in
  ignore (B.start_table b ~n_fields:0);
  Alcotest.check_raises
    "finish while nested"
    (Invalid_argument
       "Builder.finish: expected an idle builder, but builder is building a table")
    (fun () -> ignore (finish_root b (B.Unsafe.current_offset b)));
  Alcotest.check_raises
    "reset while nested"
    (Invalid_argument
       "Builder.reset: expected an idle builder, but builder is building a table")
    (fun () -> B.reset b);
  finish_table b;
  check_reusable "after finish/reset errors" b
;;

let check_invalid_table_fields () =
  let b = B.create ~init_capacity:16 () in
  Alcotest.check_raises
    "negative field count"
    (Invalid_argument "Builder.start_table: field count must be non-negative")
    (fun () -> ignore (B.start_table b ~n_fields:(-1)));
  check_reusable "after negative field count" b;
  ignore (B.start_table b ~n_fields:2);
  let before = B.Unsafe.current_offset b in
  Alcotest.check_raises
    "negative field ID"
    (Invalid_argument "Builder.push_slot_scalar: field ID must be non-negative")
    (fun () -> ignore (B.push_slot_scalar P.TByte (-1) 1 b));
  Alcotest.check_raises
    "field ID beyond declared count"
    (Invalid_argument "Builder.push_slot_ref: field ID 2 is outside table field count 2")
    (fun () -> ignore (B.push_slot_ref 2 (B.Unsafe.current_offset b) b));
  Alcotest.check_raises
    "push beyond declared count"
    (Invalid_argument
       "Builder.push_slot_scalar: field ID 2 is outside table field count 2")
    (fun () -> ignore (B.push_slot_scalar P.TByte 2 1 b));
  Alcotest.check_raises
    "default push beyond declared count"
    (Invalid_argument
       "Builder.push_slot_scalar_default: field ID 2 is outside table field count 2")
    (fun () -> ignore (B.push_slot_scalar_default P.TByte 2 ~default:1 1 b));
  Alcotest.(check bool)
    "invalid fields do not move builder"
    true
    (before = B.Unsafe.current_offset b);
  ignore (B.push_slot_scalar P.TByte 1 7 b);
  finish_table b;
  check_reusable "after invalid field errors" b
;;

let check_vector_movement_rejected () =
  let b = B.create ~init_capacity:16 () in
  B.Unsafe.start_vector b ~n_elts:1 ~elt_size:1;
  let before = B.Unsafe.current_offset b in
  Alcotest.check_raises
    "prep while vector is open"
    (Invalid_argument
       "Builder.prep: expected an idle builder or an open table, but builder is building \
        a 32-bit vector")
    (fun () -> ignore (B.Unsafe.reserve ~align:1 ~bytes:1 b));
  Alcotest.(check bool) "rejected movement" true (before = B.Unsafe.current_offset b);
  finish_vector b;
  check_reusable "after vector movement error" b
;;

let test_cases =
  [ Alcotest.test_case "Exact-capacity scalar vectors" `Quick check_exact_capacity_scalar
  ; Alcotest.test_case "Exact-capacity strings" `Quick check_exact_capacity_string
  ; Alcotest.test_case "Exact-capacity reference vectors" `Quick check_exact_capacity_refs
  ; Alcotest.test_case "Exact-capacity struct vectors" `Quick check_exact_capacity_structs
  ; Alcotest.test_case "Exact-capacity vector64" `Quick check_exact_capacity_vector64
  ; Alcotest.test_case "Exact-capacity nested vectors" `Quick check_exact_capacity_nested
  ; Alcotest.test_case "Invalid vector sizes" `Quick check_invalid_vector_sizes
  ; Alcotest.test_case "Nested starts" `Quick check_nested_starts
  ; Alcotest.test_case "Mismatched ends" `Quick check_mismatched_ends
  ; Alcotest.test_case
      "Finish and reset while nested"
      `Quick
      check_finish_and_reset_while_nested
  ; Alcotest.test_case "Invalid table fields" `Quick check_invalid_table_fields
  ; Alcotest.test_case "Vector movement" `Quick check_vector_movement_rejected
  ]
;;
