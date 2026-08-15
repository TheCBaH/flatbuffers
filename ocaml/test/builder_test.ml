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
    B.set_scalar P.TShort b i a;
    B.set_scalar P.TByte b (i + 2) b';
    B.set_padding b (i + 3) 1
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
    (fun () -> B.start_vector b ~n_elts:(-1) ~elt_size:1);
  Alcotest.check_raises
    "zero element size"
    (Invalid_argument "Builder.start_vector: element size must be positive")
    (fun () -> B.start_vector b ~n_elts:1 ~elt_size:0);
  Alcotest.check_raises
    "payload overflow"
    (Invalid_argument "Builder.start_vector: size overflow")
    (fun () -> B.start_vector b ~n_elts:max_int ~elt_size:2);
  let expected = [| 'x' |] in
  let vector = Rt.UByte.Vector.create b expected in
  let buf = finish_root b vector in
  let (Rt.Root (view, vector)) = Rt.get_root P.Bytes buf in
  Alcotest.(check (array char))
    "builder remains usable"
    expected
    (Rt.UByte.Vector.to_array view vector)
;;

let test_cases =
  [ Alcotest.test_case "Exact-capacity scalar vectors" `Quick check_exact_capacity_scalar
  ; Alcotest.test_case "Exact-capacity strings" `Quick check_exact_capacity_string
  ; Alcotest.test_case "Exact-capacity reference vectors" `Quick check_exact_capacity_refs
  ; Alcotest.test_case "Exact-capacity struct vectors" `Quick check_exact_capacity_structs
  ; Alcotest.test_case "Exact-capacity vector64" `Quick check_exact_capacity_vector64
  ; Alcotest.test_case "Exact-capacity nested vectors" `Quick check_exact_capacity_nested
  ; Alcotest.test_case "Invalid vector sizes" `Quick check_invalid_vector_sizes
  ]
;;
