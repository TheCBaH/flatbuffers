let check_offset64_roundtrip () =
  let open Fixtures.Test64_bit in
  let b = Rt.Builder.create () in
  (* build some vectors *)
  let far_vec = Rt.UByte.Vector.create b [| '\x01'; '\x02'; '\x03' |] in
  let far_str = Rt.String.create b "hello64" in
  let big_vec = Rt.UByte.Vector64.create b [| '\xAA'; '\xBB' |] in
  let near_str = Rt.String.create b "near" in
  let far_structs = LeafStruct.Vector.create b [| 42l, 3.14; 99l, 2.71 |] in
  let big_structs = LeafStruct.Vector64.create b [| 10l, 1.0; 20l, 2.0; 30l, 3.0 |] in
  let wip =
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
  in
  let buf = RootTable.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, tbl)) = RootTable.root Flatbuffers.Primitives.Bytes buf in
  (* check scalar *)
  Alcotest.(check int32) "a" 123l (RootTable.a buf tbl);
  (* check offset64 vector (4-byte length, 8-byte offset) *)
  let fv = Rt.Option.get (RootTable.far_vector buf tbl) in
  Alcotest.(check int) "far_vector length" 3 (Rt.UByte.Vector.length buf fv);
  Alcotest.(check char) "far_vector[0]" '\x01' (Rt.UByte.Vector.get buf fv 0);
  Alcotest.(check char) "far_vector[2]" '\x03' (Rt.UByte.Vector.get buf fv 2);
  (* check offset64 string *)
  let fs = Rt.Option.get (RootTable.far_string buf tbl) in
  Alcotest.(check string) "far_string" "hello64" (Rt.String.to_string buf fs);
  (* check vector64 (8-byte length, 8-byte offset) *)
  let bv = Rt.Option.get (RootTable.big_vector buf tbl) in
  Alcotest.(check int) "big_vector length" 2 (Rt.UByte.Vector64.length buf bv);
  Alcotest.(check char) "big_vector[0]" '\xAA' (Rt.UByte.Vector64.get buf bv 0);
  Alcotest.(check char) "big_vector[1]" '\xBB' (Rt.UByte.Vector64.get buf bv 1);
  (* check near_string (normal 32-bit) *)
  let ns = Rt.Option.get (RootTable.near_string buf tbl) in
  Alcotest.(check string) "near_string" "near" (Rt.String.to_string buf ns);
  (* check offset64 struct vector (4-byte length, 8-byte offset) *)
  let fsv = Rt.Option.get (RootTable.far_struct_vector buf tbl) in
  Alcotest.(check int) "far_struct_vector length" 2 (LeafStruct.Vector.length buf fsv);
  let s0 = LeafStruct.Vector.get buf fsv 0 in
  Alcotest.(check int32) "far_struct[0].a" 42l (LeafStruct.a buf s0);
  Alcotest.(check (float 0.01)) "far_struct[0].b" 3.14 (LeafStruct.b buf s0);
  let s1 = LeafStruct.Vector.get buf fsv 1 in
  Alcotest.(check int32) "far_struct[1].a" 99l (LeafStruct.a buf s1);
  let far_values = [| 42l; 99l |] in
  let far_a value = LeafStruct.a buf value in
  Alcotest.(check (array int32))
    "far_struct_vector to_array"
    far_values
    (LeafStruct.Vector.to_array buf fsv |> Array.map far_a);
  Alcotest.(check (array int32))
    "far_struct_vector to_list"
    far_values
    (LeafStruct.Vector.to_list buf fsv |> List.map far_a |> Array.of_list);
  Alcotest.(check (array int32))
    "far_struct_vector to_seq"
    far_values
    (LeafStruct.Vector.to_seq buf fsv |> Seq.map far_a |> Array.of_seq);
  let iterated = ref [] in
  LeafStruct.Vector.iter buf (fun value -> iterated := far_a value :: !iterated) fsv;
  Alcotest.(check (array int32))
    "far_struct_vector iter"
    far_values
    (List.rev !iterated |> Array.of_list);
  (* check vector64 struct vector (8-byte length, 8-byte offset) *)
  let bsv = Rt.Option.get (RootTable.big_struct_vector buf tbl) in
  Alcotest.(check int) "big_struct_vector length" 3 (LeafStruct.Vector64.length buf bsv);
  let bs0 = LeafStruct.Vector64.get buf bsv 0 in
  Alcotest.(check int32) "big_struct[0].a" 10l (LeafStruct.a buf bs0);
  let bs2 = LeafStruct.Vector64.get buf bsv 2 in
  Alcotest.(check int32) "big_struct[2].a" 30l (LeafStruct.a buf bs2);
  Alcotest.(check (float 0.01)) "big_struct[2].b" 3.0 (LeafStruct.b buf bs2);
  let big_values = [| 10l; 20l; 30l |] in
  let big_a value = LeafStruct.a buf value in
  Alcotest.(check (array int32))
    "big_struct_vector to_array"
    big_values
    (LeafStruct.Vector64.to_array buf bsv |> Array.map big_a);
  Alcotest.(check (array int32))
    "big_struct_vector to_list"
    big_values
    (LeafStruct.Vector64.to_list buf bsv |> List.map big_a |> Array.of_list);
  Alcotest.(check (array int32))
    "big_struct_vector to_seq"
    big_values
    (LeafStruct.Vector64.to_seq buf bsv |> Seq.map big_a |> Array.of_seq);
  let iterated = ref [] in
  LeafStruct.Vector64.iter buf (fun value -> iterated := big_a value :: !iterated) bsv;
  Alcotest.(check (array int32))
    "big_struct_vector iter"
    big_values
    (List.rev !iterated |> Array.of_list)
;;

let check_absent_fields () =
  let open Fixtures.Test64_bit in
  let b = Rt.Builder.create () in
  let wip = RootTable.Builder.(start b |> finish) in
  let buf = RootTable.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, tbl)) = RootTable.root Flatbuffers.Primitives.Bytes buf in
  Alcotest.(check bool)
    "far_vector absent"
    true
    (Rt.Option.is_none (RootTable.far_vector buf tbl));
  Alcotest.(check bool)
    "far_string absent"
    true
    (Rt.Option.is_none (RootTable.far_string buf tbl));
  Alcotest.(check bool)
    "big_vector absent"
    true
    (Rt.Option.is_none (RootTable.big_vector buf tbl));
  Alcotest.(check bool)
    "near_string absent"
    true
    (Rt.Option.is_none (RootTable.near_string buf tbl));
  Alcotest.(check int32) "a default" 0l (RootTable.a buf tbl)
;;

let test_cases =
  Alcotest.
    [ test_case "64-bit offset roundtrip" `Quick check_offset64_roundtrip
    ; test_case "64-bit absent fields" `Quick check_absent_fields
    ]
;;
