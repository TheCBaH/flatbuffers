let check_nested_struct_roundtrip () =
  let open Fixtures.Arrays_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let nested : NestedStruct.t =
    ([| 10l; 20l |], TestEnum.a, [| TestEnum.b; TestEnum.c |], [| 100L; 200L |])
  in
  let arr_struct : ArrayStruct.t =
    ( 3.14,
      Array.init 15 (fun i -> Int32.of_int (i + 1)),
      0x42,
      [| nested; ([| 30l; 40l |], TestEnum.c, [| TestEnum.a; TestEnum.b |], [| 300L; 400L |]) |],
      99l,
      [| 1000L; 2000L |] )
  in
  let wip = ArrayTable.Builder.(start b |> add_a arr_struct |> finish) in
  let buf = ArrayTable.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, tbl)) = ArrayTable.root Flatbuffers.Primitives.Bytes buf in
  let s = Rt.Option.get (ArrayTable.a buf tbl) in
  (* Check scalar field *)
  Alcotest.(check (float 0.01)) "a" 3.14 (ArrayStruct.a buf s);
  (* Check scalar array *)
  Alcotest.(check int) "b_length" 15 ArrayStruct.b_length;
  Alcotest.(check int32) "b[0]" 1l (ArrayStruct.b buf s 0);
  Alcotest.(check int32) "b[7]" 8l (ArrayStruct.b buf s 7);
  Alcotest.(check int32) "b[14]" 15l (ArrayStruct.b buf s 14);
  (* Check byte field *)
  Alcotest.(check int) "c" 0x42 (ArrayStruct.c buf s);
  (* Check nested struct array *)
  Alcotest.(check int) "d_length" 2 ArrayStruct.d_length;
  let ns0 = ArrayStruct.d buf s 0 in
  Alcotest.(check int32) "d[0].a[0]" 10l (NestedStruct.a buf ns0 0);
  Alcotest.(check int32) "d[0].a[1]" 20l (NestedStruct.a buf ns0 1);
  Alcotest.(check int) "d[0].b" (TestEnum.a :> int) (NestedStruct.b buf ns0 :> int);
  Alcotest.(check int) "d[0].c[0]" (TestEnum.b :> int) (NestedStruct.c buf ns0 0 :> int);
  Alcotest.(check int) "d[0].c[1]" (TestEnum.c :> int) (NestedStruct.c buf ns0 1 :> int);
  Alcotest.(check int64) "d[0].d[0]" 100L (NestedStruct.d buf ns0 0);
  Alcotest.(check int64) "d[0].d[1]" 200L (NestedStruct.d buf ns0 1);
  let ns1 = ArrayStruct.d buf s 1 in
  Alcotest.(check int32) "d[1].a[0]" 30l (NestedStruct.a buf ns1 0);
  Alcotest.(check int64) "d[1].d[1]" 400L (NestedStruct.d buf ns1 1);
  (* Check trailing fields *)
  Alcotest.(check int32) "e" 99l (ArrayStruct.e buf s);
  Alcotest.(check int64) "f[0]" 1000L (ArrayStruct.f buf s 0);
  Alcotest.(check int64) "f[1]" 2000L (ArrayStruct.f buf s 1)
;;

let check_array_absent () =
  let open Fixtures.Arrays_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let wip = ArrayTable.Builder.(start b |> finish) in
  let buf = ArrayTable.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, tbl)) = ArrayTable.root Flatbuffers.Primitives.Bytes buf in
  Alcotest.(check bool) "a is none" true (Rt.Option.is_none (ArrayTable.a buf tbl))
;;

let check_length_constants () =
  let open Fixtures.Arrays_test in
  let open MyGame.Example in
  Alcotest.(check int) "NestedStruct.a_length" 2 NestedStruct.a_length;
  Alcotest.(check int) "NestedStruct.c_length" 2 NestedStruct.c_length;
  Alcotest.(check int) "NestedStruct.d_length" 2 NestedStruct.d_length;
  Alcotest.(check int) "ArrayStruct.b_length" 15 ArrayStruct.b_length;
  Alcotest.(check int) "ArrayStruct.d_length" 2 ArrayStruct.d_length;
  Alcotest.(check int) "ArrayStruct.f_length" 2 ArrayStruct.f_length;
  Alcotest.(check int) "LargeArrayStruct.d_length" 64 LargeArrayStruct.d_length;
  Alcotest.(check int) "LargeArrayStruct.e_length" 64 LargeArrayStruct.e_length;
  Alcotest.(check int) "LargeArrayStruct.g_length" 64 LargeArrayStruct.g_length
;;

let test_cases =
  Alcotest.
    [ test_case "ArrayStruct roundtrip" `Quick check_nested_struct_roundtrip
    ; test_case "Array field absent" `Quick check_array_absent
    ; test_case "Length constants" `Quick check_length_constants
    ]
;;
