open Generated.More_defaults

let p = Flatbuffers.Primitives.Bytes

let () =
  Alcotest.run "More_defaults" [
    "MoreDefaults", [
      Alcotest.test_case "Empty table defaults" `Quick (fun () ->
        let b = Rt.Builder.create () in
        let t = MoreDefaults.Builder.start b in
        let off = MoreDefaults.Builder.finish t in
        let buf = MoreDefaults.finish_buf p b off in
        let (Rt.Root (b_, o_)) = MoreDefaults.root p buf in
        let obj = MoreDefaults.unpack b_ o_ in
        Alcotest.(check int) "ints length" 0 (Array.length obj.ints);
        Alcotest.(check int) "floats length" 0 (Array.length obj.floats);
        Alcotest.(check string) "empty_string" "" obj.empty_string;
        Alcotest.(check string) "some_string" "" obj.some_string;
        Alcotest.(check int) "abcs length" 0 (Array.length obj.abcs);
        Alcotest.(check int) "bools length" 0 (Array.length obj.bools));

      Alcotest.test_case "Object API roundtrip" `Quick (fun () ->
        let b = Rt.Builder.create () in
        let original : MoreDefaults.obj = {
          ints = [| 1l; 2l; 3l |];
          floats = [| 1.5; 2.5 |];
          empty_string = "hello";
          some_string = "world";
          abcs = [| Abc.a; Abc.c |];
          bools = [| true; false; true |];
        } in
        let off = MoreDefaults.pack b original in
        let buf = MoreDefaults.finish_buf p b off in
        let (Rt.Root (b_, o_)) = MoreDefaults.root p buf in
        let obj = MoreDefaults.unpack b_ o_ in
        Alcotest.(check int) "ints length" 3 (Array.length obj.ints);
        Alcotest.(check (list int32)) "ints values" [1l; 2l; 3l] (Array.to_list obj.ints);
        Alcotest.(check int) "floats length" 2 (Array.length obj.floats);
        Alcotest.(check string) "empty_string" "hello" obj.empty_string;
        Alcotest.(check string) "some_string" "world" obj.some_string;
        Alcotest.(check int) "abcs length" 2 (Array.length obj.abcs);
        Alcotest.(check int) "bools length" 3 (Array.length obj.bools));
    ]
  ]
