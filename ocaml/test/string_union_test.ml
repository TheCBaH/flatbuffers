let check_greeting_roundtrip () =
  let open Fixtures.String_union in
  let b = Rt.Builder.create () in
  let msg = Rt.String.create b "hello" in
  let greeting = Greeting.Builder.(start b |> add_message msg |> finish) in
  let wip = Envelope.Builder.(start b |> add_content_greeting greeting |> finish) in
  let buf = Envelope.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, e)) = Envelope.root Flatbuffers.Primitives.Bytes buf in
  Alcotest.(check char) "content_type is greeting"
    (Content.greeting :> char) (Envelope.content_type buf e :> char);
  let msg_out =
    Envelope.content buf e
      ~greeting:(fun g ->
        Rt.Option.fold ~none:"" ~some:(fun s -> Rt.String.to_string buf s)
          (Greeting.message buf g))
      ~default:(fun _ -> failwith "unexpected union type")
  in
  Alcotest.(check string) "greeting message" "hello" msg_out
;;

let check_string_member_roundtrip () =
  let open Fixtures.String_union in
  let b = Rt.Builder.create () in
  let s = Rt.String.create b "world" in
  let wip = Envelope.Builder.(start b |> add_content_name s |> finish) in
  let buf = Envelope.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, e)) = Envelope.root Flatbuffers.Primitives.Bytes buf in
  Alcotest.(check char) "content_type is name"
    (Content.name :> char) (Envelope.content_type buf e :> char);
  let s_out =
    Envelope.content buf e
      ~name:(fun s -> Rt.String.to_string buf s)
      ~default:(fun _ -> failwith "unexpected union type")
  in
  Alcotest.(check string) "string member" "world" s_out
;;

let check_obj_api_roundtrip () =
  let open Fixtures.String_union in
  (* Pack with greeting variant *)
  let b = Rt.Builder.create () in
  let obj1 : Envelope.obj = {
    content = `Greeting { Greeting.message = Some "hi" };
  } in
  let wip = Envelope.pack b obj1 in
  let buf = Envelope.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, e)) = Envelope.root Flatbuffers.Primitives.Bytes buf in
  let unpacked = Envelope.unpack buf e in
  (match unpacked.content with
   | `Greeting g ->
     Alcotest.(check (option string)) "greeting obj message" (Some "hi") g.message
   | _ -> Alcotest.fail "expected Greeting variant");
  (* Pack with string variant *)
  let b2 = Rt.Builder.create () in
  let obj2 : Envelope.obj = { content = `Name "yo" } in
  let wip2 = Envelope.pack b2 obj2 in
  let buf2 = Envelope.finish_buf Flatbuffers.Primitives.Bytes b2 wip2 in
  let (Rt.Root (buf2, e2)) = Envelope.root Flatbuffers.Primitives.Bytes buf2 in
  let unpacked2 = Envelope.unpack buf2 e2 in
  (match unpacked2.content with
   | `Name s -> Alcotest.(check string) "string obj name" "yo" s
   | _ -> Alcotest.fail "expected Name variant");
  (* Pack with none variant *)
  let b3 = Rt.Builder.create () in
  let obj3 : Envelope.obj = { content = `None_ } in
  let wip3 = Envelope.pack b3 obj3 in
  let buf3 = Envelope.finish_buf Flatbuffers.Primitives.Bytes b3 wip3 in
  let (Rt.Root (buf3, e3)) = Envelope.root Flatbuffers.Primitives.Bytes buf3 in
  let unpacked3 = Envelope.unpack buf3 e3 in
  (match unpacked3.content with
   | `None_ -> ()
   | _ -> Alcotest.fail "expected None variant")
;;

let test_cases =
  Alcotest.
    [ test_case "Greeting union roundtrip" `Quick check_greeting_roundtrip
    ; test_case "String member roundtrip" `Quick check_string_member_roundtrip
    ; test_case "Object API roundtrip" `Quick check_obj_api_roundtrip
    ]
;;
