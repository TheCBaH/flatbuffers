module P = Flatbuffers.Primitives

let unsigned32 value = Int64.logand (Int64.of_int32 value) 0xFFFF_FFFFL

let check_uoffset (name, value) =
  let bytes = Bytes.create 4 in
  P.set_int32_le bytes 0 value;
  let unsigned = unsigned32 value in
  if Int64.compare unsigned (Int64.of_int max_int) <= 0
  then Alcotest.(check int) name (Int64.to_int unsigned) (P.get_uoffset P.Bytes bytes 0)
  else
    Alcotest.check_raises name (Failure "int32_unsigned_to_int overflow") (fun () ->
      ignore (P.get_uoffset P.Bytes bytes 0))
;;

let check_uoffset_boundaries () =
  let platform_boundaries =
    if Sys.word_size = 32
    then
      [ "max_int", Int32.of_int max_int
      ; ("max_int + 1", Int64.(add (of_int Stdlib.max_int) 1L |> to_int32))
      ]
    else []
  in
  List.iter
    check_uoffset
    ([ "zero", 0l
     ; "0x7fffffff", Int32.max_int
     ; "0x80000000", Int32.min_int
     ; "0xffffffff", Int32.minus_one
     ]
     @ platform_boundaries)
;;

let test_cases =
  Alcotest.[ test_case "unsigned uoffset boundaries" `Quick check_uoffset_boundaries ]
;;
