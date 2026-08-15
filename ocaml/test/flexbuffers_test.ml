module F = Flatbuffers.Flexbuffers

let gold_file = "../../flatbuffers/tests/gold_flexbuffer_example.bin"

let require = function
  | Some value -> value
  | None -> Alcotest.fail "unexpected FlexBuffer value type"
;;

let verify_ok prim storage =
  match F.root_verified prim storage with
  | Ok root -> root
  | Error error -> Alcotest.failf "FlexBuffer verification failed: %a" F.pp_error error
;;

let rec walk value =
  match F.value_type value with
  | Null -> ()
  | Bool -> ignore (require (F.as_bool value) : bool)
  | Int | Indirect_int -> ignore (require (F.as_int64 value) : int64)
  | UInt | Indirect_uint -> ignore (require (F.as_uint64_bits value) : int64)
  | Float | Indirect_float -> ignore (require (F.as_float value) : float)
  | Key -> ignore (require (F.as_key value) : string)
  | String -> ignore (require (F.as_string value) : string)
  | Blob ->
    let blob = require (F.as_blob value) in
    ignore (F.Blob.to_bytes blob : bytes)
  | Map ->
    let map = require (F.as_map value) in
    for i = 0 to F.Map.length map - 1 do
      let _, value = F.Map.get map i in
      walk value
    done
  | Vector
  | Vector_int
  | Vector_uint
  | Vector_float
  | Vector_key
  | Vector_string_deprecated
  | Vector_int2
  | Vector_uint2
  | Vector_float2
  | Vector_int3
  | Vector_uint3
  | Vector_float3
  | Vector_int4
  | Vector_uint4
  | Vector_float4
  | Vector_bool -> F.Vector.iter walk (require (F.as_vector value))
;;

let check_gold_with prim storage =
  let root = verify_ok prim storage in
  walk root;
  let map = require (F.as_map root) in
  Alcotest.(check int) "map length" 7 (F.Map.length map);
  let bool = require (F.Map.find map "bool") |> F.as_bool |> require in
  Alcotest.(check bool) "bool" true bool;
  let foo = require (F.Map.find map "foo") |> F.as_float |> require in
  Alcotest.(check (float 0.)) "float" 100. foo;
  let bar = require (F.Map.find map "bar") |> F.as_vector |> require in
  Alcotest.(check int) "typed vector length" 3 (F.Vector.length bar);
  Alcotest.(check int64)
    "typed vector value"
    3L
    (F.Vector.get bar 2 |> F.as_int64 |> require);
  let bar3 = require (F.Map.find map "bar3") |> F.as_vector |> require in
  Alcotest.(check int) "fixed vector length" 3 (F.Vector.length bar3);
  let nested = require (F.Map.find map "mymap") |> F.as_map |> require in
  let fred = require (F.Map.find nested "foo") |> F.as_string |> require in
  Alcotest.(check string) "nested string" "Fred" fred;
  let vec = require (F.Map.find map "vec") |> F.as_vector |> require in
  Alcotest.(check int64)
    "untyped int"
    (-100L)
    (F.Vector.get vec 0 |> F.as_int64 |> require);
  Alcotest.(check string)
    "untyped string"
    "Fred"
    (F.Vector.get vec 1 |> F.as_string |> require);
  let blob = F.Vector.get vec 3 |> F.as_blob |> require in
  Alcotest.(check bytes) "blob" (Bytes.of_string "M") (F.Blob.to_bytes blob)
;;

let check_gold () =
  let bytes = Fixtures.bytes_of_file gold_file in
  check_gold_with Flatbuffers.Primitives.Bytes bytes;
  check_gold_with Flatbuffers.Primitives.String (Bytes.to_string bytes);
  check_gold_with
    Flatbuffers.Primitives.Bigstring
    (Bigstringaf.of_string ~off:0 ~len:(Bytes.length bytes) (Bytes.to_string bytes))
;;

let set_uint_le bytes pos width value =
  match width with
  | 1 -> Bytes.set_uint8 bytes pos (Int64.to_int value)
  | 2 -> Bytes.set_int16_le bytes pos (Int64.to_int value)
  | 4 -> Bytes.set_int32_le bytes pos (Int64.to_int32 value)
  | 8 -> Bytes.set_int64_le bytes pos value
  | _ -> assert false
;;

let scalar_root typ width value =
  let bytes = Bytes.make (width + 2) '\000' in
  set_uint_le bytes 0 width value;
  Bytes.set_uint8
    bytes
    width
    ((typ lsl 2)
     lor if width = 1 then 0 else if width = 2 then 1 else if width = 4 then 2 else 3);
  Bytes.set_uint8 bytes (width + 1) width;
  bytes
;;

let check_scalar_widths () =
  List.iter
    (fun width ->
       let int_root = verify_ok Flatbuffers.Primitives.Bytes (scalar_root 1 width 42L) in
       Alcotest.(check int64) "int width" 42L (F.as_int64 int_root |> require);
       let uint_root = verify_ok Flatbuffers.Primitives.Bytes (scalar_root 2 width 42L) in
       Alcotest.(check int64) "uint width" 42L (F.as_uint64_bits uint_root |> require);
       let float_root = verify_ok Flatbuffers.Primitives.Bytes (scalar_root 3 width 0L) in
       ignore (require (F.as_float float_root) : float))
    [ 1; 2; 4; 8 ];
  let null_root = verify_ok Flatbuffers.Primitives.Bytes (scalar_root 0 1 0L) in
  Alcotest.(check bool) "null" true (F.is_null null_root);
  let bool_root = verify_ok Flatbuffers.Primitives.Bytes (scalar_root 26 1 1L) in
  Alcotest.(check (option bool)) "bool" (Some true) (F.as_bool bool_root)
;;

let check_corruption_sweep () =
  let bytes = Fixtures.bytes_of_file gold_file in
  for length = 0 to Bytes.length bytes - 1 do
    match F.root_verified ~len:length Flatbuffers.Primitives.Bytes bytes with
    | Error _ -> ()
    | Ok root -> walk root
  done;
  for i = 0 to Bytes.length bytes - 1 do
    let mutated = Bytes.copy bytes in
    Bytes.set_uint8 mutated i (Bytes.get_uint8 mutated i lxor 0xff);
    match F.root_verified Flatbuffers.Primitives.Bytes mutated with
    | Error _ -> ()
    | Ok root -> walk root
  done
;;

let check_region_and_limits () =
  let escaping_key = Bytes.of_string "\000\001\016\001" in
  (match F.verify ~off:1 ~len:3 Flatbuffers.Primitives.Bytes escaping_key with
   | Error { kind = F.Invalid_offset; _ } -> ()
   | Error error -> Alcotest.failf "wrong region error: %a" F.pp_error error
   | Ok () -> Alcotest.fail "FlexBuffer escaped its containing region");
  let bytes = Fixtures.bytes_of_file gold_file in
  let options = { F.default_options with max_depth = 1 } in
  (match F.verify ~options Flatbuffers.Primitives.Bytes bytes with
   | Error { kind = F.Depth_limit_exceeded; _ } -> ()
   | Error error -> Alcotest.failf "wrong depth error: %a" F.pp_error error
   | Ok () -> Alcotest.fail "depth limit was not enforced");
  let options = { F.default_options with max_values = 1 } in
  match F.verify ~options Flatbuffers.Primitives.Bytes bytes with
  | Error { kind = F.Value_limit_exceeded; _ } -> ()
  | Error error -> Alcotest.failf "wrong value-count error: %a" F.pp_error error
  | Ok () -> Alcotest.fail "value-count limit was not enforced"
;;

let check_generated_accessor () =
  let module M = Fixtures.Monster_test.MyGame.Example.Monster in
  let module Rt = Fixtures.Monster_test.Rt in
  let builder = Rt.Builder.create () in
  let name = Rt.String.create builder "FlexMonster" in
  let encoded = Rt.UByte.Vector.create builder [| '\xd2'; '\x04'; '\x05'; '\x02' |] in
  let monster =
    M.Builder.(start builder |> add_name name |> add_flex encoded |> finish)
  in
  let bytes = M.finish_buf Flatbuffers.Primitives.Bytes builder monster in
  let (Fixtures.Monster_test.Rt.Root (buf, monster)) =
    match M.root_verified Flatbuffers.Primitives.Bytes bytes with
    | Ok root -> root
    | Error error ->
      Alcotest.failf
        "generated verification failed: %a"
        Flatbuffers.Verifier.pp_error
        error
  in
  let flex = M.flex_flexbuffer_root buf monster |> require in
  Alcotest.(check int64) "generated flexbuffer accessor" 1234L (F.as_int64 flex |> require);
  let builder = Rt.Builder.create () in
  let name = Rt.String.create builder "InvalidFlexMonster" in
  let encoded = Rt.UByte.Vector.create builder [| '\xd2'; '\x04'; '\xff'; '\x02' |] in
  let monster =
    M.Builder.(start builder |> add_name name |> add_flex encoded |> finish)
  in
  let bytes = M.finish_buf Flatbuffers.Primitives.Bytes builder monster in
  (match M.verify Flatbuffers.Primitives.Bytes bytes with
   | Error { kind = Flatbuffers.Verifier.Invalid_flexbuffer _; _ } -> ()
   | Error error ->
     Alcotest.failf "wrong generated error: %a" Flatbuffers.Verifier.pp_error error
   | Ok () -> Alcotest.fail "generated verifier accepted an invalid FlexBuffer");
  let options = { Flatbuffers.Verifier.default_options with check_flexbuffers = false } in
  match M.verify ~options Flatbuffers.Primitives.Bytes bytes with
  | Ok () -> ()
  | Error error ->
    Alcotest.failf
      "disabled FlexBuffer check failed: %a"
      Flatbuffers.Verifier.pp_error
      error
;;

let test_cases =
  Alcotest.
    [ test_case "upstream golden buffer" `Quick check_gold
    ; test_case "scalar widths" `Quick check_scalar_widths
    ; test_case "truncation and mutation sweep" `Quick check_corruption_sweep
    ; test_case "region and work limits" `Quick check_region_and_limits
    ; test_case "generated accessor and verifier" `Quick check_generated_accessor
    ]
;;
