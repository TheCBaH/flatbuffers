let check_monsterdata ?(size_prefixed = false) buf =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let open Rt in
  Alcotest.(check bool)
    "has_ident"
    true
    (Monster.has_identifier Flatbuffers.Primitives.Bytes ~size_prefixed buf);
  let (Root (buf, m)) = Monster.root ~size_prefixed Flatbuffers.Primitives.Bytes buf in
  Alcotest.(check int) "hp" 80 (Monster.hp buf m);
  Alcotest.(check int) "mana" 150 (Monster.mana buf m);
  Alcotest.(check string) "name" "MyMonster" (Monster.name buf m |> String.to_string buf);
  Alcotest.(check bool) "color" true (Monster.color buf m == Color.blue);
  let pos = Monster.pos buf m in
  Alcotest.(check bool) "pos is some" true (Monster.pos buf m |> Option.is_some);
  let pos = Option.get pos in
  Alcotest.(check (float 0.)) "pos.x" 1.0 (Vec3.x buf pos);
  Alcotest.(check (float 0.)) "pos.y" 2.0 (Vec3.y buf pos);
  Alcotest.(check (float 0.)) "pos.z" 3.0 (Vec3.z buf pos);
  Alcotest.(check (float 0.)) "pos.test1" 3.0 (Vec3.test1 buf pos);
  Alcotest.(check char) "pos.test2" (Color.green :> char) (Vec3.test2 buf pos :> char);
  let test3 = Vec3.test3 buf pos in
  Alcotest.(check int) "pos.test3.a" 5 (Test.a buf test3);
  Alcotest.(check int) "pos.test3.b" 6 (Test.b buf test3);
  let test_type = Monster.test_type buf m in
  Alcotest.(check char) "test_type" (Any.monster :> char) (test_type :> char);
  let monster2 =
    Monster.test buf m ~monster:Fun.id ~default:(fun _ -> failwith "unexpected union")
  in
  Alcotest.(check string)
    "test.name"
    "Fred"
    (Monster.name buf monster2 |> String.to_string buf);
  let inventory = Monster.inventory buf m in
  Alcotest.(check bool) "inventory is some" true (Option.is_some inventory);
  let inventory = Option.get inventory in
  Alcotest.(check (array char))
    "inventory"
    [| '\x00'; '\x01'; '\x02'; '\x03'; '\x04' |]
    (UByte.Vector.to_array buf inventory);
  let vec_of_doubles = Monster.vector_of_doubles buf m in
  Alcotest.(check bool) "vector_of_doubles is some" true (Option.is_some vec_of_doubles);
  let vec_of_doubles = Option.get vec_of_doubles in
  Alcotest.(check (array (float 0.)))
    "vector_of_doubles"
    [| -1.7976931348623157e+308; 0.; 1.7976931348623157e+308 |]
    (Double.Vector.to_array buf vec_of_doubles);
  let test4 = Monster.test4 buf m in
  Alcotest.(check bool) "test4 is some" true (Option.is_some test4);
  let test4 = Option.get test4 in
  Alcotest.(check int) "test4 length" 2 (Test.Vector.length buf test4);
  (* don't check order since cpp vs. python example data write these differently *)
  Alcotest.(check int)
    "test4 sum"
    100
    (Test.Vector.to_seq buf test4
     |> Seq.fold_left (fun a t -> a + Test.a buf t + Test.b buf t) 0);
  let aos = Monster.testarrayofstring buf m in
  Alcotest.(check bool) "testarrayofstring is some" true (Option.is_some aos);
  Alcotest.(check (list string))
    "testarrayofstring"
    [ "test1"; "test2" ]
    (Option.get aos |> String.Vector.to_list buf |> List.map (String.to_string buf));
  let aot = Monster.testarrayoftables buf m in
  Alcotest.(check bool) "testarrayoftables is none" true (Option.is_none aot)
;;

let enum_name () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  Alcotest.(check string) "Color.to_string" "red" Color.(to_string red);
  Alcotest.(check string) "Color.to_string" "blue" Color.(to_string blue);
  Alcotest.(check string) "Color.to_string" "green" Color.(to_string green)
;;

let check_serialized path () = check_monsterdata (Fixtures.bytes_of_file path)

let check_generated ?(size_prefixed = false) () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf =
    Fixtures.create_example_monster b
    |> Monster.finish_buf Flatbuffers.Primitives.Bytes ~size_prefixed b
  in
  check_monsterdata ~size_prefixed buf
;;

let check_generated_matches ?(size_prefixed = false) path () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf =
    Fixtures.create_example_monster b
    |> Monster.finish_buf Flatbuffers.Primitives.Bytes ~size_prefixed b
  in
  let buf' = Fixtures.bytes_of_file path in
  Fixtures.check_bytes_chunked buf' buf
;;

let check_generated_reset () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* build first example *)
  let buf =
    Fixtures.create_example_monster b |> Monster.finish_buf Flatbuffers.Primitives.Bytes b
  in
  Rt.Builder.reset b;
  (* write another monster *)
  Fixtures.create_monster
    b
    ~pos:(0., 0., 0., 0., MyGame.Example.Color.blue, (0, 0))
    ~hp:42
    ~name:"AnotherMonster"
    ~inv:(Array.make 100 '\xff')
    ~name2:"ChildMonsterName"
    ~test4:[||]
    ~strings:[||]
    ~longs:[||]
    ~doubles:[||]
  |> ignore;
  Rt.Builder.reset b;
  (* write second example *)
  Fixtures.create_example_monster b
  |> Monster.finish_buf Flatbuffers.Primitives.Bytes b
  |> Fixtures.check_bytes_chunked buf
;;

let check_defaults_not_written () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf =
    Monster.Builder.(start b |> finish)
    |> Monster.finish_buf Flatbuffers.Primitives.Bytes b
  in
  let buf' =
    Monster.Builder.(
      start b
      |> add_hp 100
      |> add_mana 150
      |> add_color Color.blue
      |> add_testf2 3.
      |> add_testhashs32_fnv1 0l
      |> add_testhashu64_fnv1 0L
      |> add_inf_default infinity
      |> add_nan_default nan
      |> finish)
    |> Monster.finish_buf Flatbuffers.Primitives.Bytes b
  in
  Alcotest.(check bytes) "buffers are identical" buf buf'
;;

let check_default_layout ?(size_prefixed = false) () =
  let open Fixtures.Monster_test in
  (* 0 of 62 fields populated *)
  let n_fields = 0 in
  let vtable_size =
    (n_fields * 2) (* vtable field offsets *)
    + 2 (* uint16 vtable size *)
    + 2 (* uint16 inline size *)
  in
  (* just soffset to vtable *)
  let monster_inline_size = 4 in
  (* initial offset to monster table plus identifier *)
  let fb_header_size = 8 in
  (* don't need to pad for initial offset alignment *)
  Alcotest.(check int) "no padding" 0 ((monster_inline_size + vtable_size) mod 4);
  let monster_size = fb_header_size + vtable_size + monster_inline_size in
  let buffer_size = monster_size + if size_prefixed then 4 else 0 in
  (* manually construct a default monster buffer *)
  let layout = Bytes.make buffer_size '\000' in
  let monster_table_start = buffer_size - monster_inline_size in
  Bytes.set_int32_le layout monster_table_start (Int32.of_int vtable_size);
  let vtable_start = monster_table_start - vtable_size in
  Bytes.set_uint16_le layout vtable_start vtable_size;
  Bytes.set_uint16_le layout (vtable_start + 2) monster_inline_size;
  (* construct header *)
  let buf_start = if size_prefixed then 4 else 0 in
  let monster_offset = monster_size - monster_inline_size in
  Bytes.set_int32_le layout buf_start (Int32.of_int monster_offset);
  Bytes.blit_string "MONS" 0 layout (buf_start + 4) 4;
  if size_prefixed then Bytes.set_int32_le layout 0 (Int32.of_int monster_size);
  (* check against builder output *)
  let b = Rt.Builder.create () in
  let buf =
    MyGame.Example.Monster.(
      Builder.(start b |> finish)
      |> finish_buf Flatbuffers.Primitives.Bytes ~size_prefixed b)
  in
  Alcotest.(check int) "buffer size" (Bytes.length layout) (Bytes.length buf);
  Alcotest.(check bytes) "buffer layout" layout buf
;;

let check_monster_extra_floats () =
  let open Fixtures.Monster_extra in
  let open MyGame in
  let b = Rt.Builder.create () in
  let buf =
    MonsterExtra.(
      Builder.(start b |> finish) |> finish_buf Flatbuffers.Primitives.Bytes b)
  in
  let (Rt.Root (buf, mon)) = MonsterExtra.root Flatbuffers.Primitives.Bytes buf in
  Alcotest.(check bool) "is_nan d0" true (Float.is_nan (MonsterExtra.d0 buf mon));
  Alcotest.(check bool) "is_nan d1" true (Float.is_nan (MonsterExtra.d1 buf mon));
  Alcotest.(check (float 0.)) "d2" Float.infinity (MonsterExtra.d2 buf mon);
  Alcotest.(check (float 0.)) "d3" Float.neg_infinity (MonsterExtra.d3 buf mon);
  Alcotest.(check bool) "is_nan d0" true (Float.is_nan (MonsterExtra.f0 buf mon));
  Alcotest.(check bool) "is_nan d1" true (Float.is_nan (MonsterExtra.f1 buf mon));
  Alcotest.(check (float 0.)) "d2" Float.infinity (MonsterExtra.f2 buf mon);
  Alcotest.(check (float 0.)) "d3" Float.neg_infinity (MonsterExtra.f3 buf mon)
;;

let check_nested_flatbuffer_roundtrip () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* Build an inner monster as a finished flatbuffer *)
  let inner_name = Rt.String.create b "NestedMonster" in
  let inner_wip =
    Monster.Builder.(start b |> add_name inner_name |> add_hp 42 |> finish)
  in
  let inner_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b inner_wip in
  (* Prepare vectors/strings before starting the outer table *)
  let outer_name = Rt.String.create b "OuterMonster" in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  (* Build an outer monster with the inner as nested flatbuffer *)
  let outer_wip =
    Monster.Builder.(
      start b
      |> add_name outer_name
      |> add_testnestedflatbuffer nested_vec
      |> finish)
  in
  let outer_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b outer_wip in
  (* Read back outer monster *)
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bytes outer_buf in
  Alcotest.(check string)
    "outer name"
    "OuterMonster"
    (Monster.name buf m |> Rt.String.to_string buf);
  (* Read nested flatbuffer *)
  let nested = Monster.testnestedflatbuffer_as_monster buf m in
  Alcotest.(check bool) "nested is Some" true (Option.is_some nested);
  let (Rt.Root (nbuf, nm)) = Option.get nested in
  Alcotest.(check string)
    "nested name"
    "NestedMonster"
    (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  Alcotest.(check int) "nested hp" 42 (Monster.hp nbuf nm)
;;

let check_nested_flatbuffer_absent () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let name = Rt.String.create b "NoNested" in
  let wip = Monster.Builder.(start b |> add_name name |> finish) in
  let buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bytes buf in
  let nested = Monster.testnestedflatbuffer_as_monster buf m in
  Alcotest.(check bool) "nested is None" true (Option.is_none nested)
;;

let check_nested_flatbuffer_raw_access () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* Build inner monster *)
  let inner_name = Rt.String.create b "RawNested" in
  let inner_wip =
    Monster.Builder.(start b |> add_name inner_name |> finish)
  in
  let inner_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b inner_wip in
  (* Prepare vectors/strings before starting the outer table *)
  let outer_name = Rt.String.create b "OuterRaw" in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  (* Build outer with nested *)
  let outer_wip =
    Monster.Builder.(
      start b
      |> add_name outer_name
      |> add_testnestedflatbuffer nested_vec
      |> finish)
  in
  let outer_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b outer_wip in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bytes outer_buf in
  (* Raw UByte vector access should also work *)
  let raw_vec = Monster.testnestedflatbuffer buf m in
  Alcotest.(check bool) "raw vec is some" true (Rt.Option.is_some raw_vec);
  let raw_vec = Rt.Option.get raw_vec in
  let raw_len = Rt.UByte.Vector.length buf raw_vec in
  Alcotest.(check int) "raw vec length" (Bytes.length inner_buf) raw_len
;;

(* Helper: build a nested monster buffer with the given fields *)
let build_nested_monster_buf b ~name ~hp ~mana =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let n = Rt.String.create b name in
  let inv = Rt.UByte.Vector.create b [| '\x01'; '\x02'; '\x03' |] in
  let wip =
    Monster.Builder.(start b |> add_name n |> add_hp hp |> add_mana mana |> add_inventory inv |> finish)
  in
  Monster.finish_buf Flatbuffers.Primitives.Bytes b wip
;;

(* Helper: build an outer monster with a nested flatbuffer *)
let build_outer_with_nested b ~outer_name ~inner_buf =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let name = Rt.String.create b outer_name in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  let wip =
    Monster.Builder.(
      start b |> add_name name |> add_testnestedflatbuffer nested_vec |> finish)
  in
  Monster.finish_buf Flatbuffers.Primitives.Bytes b wip
;;

let check_nested_string_buffer () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_buf = build_nested_monster_buf b ~name:"StringNested" ~hp:99 ~mana:200 in
  let outer_buf = build_outer_with_nested b ~outer_name:"StringOuter" ~inner_buf in
  (* Read via String buffer type *)
  let outer_str = Bytes.to_string outer_buf in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.String outer_str in
  let nested = Monster.testnestedflatbuffer_as_monster buf m in
  Alcotest.(check bool) "nested is Some" true (Option.is_some nested);
  let (Rt.Root (nbuf, nm)) = Option.get nested in
  Alcotest.(check string) "name" "StringNested" (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  Alcotest.(check int) "hp" 99 (Monster.hp nbuf nm);
  Alcotest.(check int) "mana" 200 (Monster.mana nbuf nm)
;;

let check_nested_bigstring_buffer () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_buf = build_nested_monster_buf b ~name:"BigstringNested" ~hp:77 ~mana:300 in
  let outer_buf = build_outer_with_nested b ~outer_name:"BigstringOuter" ~inner_buf in
  (* Read via Bigstring buffer type *)
  let outer_bs =
    Bigstringaf.of_string ~off:0 ~len:(Bytes.length outer_buf) (Bytes.to_string outer_buf)
  in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bigstring outer_bs in
  let nested = Monster.testnestedflatbuffer_as_monster buf m in
  Alcotest.(check bool) "nested is Some" true (Option.is_some nested);
  let (Rt.Root (nbuf, nm)) = Option.get nested in
  Alcotest.(check string) "name" "BigstringNested" (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  Alcotest.(check int) "hp" 77 (Monster.hp nbuf nm);
  Alcotest.(check int) "mana" 300 (Monster.mana nbuf nm)
;;

let check_nested_rich_inner () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* Build a rich inner monster with pos, inventory, test4, strings *)
  let inner_name = Rt.String.create b "RichInner" in
  let inner_inv = Rt.UByte.Vector.create b [| '\x0a'; '\x0b'; '\x0c'; '\x0d' |] in
  let inner_test4 = Test.Vector.create b [| (10, 20); (30, 40) |] in
  let inner_strings =
    Array.map (Rt.String.create b) [| "hello"; "world" |]
    |> Rt.String.Vector.create b
  in
  let inner_wip =
    Monster.Builder.(
      start b
      |> add_name inner_name
      |> add_hp 55
      |> add_pos (1.5, 2.5, 3.5, 0., Color.red, (7, 8))
      |> add_inventory inner_inv
      |> add_test4 inner_test4
      |> add_testarrayofstring inner_strings
      |> finish)
  in
  let inner_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b inner_wip in
  let outer_buf = build_outer_with_nested b ~outer_name:"RichOuter" ~inner_buf in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bytes outer_buf in
  let (Rt.Root (nbuf, nm)) = Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
  (* Verify all inner fields *)
  Alcotest.(check string) "name" "RichInner" (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  Alcotest.(check int) "hp" 55 (Monster.hp nbuf nm);
  let pos = Rt.Option.get (Monster.pos nbuf nm) in
  Alcotest.(check (float 0.)) "pos.x" 1.5 (Vec3.x nbuf pos);
  Alcotest.(check (float 0.)) "pos.y" 2.5 (Vec3.y nbuf pos);
  Alcotest.(check (float 0.)) "pos.z" 3.5 (Vec3.z nbuf pos);
  let inv = Rt.Option.get (Monster.inventory nbuf nm) in
  Alcotest.(check (array char)) "inventory" [| '\x0a'; '\x0b'; '\x0c'; '\x0d' |]
    (Rt.UByte.Vector.to_array nbuf inv);
  let t4 = Rt.Option.get (Monster.test4 nbuf nm) in
  Alcotest.(check int) "test4 length" 2 (Test.Vector.length nbuf t4);
  let aos = Rt.Option.get (Monster.testarrayofstring nbuf nm) in
  Alcotest.(check (list string)) "strings" [ "hello"; "world" ]
    (Rt.String.Vector.to_list nbuf aos |> List.map (Rt.String.to_string nbuf))
;;

let check_nested_testrequired () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_buf = build_nested_monster_buf b ~name:"Required" ~hp:33 ~mana:44 in
  (* Use testrequirednestedflatbuffer field *)
  let name = Rt.String.create b "RequiredOuter" in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  let wip =
    Monster.Builder.(
      start b |> add_name name |> add_testrequirednestedflatbuffer nested_vec |> finish)
  in
  let outer_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bytes outer_buf in
  (* testnestedflatbuffer (optional) should be absent *)
  Alcotest.(check bool) "optional nested absent" true
    (Option.is_none (Monster.testnestedflatbuffer_as_monster buf m));
  (* testrequirednestedflatbuffer should be present *)
  let nested = Monster.testrequirednestedflatbuffer_as_monster buf m in
  Alcotest.(check bool) "required nested present" true (Option.is_some nested);
  let (Rt.Root (nbuf, nm)) = Option.get nested in
  Alcotest.(check string) "name" "Required" (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  Alcotest.(check int) "hp" 33 (Monster.hp nbuf nm)
;;

let check_nested_builder_reset () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* Build first nested monster *)
  let buf1 =
    let inner = build_nested_monster_buf b ~name:"First" ~hp:1 ~mana:2 in
    build_outer_with_nested b ~outer_name:"Outer1" ~inner_buf:inner
  in
  Rt.Builder.reset b;
  (* Build second nested monster after reset *)
  let buf2 =
    let inner = build_nested_monster_buf b ~name:"Second" ~hp:3 ~mana:4 in
    build_outer_with_nested b ~outer_name:"Outer2" ~inner_buf:inner
  in
  (* Verify first *)
  let (Rt.Root (b1, m1)) = Monster.root Flatbuffers.Primitives.Bytes buf1 in
  let (Rt.Root (nb1, nm1)) = Option.get (Monster.testnestedflatbuffer_as_monster b1 m1) in
  Alcotest.(check string) "first outer" "Outer1"
    (Monster.name b1 m1 |> Rt.String.to_string b1);
  Alcotest.(check string) "first inner" "First"
    (Monster.name nb1 nm1 |> Rt.String.to_string nb1);
  Alcotest.(check int) "first hp" 1 (Monster.hp nb1 nm1);
  (* Verify second *)
  let (Rt.Root (b2, m2)) = Monster.root Flatbuffers.Primitives.Bytes buf2 in
  let (Rt.Root (nb2, nm2)) = Option.get (Monster.testnestedflatbuffer_as_monster b2 m2) in
  Alcotest.(check string) "second outer" "Outer2"
    (Monster.name b2 m2 |> Rt.String.to_string b2);
  Alcotest.(check string) "second inner" "Second"
    (Monster.name nb2 nm2 |> Rt.String.to_string nb2);
  Alcotest.(check int) "second hp" 3 (Monster.hp nb2 nm2)
;;

let check_nested_defaults () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* Build inner monster with only required name, rely on defaults *)
  let inner_name = Rt.String.create b "DefaultsInner" in
  let inner_wip = Monster.Builder.(start b |> add_name inner_name |> finish) in
  let inner_buf = Monster.finish_buf Flatbuffers.Primitives.Bytes b inner_wip in
  let outer_buf = build_outer_with_nested b ~outer_name:"DefaultsOuter" ~inner_buf in
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bytes outer_buf in
  let (Rt.Root (nbuf, nm)) = Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
  (* Verify default values are returned correctly from nested *)
  Alcotest.(check int) "default hp" 100 (Monster.hp nbuf nm);
  Alcotest.(check int) "default mana" 150 (Monster.mana nbuf nm);
  Alcotest.(check bool) "pos absent" true (Rt.Option.is_none (Monster.pos nbuf nm));
  Alcotest.(check bool) "inventory absent" true (Rt.Option.is_none (Monster.inventory nbuf nm))
;;

let check_extension_ident () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  Alcotest.(check (option string)) "extension" (Some "mon") Monster.extension;
  Alcotest.(check (option string)) "identifier" (Some "MONS") Monster.identifier
;;

let gold_monsterdata = "../../flatbuffers/tests/monsterdata_test.mon"
let python_monsterdata = "../../flatbuffers/tests/monsterdata_python_wire.mon"

let test_cases =
  Alcotest.
    [ test_case "Read gold monsterdata" `Quick (check_serialized gold_monsterdata)
    ; test_case "Read python monsterdata" `Quick (check_serialized python_monsterdata)
    ; test_case "Read generated monster" `Quick check_generated
    ; test_case "Read gen size-prefixed" `Quick (check_generated ~size_prefixed:true)
    ; test_case "Check nan and inf float defaults" `Quick check_monster_extra_floats
    ; test_case "Reusing builder produces same output" `Quick check_generated_reset
    ; test_case "Check default scalars not written" `Quick check_defaults_not_written
    ; test_case "Check default monster layout" `Quick check_default_layout
    ; test_case
        "Check size-prefixed layout"
        `Quick
        (check_default_layout ~size_prefixed:true)
    ; test_case
        "Check gen matches python"
        `Quick
        (check_generated_matches python_monsterdata)
    ; test_case "Check enum names" `Quick enum_name
    ; test_case "Nested flatbuffer roundtrip" `Quick check_nested_flatbuffer_roundtrip
    ; test_case "Nested flatbuffer absent" `Quick check_nested_flatbuffer_absent
    ; test_case "Nested flatbuffer raw access" `Quick check_nested_flatbuffer_raw_access
    ; test_case "Nested via String buffer" `Quick check_nested_string_buffer
    ; test_case "Nested via Bigstring buffer" `Quick check_nested_bigstring_buffer
    ; test_case "Nested rich inner monster" `Quick check_nested_rich_inner
    ; test_case "Nested testrequired field" `Quick check_nested_testrequired
    ; test_case "Nested with builder reset" `Quick check_nested_builder_reset
    ; test_case "Nested default values" `Quick check_nested_defaults
    ]
;;
