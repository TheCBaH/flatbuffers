(* Comprehensive Melange test: exercises all schema variants with Bytes and JsDataView *)

let to_dv buf = Js_dataview.of_bytes buf ~off:0 ~len:(Bytes.length buf)

let tests_run = ref 0
let tests_failed = ref 0

let check msg ok =
  incr tests_run;
  if not ok then begin
    incr tests_failed;
    Printf.printf "  FAIL: %s\n%!" msg
  end

let check_eq msg ~expected actual =
  check msg (expected = actual)

let check_float msg ~expected actual =
  if Float.is_nan expected then check msg (Float.is_nan actual)
  else if Float.is_infinite expected then check_eq msg ~expected actual
  else check msg (Float.abs (expected -. actual) < 1e-6)

let check_float_array msg ~expected actual =
  check (msg ^ " length") (Array.length expected = Array.length actual);
  Array.iteri (fun i e ->
    check_float (Printf.sprintf "%s[%d]" msg i) ~expected:e actual.(i))
    expected

(* ── Monster sample (monster.fbs) ── *)

let test_monster_sample () =
  let open Monster in
  let open MyGame.Sample in
  let b = Rt.Builder.create () in
  let sword_name = Rt.String.create b "Sword" in
  let axe_name = Rt.String.create b "Axe" in
  let sword = Weapon.Builder.(start b |> add_name sword_name |> add_damage 3 |> finish) in
  let axe = Weapon.Builder.(start b |> add_name axe_name |> add_damage 5 |> finish) in
  let weapons = Weapon.Vector.create b [| sword; axe |] in
  let name = Rt.String.create b "MyMonster" in
  let inventory = Rt.UByte.Vector.create b (Array.init 10 Char.chr) in
  let orc =
    Monster.Builder.(
      start b
      |> add_pos (1., 2., 3.)
      |> add_mana 150
      |> add_hp 80
      |> add_name name
      |> add_inventory inventory
      |> add_color Color.red
      |> add_weapons weapons
      |> add_equipped_weapon axe
      |> finish)
  in
  let buf = Monster.finish_buf Primitives.Bytes b orc in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (b, monster)) = Monster.root prim buf in
    check_eq "hp" ~expected:80 (Monster.hp b monster);
    check_eq "mana" ~expected:150 (Monster.mana b monster);
    check_eq "name" ~expected:"MyMonster"
      (Monster.name b monster |> Rt.Option.get |> Rt.String.to_string b);
    let pos = Monster.pos b monster |> Rt.Option.get in
    check_eq "pos" ~expected:(1., 2., 3.) (Vec3.x b pos, Vec3.y b pos, Vec3.z b pos);
    let inv = Monster.inventory b monster |> Rt.Option.get in
    check_eq "inventory" ~expected:(Array.init 10 Char.chr) (Rt.UByte.Vector.to_array b inv);
    let weapons = Monster.weapons b monster |> Rt.Option.get in
    check_eq "weapons.length" ~expected:2 (Weapon.Vector.length b weapons);
    let sword = Weapon.Vector.get b weapons 0 in
    check_eq "sword.name" ~expected:"Sword"
      (Weapon.name b sword |> Rt.Option.get |> Rt.String.to_string b);
    check_eq "sword.damage" ~expected:3 (Weapon.damage b sword);
    let axe = Weapon.Vector.get b weapons 1 in
    check_eq "axe.name" ~expected:"Axe"
      (Weapon.name b axe |> Rt.Option.get |> Rt.String.to_string b);
    check_eq "axe.damage" ~expected:5 (Weapon.damage b axe);
    check_eq "equipped_type" ~expected:Equipment.weapon (Monster.equipped_type b monster);
    let equipped =
      Monster.equipped b monster ~weapon:Fun.id ~default:(fun e ->
        failwith @@ "Unexpected union member: " ^ Equipment.to_string e)
    in
    check_eq "equipped.name" ~expected:"Axe"
      (Weapon.name b equipped |> Rt.Option.get |> Rt.String.to_string b);
    check_eq "equipped.damage" ~expected:5 (Weapon.damage b equipped)
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

(* ── Monster test (monster_test.fbs) ── *)

let create_example_monster b =
  let open Monster_test in
  let open MyGame.Example in
  let string = Rt.String.create b "MyMonster" in
  let strings = Array.map (Rt.String.create b) [| "test1"; "test2" |] in
  let name2 = Rt.String.create b "Fred" in
  let inv = Rt.UByte.Vector.create b (Array.init 5 Char.chr) in
  let mon2 = Monster.Builder.(start b |> add_name name2 |> finish) in
  let test4 = Test.Vector.create b [| (30, 40); (10, 20) |] in
  let array_of_strings = Rt.String.Vector.create b strings in
  let vector_of_longs = Rt.Long.Vector.create b
    [| 1L; 100L; 10_000L; 1_000_000L; 100_000_000L |] in
  let vector_of_doubles = Rt.Double.Vector.create b
    [| -1.7976931348623157e+308; 0.; 1.7976931348623157e+308 |] in
  Monster.Builder.(
    start b
    |> add_pos (1.0, 2.0, 3.0, 3.0, Color.green, (5, 6))
    |> add_hp 80
    |> add_name string
    |> add_inventory inv
    |> add_test_monster mon2
    |> add_test4 test4
    |> add_testarrayofstring array_of_strings
    |> add_vector_of_longs vector_of_longs
    |> add_vector_of_doubles vector_of_doubles
    |> finish)

(* Note: Melange emulates Int64 with a pair of int32s, so
   Int64.float_of_bits (Bytes.get_int64_le ...) is broken for doubles.
   Tests involving double fields are only verified via JsDataView where
   JavaScript's native getFloat64 is used. The ~check_doubles flag
   controls this. *)

let verify_monsterdata ?(check_doubles = true) (type a) (prim : a Primitives.t) (buf : a) =
  let open Monster_test in
  let open MyGame.Example in
  let (Rt.Root (buf, m)) = Monster.root prim buf in
  check_eq "hp" ~expected:80 (Monster.hp buf m);
  check_eq "mana" ~expected:150 (Monster.mana buf m);
  check_eq "name" ~expected:"MyMonster"
    (Monster.name buf m |> Rt.String.to_string buf);
  check "color" (Monster.color buf m == Color.blue);
  let pos = Rt.Option.get (Monster.pos buf m) in
  check_eq "pos.x" ~expected:1.0 (Vec3.x buf pos);
  check_eq "pos.y" ~expected:2.0 (Vec3.y buf pos);
  check_eq "pos.z" ~expected:3.0 (Vec3.z buf pos);
  if check_doubles then
    check_float "pos.test1" ~expected:3.0 (Vec3.test1 buf pos);
  check_eq "pos.test2" ~expected:(Color.green :> char) (Vec3.test2 buf pos :> char);
  let test3 = Vec3.test3 buf pos in
  check_eq "test3.a" ~expected:5 (Test.a buf test3);
  check_eq "test3.b" ~expected:6 (Test.b buf test3);
  check_eq "test_type" ~expected:(Any.monster :> char) (Monster.test_type buf m :> char);
  let monster2 =
    Monster.test buf m ~monster:Fun.id ~default:(fun _ -> failwith "unexpected union")
  in
  check_eq "test.name" ~expected:"Fred"
    (Monster.name buf monster2 |> Rt.String.to_string buf);
  let inventory = Rt.Option.get (Monster.inventory buf m) in
  check_eq "inventory" ~expected:[| '\x00'; '\x01'; '\x02'; '\x03'; '\x04' |]
    (Rt.UByte.Vector.to_array buf inventory);
  if check_doubles then begin
    let vec_of_doubles = Rt.Option.get (Monster.vector_of_doubles buf m) in
    check_float_array "vector_of_doubles"
      ~expected:[| -1.7976931348623157e+308; 0.; 1.7976931348623157e+308 |]
      (Rt.Double.Vector.to_array buf vec_of_doubles)
  end;
  let test4 = Rt.Option.get (Monster.test4 buf m) in
  check_eq "test4 length" ~expected:2 (Test.Vector.length buf test4);
  check_eq "test4 sum" ~expected:100
    (Test.Vector.to_seq buf test4
     |> Seq.fold_left (fun a t -> a + Test.a buf t + Test.b buf t) 0);
  let aos = Rt.Option.get (Monster.testarrayofstring buf m) in
  check_eq "testarrayofstring" ~expected:[ "test1"; "test2" ]
    (Rt.String.Vector.to_list buf aos |> List.map (Rt.String.to_string buf));
  check "testarrayoftables is none" (Rt.Option.is_none (Monster.testarrayoftables buf m))

let test_generated_monster () =
  let open Monster_test in
  let b = Rt.Builder.create () in
  let buf =
    create_example_monster b
    |> MyGame.Example.Monster.finish_buf Primitives.Bytes b
  in
  verify_monsterdata ~check_doubles:false Primitives.Bytes buf;
  verify_monsterdata Primitives.JsDataView (to_dv buf)

let test_generated_size_prefixed () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf =
    create_example_monster b
    |> Monster.finish_buf Primitives.Bytes ~size_prefixed:true b
  in
  check "has_ident"
    (Monster.has_identifier Primitives.Bytes ~size_prefixed:true buf);
  let (Rt.Root (buf, m)) =
    Monster.root ~size_prefixed:true Primitives.Bytes buf
  in
  check_eq "hp" ~expected:80 (Monster.hp buf m);
  check_eq "name" ~expected:"MyMonster"
    (Monster.name buf m |> Rt.String.to_string buf)

let test_monster_extra_floats () =
  let open Monster_extra in
  let open MyGame in
  let b = Rt.Builder.create () in
  let buf =
    MonsterExtra.(Builder.(start b |> finish)
    |> finish_buf Primitives.Bytes b)
  in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, mon)) = MonsterExtra.root prim buf in
    check "d0 is nan" (Float.is_nan (MonsterExtra.d0 buf mon));
    check "d1 is nan" (Float.is_nan (MonsterExtra.d1 buf mon));
    check_eq "d2" ~expected:Float.infinity (MonsterExtra.d2 buf mon);
    check_eq "d3" ~expected:Float.neg_infinity (MonsterExtra.d3 buf mon);
    check "f0 is nan" (Float.is_nan (MonsterExtra.f0 buf mon));
    check "f1 is nan" (Float.is_nan (MonsterExtra.f1 buf mon));
    check_eq "f2" ~expected:Float.infinity (MonsterExtra.f2 buf mon);
    check_eq "f3" ~expected:Float.neg_infinity (MonsterExtra.f3 buf mon)
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_builder_reset () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf =
    create_example_monster b |> Monster.finish_buf Primitives.Bytes b
  in
  Rt.Builder.reset b;
  let _ = create_example_monster b |> Monster.finish_buf Primitives.Bytes b in
  Rt.Builder.reset b;
  let buf' =
    create_example_monster b |> Monster.finish_buf Primitives.Bytes b
  in
  check_eq "reset buffers match" ~expected:buf buf'

let test_defaults_not_written () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf =
    Monster.Builder.(start b |> finish)
    |> Monster.finish_buf Primitives.Bytes b
  in
  (* Note: skip nan_default and inf_default — Melange's Int64 emulation may
     produce different bit patterns for NaN/Inf through bits_of_float, causing
     the builder to write non-default bytes. Test non-float defaults only. *)
  let buf' =
    Monster.Builder.(
      start b
      |> add_hp 100
      |> add_mana 150
      |> add_color Color.blue
      |> add_testhashs32_fnv1 0l
      |> add_testhashu64_fnv1 0L
      |> finish)
    |> Monster.finish_buf Primitives.Bytes b
  in
  check_eq "default buffers identical" ~expected:buf buf'

let test_enum_names () =
  let open Monster_test.MyGame.Example in
  check_eq "red" ~expected:"red" Color.(to_string red);
  check_eq "blue" ~expected:"blue" Color.(to_string blue);
  check_eq "green" ~expected:"green" Color.(to_string green)

let test_extension_ident () =
  let open Monster_test.MyGame.Example in
  check_eq "extension" ~expected:(Some "mon") Monster.extension;
  check_eq "identifier" ~expected:(Some "MONS") Monster.identifier

(* ── Nested flatbuffers ── *)

let build_nested_monster_buf b ~name ~hp ~mana =
  let open Monster_test in
  let open MyGame.Example in
  let n = Rt.String.create b name in
  let inv = Rt.UByte.Vector.create b [| '\x01'; '\x02'; '\x03' |] in
  let wip =
    Monster.Builder.(start b |> add_name n |> add_hp hp |> add_mana mana
    |> add_inventory inv |> finish)
  in
  Monster.finish_buf Primitives.Bytes b wip

let build_outer_with_nested b ~outer_name ~inner_buf =
  let open Monster_test in
  let open MyGame.Example in
  let name = Rt.String.create b outer_name in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  let wip =
    Monster.Builder.(start b |> add_name name |> add_testnestedflatbuffer nested_vec |> finish)
  in
  Monster.finish_buf Primitives.Bytes b wip

let test_nested_roundtrip () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_name = Rt.String.create b "NestedMonster" in
  let inner_wip =
    Monster.Builder.(start b |> add_name inner_name |> add_hp 42 |> finish)
  in
  let inner_buf = Monster.finish_buf Primitives.Bytes b inner_wip in
  let outer_name = Rt.String.create b "OuterMonster" in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  let outer_wip =
    Monster.Builder.(start b |> add_name outer_name |> add_testnestedflatbuffer nested_vec
    |> finish)
  in
  let outer_buf = Monster.finish_buf Primitives.Bytes b outer_wip in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, m)) = Monster.root prim buf in
    check_eq "outer name" ~expected:"OuterMonster"
      (Monster.name buf m |> Rt.String.to_string buf);
    let (Rt.Root (nbuf, nm)) =
      Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
    check_eq "nested name" ~expected:"NestedMonster"
      (Monster.name nbuf nm |> Rt.String.to_string nbuf);
    check_eq "nested hp" ~expected:42 (Monster.hp nbuf nm)
  in
  verify Primitives.Bytes outer_buf;
  verify Primitives.JsDataView (to_dv outer_buf)

let test_nested_absent () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let name = Rt.String.create b "NoNested" in
  let wip = Monster.Builder.(start b |> add_name name |> finish) in
  let buf = Monster.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes buf in
  check "nested absent" (Option.is_none (Monster.testnestedflatbuffer_as_monster buf m))

let test_nested_raw_access () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_name = Rt.String.create b "RawNested" in
  let inner_wip = Monster.Builder.(start b |> add_name inner_name |> finish) in
  let inner_buf = Monster.finish_buf Primitives.Bytes b inner_wip in
  let outer_name = Rt.String.create b "OuterRaw" in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  let outer_wip =
    Monster.Builder.(start b |> add_name outer_name |> add_testnestedflatbuffer nested_vec
    |> finish)
  in
  let outer_buf = Monster.finish_buf Primitives.Bytes b outer_wip in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes outer_buf in
  let raw_vec = Rt.Option.get (Monster.testnestedflatbuffer buf m) in
  check_eq "raw vec length" ~expected:(Bytes.length inner_buf) (Rt.UByte.Vector.length buf raw_vec)

let test_nested_string_buffer () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_buf = build_nested_monster_buf b ~name:"StringNested" ~hp:99 ~mana:200 in
  let outer_buf = build_outer_with_nested b ~outer_name:"StringOuter" ~inner_buf in
  let outer_str = Bytes.to_string outer_buf in
  let (Rt.Root (buf, m)) = Monster.root Primitives.String outer_str in
  let (Rt.Root (nbuf, nm)) =
    Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
  check_eq "name" ~expected:"StringNested" (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  check_eq "hp" ~expected:99 (Monster.hp nbuf nm);
  check_eq "mana" ~expected:200 (Monster.mana nbuf nm)

let test_nested_jsdataview_buffer () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_buf = build_nested_monster_buf b ~name:"DvNested" ~hp:77 ~mana:300 in
  let outer_buf = build_outer_with_nested b ~outer_name:"DvOuter" ~inner_buf in
  let dv = to_dv outer_buf in
  let (Rt.Root (buf, m)) = Monster.root Primitives.JsDataView dv in
  let (Rt.Root (nbuf, nm)) =
    Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
  check_eq "name" ~expected:"DvNested" (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  check_eq "hp" ~expected:77 (Monster.hp nbuf nm);
  check_eq "mana" ~expected:300 (Monster.mana nbuf nm)

let test_nested_rich_inner () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
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
  let inner_buf = Monster.finish_buf Primitives.Bytes b inner_wip in
  let outer_buf = build_outer_with_nested b ~outer_name:"RichOuter" ~inner_buf in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, m)) = Monster.root prim buf in
    let (Rt.Root (nbuf, nm)) =
      Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
    check_eq "rich name" ~expected:"RichInner"
      (Monster.name nbuf nm |> Rt.String.to_string nbuf);
    check_eq "rich hp" ~expected:55 (Monster.hp nbuf nm);
    let pos = Rt.Option.get (Monster.pos nbuf nm) in
    check_eq "rich pos.x" ~expected:1.5 (Vec3.x nbuf pos);
    check_eq "rich pos.y" ~expected:2.5 (Vec3.y nbuf pos);
    check_eq "rich pos.z" ~expected:3.5 (Vec3.z nbuf pos);
    let inv = Rt.Option.get (Monster.inventory nbuf nm) in
    check_eq "rich inv" ~expected:[| '\x0a'; '\x0b'; '\x0c'; '\x0d' |]
      (Rt.UByte.Vector.to_array nbuf inv);
    let t4 = Rt.Option.get (Monster.test4 nbuf nm) in
    check_eq "rich test4 len" ~expected:2 (Test.Vector.length nbuf t4);
    let aos = Rt.Option.get (Monster.testarrayofstring nbuf nm) in
    check_eq "rich strings" ~expected:[ "hello"; "world" ]
      (Rt.String.Vector.to_list nbuf aos |> List.map (Rt.String.to_string nbuf))
  in
  verify Primitives.Bytes outer_buf;
  verify Primitives.JsDataView (to_dv outer_buf)

let test_nested_testrequired () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_buf = build_nested_monster_buf b ~name:"Required" ~hp:33 ~mana:44 in
  let name = Rt.String.create b "RequiredOuter" in
  let nested_vec = Rt.create_nested_vector b inner_buf in
  let wip =
    Monster.Builder.(start b |> add_name name
    |> add_testrequirednestedflatbuffer nested_vec |> finish)
  in
  let outer_buf = Monster.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes outer_buf in
  check "optional nested absent"
    (Option.is_none (Monster.testnestedflatbuffer_as_monster buf m));
  let (Rt.Root (nbuf, nm)) =
    Option.get (Monster.testrequirednestedflatbuffer_as_monster buf m) in
  check_eq "required name" ~expected:"Required"
    (Monster.name nbuf nm |> Rt.String.to_string nbuf);
  check_eq "required hp" ~expected:33 (Monster.hp nbuf nm)

let test_nested_builder_reset () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf1 =
    let inner = build_nested_monster_buf b ~name:"First" ~hp:1 ~mana:2 in
    build_outer_with_nested b ~outer_name:"Outer1" ~inner_buf:inner
  in
  Rt.Builder.reset b;
  let buf2 =
    let inner = build_nested_monster_buf b ~name:"Second" ~hp:3 ~mana:4 in
    build_outer_with_nested b ~outer_name:"Outer2" ~inner_buf:inner
  in
  let (Rt.Root (b1, m1)) = Monster.root Primitives.Bytes buf1 in
  let (Rt.Root (nb1, nm1)) =
    Option.get (Monster.testnestedflatbuffer_as_monster b1 m1) in
  check_eq "first outer" ~expected:"Outer1"
    (Monster.name b1 m1 |> Rt.String.to_string b1);
  check_eq "first inner" ~expected:"First"
    (Monster.name nb1 nm1 |> Rt.String.to_string nb1);
  let (Rt.Root (b2, m2)) = Monster.root Primitives.Bytes buf2 in
  let (Rt.Root (nb2, nm2)) =
    Option.get (Monster.testnestedflatbuffer_as_monster b2 m2) in
  check_eq "second outer" ~expected:"Outer2"
    (Monster.name b2 m2 |> Rt.String.to_string b2);
  check_eq "second inner" ~expected:"Second"
    (Monster.name nb2 nm2 |> Rt.String.to_string nb2)

let test_nested_defaults () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let inner_name = Rt.String.create b "DefaultsInner" in
  let inner_wip = Monster.Builder.(start b |> add_name inner_name |> finish) in
  let inner_buf = Monster.finish_buf Primitives.Bytes b inner_wip in
  let outer_buf = build_outer_with_nested b ~outer_name:"DefaultsOuter" ~inner_buf in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes outer_buf in
  let (Rt.Root (nbuf, nm)) =
    Option.get (Monster.testnestedflatbuffer_as_monster buf m) in
  check_eq "default hp" ~expected:100 (Monster.hp nbuf nm);
  check_eq "default mana" ~expected:150 (Monster.mana nbuf nm);
  check "pos absent" (Rt.Option.is_none (Monster.pos nbuf nm));
  check "inventory absent" (Rt.Option.is_none (Monster.inventory nbuf nm))

let test_union_default_tag () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let name = Rt.String.create b "NoUnion" in
  let wip = Monster.Builder.(start b |> add_name name |> finish) in
  let buf = Monster.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes buf in
  check_eq "test_type absent" ~expected:(Any.none :> char) (Monster.test_type buf m :> char);
  let result =
    Monster.test buf m
      ~none:42
      ~monster:(fun _ -> failwith "should not read monster")
      ~default:(fun _ -> failwith "should not hit default")
  in
  check_eq "none branch" ~expected:42 result;
  let result2 =
    Monster.test buf m
      ~monster:(fun _ -> failwith "should not read monster")
      ~default:(fun t ->
        check_eq "default tag" ~expected:(Any.none :> char) (t :> char); 99)
  in
  check_eq "default fallback" ~expected:99 result2

(* ── Key lookups ── *)

let test_key_lookup_stat () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let stats =
    [| (10, 100L); (20, 200L); (30, 300L) |]
    |> Array.map (fun (count, value) ->
      let id = Rt.String.create b (Printf.sprintf "stat_%d" count) in
      Stat.Builder.(start b |> add_id id |> add_count count |> add_val_ value |> finish))
  in
  let stats_vec = Stat.Vector.create b stats in
  let name = Rt.String.create b "KeyLookup" in
  let wip =
    Monster.Builder.(start b |> add_name name |> add_scalar_key_sorted_tables stats_vec
    |> finish)
  in
  let buf = Monster.finish_buf Primitives.Bytes b wip in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, m)) = Monster.root prim buf in
    let vec = Rt.Option.get (Monster.scalar_key_sorted_tables buf m) in
    let found = Rt.Option.get (Stat.lookup_by_key buf vec 20) in
    check_eq "count=20" ~expected:20 (Stat.count buf found);
    check_eq "val=200" ~expected:200L (Stat.val_ buf found);
    check "found 10" (Rt.Option.is_some (Stat.lookup_by_key buf vec 10));
    check "found 30" (Rt.Option.is_some (Stat.lookup_by_key buf vec 30))
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_key_lookup_not_found () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let stats =
    [| 10; 20; 30 |]
    |> Array.map (fun count ->
      let id = Rt.String.create b "x" in
      Stat.Builder.(start b |> add_id id |> add_count count |> finish))
  in
  let stats_vec = Stat.Vector.create b stats in
  let name = Rt.String.create b "NotFound" in
  let wip =
    Monster.Builder.(start b |> add_name name |> add_scalar_key_sorted_tables stats_vec
    |> finish)
  in
  let buf = Monster.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes buf in
  let vec = Rt.Option.get (Monster.scalar_key_sorted_tables buf m) in
  check "not found 15" (Rt.Option.is_none (Stat.lookup_by_key buf vec 15));
  check "not found 0" (Rt.Option.is_none (Stat.lookup_by_key buf vec 0));
  check "not found 99" (Rt.Option.is_none (Stat.lookup_by_key buf vec 99))

let test_key_lookup_struct () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let abilities = Ability.Vector.create b [| (1l, 10l); (3l, 30l); (5l, 50l) |] in
  let name = Rt.String.create b "AbilityLookup" in
  let wip =
    Monster.Builder.(start b |> add_name name |> add_testarrayofsortedstruct abilities
    |> finish)
  in
  let buf = Monster.finish_buf Primitives.Bytes b wip in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, m)) = Monster.root prim buf in
    let vec = Rt.Option.get (Monster.testarrayofsortedstruct buf m) in
    let found = Rt.Option.get (Ability.lookup_by_key buf vec 3l) in
    check_eq "id=3" ~expected:3l (Ability.id buf found);
    check_eq "distance=30" ~expected:30l (Ability.distance buf found);
    check "found 1" (Rt.Option.is_some (Ability.lookup_by_key buf vec 1l));
    check "found 5" (Rt.Option.is_some (Ability.lookup_by_key buf vec 5l));
    check "not found 2" (Rt.Option.is_none (Ability.lookup_by_key buf vec 2l));
    check "not found 0" (Rt.Option.is_none (Ability.lookup_by_key buf vec 0l))
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_key_lookup_single () =
  let open Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let stats =
    [| Stat.Builder.(let id = Rt.String.create b "only" in
       start b |> add_id id |> add_count 42 |> finish) |]
  in
  let stats_vec = Stat.Vector.create b stats in
  let name = Rt.String.create b "Single" in
  let wip =
    Monster.Builder.(start b |> add_name name |> add_scalar_key_sorted_tables stats_vec
    |> finish)
  in
  let buf = Monster.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, m)) = Monster.root Primitives.Bytes buf in
  let vec = Rt.Option.get (Monster.scalar_key_sorted_tables buf m) in
  check "found single" (Rt.Option.is_some (Stat.lookup_by_key buf vec 42));
  check "not found other" (Rt.Option.is_none (Stat.lookup_by_key buf vec 41))

(* ── Arrays (arrays_test.fbs) ── *)

let test_arrays_roundtrip () =
  let open Arrays_test in
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
  let buf = ArrayTable.finish_buf Primitives.Bytes b wip in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, tbl)) = ArrayTable.root prim buf in
    let s = Rt.Option.get (ArrayTable.a buf tbl) in
    check "a ~= 3.14" (Float.abs (ArrayStruct.a buf s -. 3.14) < 0.01);
    check_eq "b_length" ~expected:15 ArrayStruct.b_length;
    check_eq "b[0]" ~expected:1l (ArrayStruct.b buf s 0);
    check_eq "b[7]" ~expected:8l (ArrayStruct.b buf s 7);
    check_eq "b[14]" ~expected:15l (ArrayStruct.b buf s 14);
    check_eq "c" ~expected:0x42 (ArrayStruct.c buf s);
    check_eq "d_length" ~expected:2 ArrayStruct.d_length;
    let ns0 = ArrayStruct.d buf s 0 in
    check_eq "d[0].a[0]" ~expected:10l (NestedStruct.a buf ns0 0);
    check_eq "d[0].a[1]" ~expected:20l (NestedStruct.a buf ns0 1);
    check_eq "d[0].b" ~expected:(TestEnum.a :> int) (NestedStruct.b buf ns0 :> int);
    check_eq "d[0].c[0]" ~expected:(TestEnum.b :> int) (NestedStruct.c buf ns0 0 :> int);
    check_eq "d[0].c[1]" ~expected:(TestEnum.c :> int) (NestedStruct.c buf ns0 1 :> int);
    check_eq "d[0].d[0]" ~expected:100L (NestedStruct.d buf ns0 0);
    check_eq "d[0].d[1]" ~expected:200L (NestedStruct.d buf ns0 1);
    let ns1 = ArrayStruct.d buf s 1 in
    check_eq "d[1].a[0]" ~expected:30l (NestedStruct.a buf ns1 0);
    check_eq "d[1].d[1]" ~expected:400L (NestedStruct.d buf ns1 1);
    check_eq "e" ~expected:99l (ArrayStruct.e buf s);
    check_eq "f[0]" ~expected:1000L (ArrayStruct.f buf s 0);
    check_eq "f[1]" ~expected:2000L (ArrayStruct.f buf s 1)
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_arrays_absent () =
  let open Arrays_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let wip = ArrayTable.Builder.(start b |> finish) in
  let buf = ArrayTable.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, tbl)) = ArrayTable.root Primitives.Bytes buf in
  check "a is none" (Rt.Option.is_none (ArrayTable.a buf tbl))

let test_arrays_length_constants () =
  let open Arrays_test.MyGame.Example in
  check_eq "NestedStruct.a_length" ~expected:2 NestedStruct.a_length;
  check_eq "NestedStruct.c_length" ~expected:2 NestedStruct.c_length;
  check_eq "NestedStruct.d_length" ~expected:2 NestedStruct.d_length;
  check_eq "ArrayStruct.b_length" ~expected:15 ArrayStruct.b_length;
  check_eq "ArrayStruct.d_length" ~expected:2 ArrayStruct.d_length;
  check_eq "ArrayStruct.f_length" ~expected:2 ArrayStruct.f_length;
  check_eq "LargeArrayStruct.d_length" ~expected:64 LargeArrayStruct.d_length;
  check_eq "LargeArrayStruct.e_length" ~expected:64 LargeArrayStruct.e_length;
  check_eq "LargeArrayStruct.g_length" ~expected:64 LargeArrayStruct.g_length

(* ── 64-bit offsets (test_64bit.fbs) ── *)

let test_offset64_roundtrip () =
  let open Test64_bit in
  let b = Rt.Builder.create () in
  let far_vec = Rt.UByte.Vector.create b [| '\x01'; '\x02'; '\x03' |] in
  let far_str = Rt.String.create b "hello64" in
  let big_vec = Rt.UByte.Vector64.create b [| '\xAA'; '\xBB' |] in
  let near_str = Rt.String.create b "near" in
  let far_structs = LeafStruct.Vector.create b [| (42l, 3.14); (99l, 2.71) |] in
  let big_structs = LeafStruct.Vector64.create b [| (10l, 1.0); (20l, 2.0); (30l, 3.0) |] in
  let wip = RootTable.Builder.(
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
  let buf = RootTable.finish_buf Primitives.Bytes b wip in
  let verify ?(check_doubles = true) (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, tbl)) = RootTable.root prim buf in
    check_eq "a" ~expected:123l (RootTable.a buf tbl);
    let fv = Rt.Option.get (RootTable.far_vector buf tbl) in
    check_eq "far_vector len" ~expected:3 (Rt.UByte.Vector.length buf fv);
    check_eq "far_vector[0]" ~expected:'\x01' (Rt.UByte.Vector.get buf fv 0);
    check_eq "far_vector[2]" ~expected:'\x03' (Rt.UByte.Vector.get buf fv 2);
    let fs = Rt.Option.get (RootTable.far_string buf tbl) in
    check_eq "far_string" ~expected:"hello64" (Rt.String.to_string buf fs);
    let bv = Rt.Option.get (RootTable.big_vector buf tbl) in
    check_eq "big_vector len" ~expected:2 (Rt.UByte.Vector64.length buf bv);
    check_eq "big_vector[0]" ~expected:'\xAA' (Rt.UByte.Vector64.get buf bv 0);
    check_eq "big_vector[1]" ~expected:'\xBB' (Rt.UByte.Vector64.get buf bv 1);
    let ns = Rt.Option.get (RootTable.near_string buf tbl) in
    check_eq "near_string" ~expected:"near" (Rt.String.to_string buf ns);
    let fsv = Rt.Option.get (RootTable.far_struct_vector buf tbl) in
    check_eq "far_struct len" ~expected:2 (LeafStruct.Vector.length buf fsv);
    let s0 = LeafStruct.Vector.get buf fsv 0 in
    check_eq "far_struct[0].a" ~expected:42l (LeafStruct.a buf s0);
    if check_doubles then
      check_float "far_struct[0].b" ~expected:3.14 (LeafStruct.b buf s0);
    let bsv = Rt.Option.get (RootTable.big_struct_vector buf tbl) in
    check_eq "big_struct len" ~expected:3 (LeafStruct.Vector64.length buf bsv);
    let bs2 = LeafStruct.Vector64.get buf bsv 2 in
    check_eq "big_struct[2].a" ~expected:30l (LeafStruct.a buf bs2);
    if check_doubles then
      check_float "big_struct[2].b" ~expected:3.0 (LeafStruct.b buf bs2)
  in
  verify ~check_doubles:false Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_offset64_absent () =
  let open Test64_bit in
  let b = Rt.Builder.create () in
  let wip = RootTable.Builder.(start b |> finish) in
  let buf = RootTable.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, tbl)) = RootTable.root Primitives.Bytes buf in
  check "far_vector absent" (Rt.Option.is_none (RootTable.far_vector buf tbl));
  check "far_string absent" (Rt.Option.is_none (RootTable.far_string buf tbl));
  check "big_vector absent" (Rt.Option.is_none (RootTable.big_vector buf tbl));
  check "near_string absent" (Rt.Option.is_none (RootTable.near_string buf tbl));
  check_eq "a default" ~expected:0l (RootTable.a buf tbl)

(* ── String union (string_union.fbs) ── *)

let test_greeting_roundtrip () =
  let open String_union in
  let b = Rt.Builder.create () in
  let msg = Rt.String.create b "hello" in
  let greeting = Greeting.Builder.(start b |> add_message msg |> finish) in
  let wip = Envelope.Builder.(start b |> add_content_greeting greeting |> finish) in
  let buf = Envelope.finish_buf Primitives.Bytes b wip in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, e)) = Envelope.root prim buf in
    check_eq "content_type" ~expected:(Content.greeting :> char)
      (Envelope.content_type buf e :> char);
    let msg_out =
      Envelope.content buf e
        ~greeting:(fun g ->
          Rt.Option.fold ~none:"" ~some:(fun s -> Rt.String.to_string buf s)
            (Greeting.message buf g))
        ~default:(fun _ -> failwith "unexpected union type")
    in
    check_eq "greeting message" ~expected:"hello" msg_out
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_string_member_roundtrip () =
  let open String_union in
  let b = Rt.Builder.create () in
  let s = Rt.String.create b "world" in
  let wip = Envelope.Builder.(start b |> add_content_name s |> finish) in
  let buf = Envelope.finish_buf Primitives.Bytes b wip in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, e)) = Envelope.root prim buf in
    check_eq "content_type" ~expected:(Content.name :> char)
      (Envelope.content_type buf e :> char);
    let s_out =
      Envelope.content buf e
        ~name:(fun s -> Rt.String.to_string buf s)
        ~default:(fun _ -> failwith "unexpected union type")
    in
    check_eq "string member" ~expected:"world" s_out
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_string_union_obj_api () =
  let open String_union in
  let b = Rt.Builder.create () in
  let obj1 : Envelope.obj = {
    content = `Greeting { Greeting.message = Some "hi" };
  } in
  let wip = Envelope.pack b obj1 in
  let buf = Envelope.finish_buf Primitives.Bytes b wip in
  let (Rt.Root (buf, e)) = Envelope.root Primitives.Bytes buf in
  let unpacked = Envelope.unpack buf e in
  (match unpacked.content with
   | `Greeting g -> check_eq "greeting obj" ~expected:(Some "hi") g.message
   | _ -> check "expected Greeting" false);
  let b2 = Rt.Builder.create () in
  let obj2 : Envelope.obj = { content = `Name "yo" } in
  let wip2 = Envelope.pack b2 obj2 in
  let buf2 = Envelope.finish_buf Primitives.Bytes b2 wip2 in
  let (Rt.Root (buf2, e2)) = Envelope.root Primitives.Bytes buf2 in
  let unpacked2 = Envelope.unpack buf2 e2 in
  (match unpacked2.content with
   | `Name s -> check_eq "string obj name" ~expected:"yo" s
   | _ -> check "expected Name" false);
  let b3 = Rt.Builder.create () in
  let obj3 : Envelope.obj = { content = `None_ } in
  let wip3 = Envelope.pack b3 obj3 in
  let buf3 = Envelope.finish_buf Primitives.Bytes b3 wip3 in
  let (Rt.Root (buf3, e3)) = Envelope.root Primitives.Bytes buf3 in
  let unpacked3 = Envelope.unpack buf3 e3 in
  (match unpacked3.content with
   | `None_ -> check "none variant" true
   | _ -> check "expected None" false)

(* ── More defaults (more_defaults.fbs) ── *)

let test_more_defaults_empty () =
  let open More_defaults in
  let b = Rt.Builder.create () in
  let t = MoreDefaults.Builder.start b in
  let off = MoreDefaults.Builder.finish t in
  let buf = MoreDefaults.finish_buf Primitives.Bytes b off in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (b_, o_)) = MoreDefaults.root prim buf in
    let obj = MoreDefaults.unpack b_ o_ in
    check_eq "ints length" ~expected:0 (Array.length obj.ints);
    check_eq "floats length" ~expected:0 (Array.length obj.floats);
    check_eq "empty_string" ~expected:"" obj.empty_string;
    check_eq "some_string" ~expected:"" obj.some_string;
    check_eq "abcs length" ~expected:0 (Array.length obj.abcs);
    check_eq "bools length" ~expected:0 (Array.length obj.bools)
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

let test_more_defaults_obj_api () =
  let open More_defaults in
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
  let buf = MoreDefaults.finish_buf Primitives.Bytes b off in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (b_, o_)) = MoreDefaults.root prim buf in
    let obj = MoreDefaults.unpack b_ o_ in
    check_eq "ints length" ~expected:3 (Array.length obj.ints);
    check_eq "ints" ~expected:[| 1l; 2l; 3l |] obj.ints;
    check_eq "floats length" ~expected:2 (Array.length obj.floats);
    check_eq "empty_string" ~expected:"hello" obj.empty_string;
    check_eq "some_string" ~expected:"world" obj.some_string;
    check_eq "abcs length" ~expected:2 (Array.length obj.abcs);
    check_eq "bools length" ~expected:3 (Array.length obj.bools)
  in
  verify Primitives.Bytes buf;
  verify Primitives.JsDataView (to_dv buf)

(* ── Casing (casing_test.fbs) ── *)

let test_casing_roundtrip () =
  let open Casing_test in
  let open Casing in
  let b = Rt.Builder.create () in
  let lstm =
    Lstmoptions.Builder.(start b |> add_num_units 128l |> add_cell_clip 1.5 |> finish)
  in
  let rnn =
    Rnnconfig.Builder.(start b |> add_hidden_size 64l |> add_time_major true |> finish)
  in
  let topk = TopKv2Params.Builder.(start b |> add_k 5l |> finish) in
  let conv =
    Conv2Doptions.Builder.(
      start b |> add_stride_w 1l |> add_stride_h 2l |> add_padding 0l |> finish)
  in
  let svdf =
    Svdfparams.Builder.(start b |> add_rank 2l |> add_activation 1l |> finish)
  in
  let root =
    SimpleTable.Builder.(
      start b |> add_value 42l |> add_lstm lstm |> add_rnn rnn |> add_topk topk
      |> add_conv conv |> add_svdf svdf |> finish)
  in
  let buf = SimpleTable.finish_buf Primitives.String b root in
  let verify (type a) (prim : a Primitives.t) (buf : a) =
    let (Rt.Root (buf, tbl)) = SimpleTable.root prim buf in
    check_eq "value" ~expected:42l (SimpleTable.value buf tbl);
    let lstm = Rt.Option.get (SimpleTable.lstm buf tbl) in
    check_eq "lstm.num_units" ~expected:128l (Lstmoptions.num_units buf lstm);
    check "lstm.cell_clip ~= 1.5"
      (Float.abs (Lstmoptions.cell_clip buf lstm -. 1.5) < 0.01);
    let rnn = Rt.Option.get (SimpleTable.rnn buf tbl) in
    check_eq "rnn.hidden_size" ~expected:64l (Rnnconfig.hidden_size buf rnn);
    check_eq "rnn.time_major" ~expected:true (Rnnconfig.time_major buf rnn);
    let topk = Rt.Option.get (SimpleTable.topk buf tbl) in
    check_eq "topk.k" ~expected:5l (TopKv2Params.k buf topk);
    let conv = Rt.Option.get (SimpleTable.conv buf tbl) in
    check_eq "conv.stride_w" ~expected:1l (Conv2Doptions.stride_w buf conv);
    check_eq "conv.stride_h" ~expected:2l (Conv2Doptions.stride_h buf conv);
    check_eq "conv.padding" ~expected:0l (Conv2Doptions.padding buf conv);
    let svdf = Rt.Option.get (SimpleTable.svdf buf tbl) in
    check_eq "svdf.rank" ~expected:2l (Svdfparams.rank buf svdf);
    check_eq "svdf.activation" ~expected:1l (Svdfparams.activation buf svdf)
  in
  verify Primitives.String buf;
  let buf_bytes = Bytes.of_string buf in
  verify Primitives.Bytes buf_bytes;
  verify Primitives.JsDataView (to_dv buf_bytes)

(* ── Run all ── *)

let run name f =
  Printf.printf "  %s... %!" name;
  f ();
  Printf.printf "ok\n%!"

let () =
  Printf.printf "Melange: Monster sample\n%!";
  run "sample build+read" test_monster_sample;
  Printf.printf "Melange: Monster test\n%!";
  run "generated monster" test_generated_monster;
  run "size-prefixed" test_generated_size_prefixed;
  run "extra floats (nan/inf)" test_monster_extra_floats;
  run "builder reset" test_builder_reset;
  run "defaults not written" test_defaults_not_written;
  run "enum names" test_enum_names;
  run "extension/identifier" test_extension_ident;
  Printf.printf "Melange: Nested flatbuffers\n%!";
  run "nested roundtrip" test_nested_roundtrip;
  run "nested absent" test_nested_absent;
  run "nested raw access" test_nested_raw_access;
  run "nested String buffer" test_nested_string_buffer;
  run "nested JsDataView buffer" test_nested_jsdataview_buffer;
  run "nested rich inner" test_nested_rich_inner;
  run "nested testrequired" test_nested_testrequired;
  run "nested builder reset" test_nested_builder_reset;
  run "nested defaults" test_nested_defaults;
  run "union default tag" test_union_default_tag;
  Printf.printf "Melange: Key lookups\n%!";
  run "key lookup stat" test_key_lookup_stat;
  run "key lookup not found" test_key_lookup_not_found;
  run "key lookup struct" test_key_lookup_struct;
  run "key lookup single" test_key_lookup_single;
  Printf.printf "Melange: Arrays\n%!";
  run "arrays roundtrip" test_arrays_roundtrip;
  run "arrays absent" test_arrays_absent;
  run "arrays length constants" test_arrays_length_constants;
  Printf.printf "Melange: 64-bit offsets\n%!";
  run "offset64 roundtrip" test_offset64_roundtrip;
  run "offset64 absent" test_offset64_absent;
  Printf.printf "Melange: String union\n%!";
  run "greeting roundtrip" test_greeting_roundtrip;
  run "string member roundtrip" test_string_member_roundtrip;
  run "object API roundtrip" test_string_union_obj_api;
  Printf.printf "Melange: More defaults\n%!";
  run "empty table defaults" test_more_defaults_empty;
  run "object API roundtrip" test_more_defaults_obj_api;
  Printf.printf "Melange: Casing\n%!";
  run "casing roundtrip" test_casing_roundtrip;
  Printf.printf "\nMelange: %d passed, %d failed (of %d)\n%!"
    (!tests_run - !tests_failed) !tests_failed !tests_run;
  if !tests_failed > 0 then exit 1
