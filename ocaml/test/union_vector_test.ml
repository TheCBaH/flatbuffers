module P = Flatbuffers.Primitives
module B = Flatbuffers.Runtime.Builder
open Fixtures.Union_vector
open UnionVector

let check_ok name = function
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %s" name (Flatbuffers.Verifier.error_to_string e)
;;

let u32 buf i = Int32.to_int (Bytes.get_int32_le buf i)

let root_field buf voff =
  let root = u32 buf 0 in
  let vtable = root - Int32.to_int (Bytes.get_int32_le buf root) in
  let vtable_size = Bytes.get_uint16_le buf vtable in
  if voff >= vtable_size
  then -1
  else (
    let offset = Bytes.get_uint16_le buf (vtable + voff) in
    if offset = 0 then -1 else root + offset)
;;

let render_item buf inventory i =
  Inventory.items
    ~none:"none"
    ~sword:(fun sword -> Printf.sprintf "sword:%ld" (Sword.damage buf sword))
    ~spell:(fun spell ->
      let name = Spell.name buf spell |> Rt.Option.get |> Rt.String.to_string buf in
      "spell:" ^ name)
    ~label:(fun label -> "label:" ^ Rt.String.to_string buf label)
    ~default:(fun tag -> "unknown:" ^ Item.to_string tag)
    buf
    inventory
    i
;;

let create_inventory () =
  let b = Rt.Builder.create () in
  let sword = Sword.Builder.(start b |> add_damage 7l |> finish) in
  let spell_name = Rt.String.create b "fire" in
  let spell = Spell.Builder.(start b |> add_name spell_name |> finish) in
  let label = Rt.String.create b "potion" in
  let items =
    Inventory.Builder.create_items
      b
      [| `None_; `Sword sword; `Spell spell; `Label label |]
  in
  let required_items = Inventory.Builder.create_required_items b [||] in
  let inventory =
    Inventory.Builder.(
      start b |> add_items items |> add_required_items required_items |> finish)
  in
  Inventory.finish_buf P.Bytes b inventory
;;

let check_zero_copy_roundtrip () =
  let bytes = create_inventory () in
  check_ok "generated verifier" (Inventory.verify P.Bytes bytes);
  let (Rt.Root (buf, inventory)) =
    match Inventory.root_verified P.Bytes bytes with
    | Ok root -> root
    | Error e -> Alcotest.fail (Flatbuffers.Verifier.error_to_string e)
  in
  Alcotest.(check int) "length" 4 (Inventory.items_length buf inventory);
  let expected = [| "none"; "sword:7"; "spell:fire"; "label:potion" |] in
  Alcotest.(check (array string))
    "indexed"
    expected
    (Array.init 4 (render_item buf inventory));
  Alcotest.(check (list string))
    "list"
    (Array.to_list expected)
    (Inventory.items_to_list
       ~none:"none"
       ~sword:(fun sword -> Printf.sprintf "sword:%ld" (Sword.damage buf sword))
       ~spell:(fun spell ->
         let name = Spell.name buf spell |> Rt.Option.get |> Rt.String.to_string buf in
         "spell:" ^ name)
       ~label:(fun label -> "label:" ^ Rt.String.to_string buf label)
       ~default:(fun tag -> "unknown:" ^ Item.to_string tag)
       buf
       inventory);
  Alcotest.(check (array string))
    "array"
    expected
    (Inventory.items_to_array
       ~none:"none"
       ~sword:(fun sword -> Printf.sprintf "sword:%ld" (Sword.damage buf sword))
       ~spell:(fun spell ->
         let name = Spell.name buf spell |> Rt.Option.get |> Rt.String.to_string buf in
         "spell:" ^ name)
       ~label:(fun label -> "label:" ^ Rt.String.to_string buf label)
       ~default:(fun tag -> "unknown:" ^ Item.to_string tag)
       buf
       inventory);
  let iterated = ref [] in
  Inventory.items_iter
    ~none:"none"
    ~sword:(fun sword -> Printf.sprintf "sword:%ld" (Sword.damage buf sword))
    ~spell:(fun _ -> "spell")
    ~label:(fun _ -> "label")
    ~default:(fun _ -> "unknown")
    buf
    inventory
    (fun item -> iterated := item :: !iterated);
  Alcotest.(check (list string))
    "iteration order"
    [ "none"; "sword:7"; "spell"; "label" ]
    (List.rev !iterated)
;;

let check_absent_optional_and_required () =
  let b = Rt.Builder.create () in
  let required_items = Inventory.Builder.create_required_items b [||] in
  let root =
    Inventory.Builder.(start b |> add_required_items required_items |> finish)
    |> Inventory.finish_buf P.Bytes b
  in
  check_ok "empty required vector" (Inventory.verify P.Bytes root);
  let (Rt.Root (buf, inventory)) = Inventory.root P.Bytes root in
  Alcotest.(check int) "absent length" 0 (Inventory.items_length buf inventory);
  Alcotest.(check int)
    "required empty length"
    0
    (Inventory.required_items_length buf inventory);
  Alcotest.(check (list string))
    "absent list"
    []
    (Inventory.items_to_list ~default:(fun _ -> "unknown") buf inventory);
  let missing =
    let b = Rt.Builder.create () in
    Inventory.Builder.(start b |> finish) |> Inventory.finish_buf P.Bytes b
  in
  match Inventory.verify P.Bytes missing with
  | Ok () -> Alcotest.fail "accepted a missing required union vector"
  | Error _ -> ()
;;

let check_object_api_roundtrip () =
  let original : Inventory.obj =
    { items =
        [| `None_
         ; `Sword { Sword.damage = 42l }
         ; `Spell { Spell.name = Some "ice" }
         ; `Label "scroll"
        |]
    ; required_items = [| `Label "required" |]
    }
  in
  let b = Rt.Builder.create () in
  let bytes = Inventory.pack b original |> Inventory.finish_buf P.Bytes b in
  check_ok "object verifier" (Inventory.verify P.Bytes bytes);
  let (Rt.Root (buf, inventory)) = Inventory.root P.Bytes bytes in
  Alcotest.(check bool) "object roundtrip" true (Inventory.unpack buf inventory = original)
;;

let build_after_optional_mismatch ~fail_first =
  let b = B.create () in
  let label = B.create_string b "value" in
  if fail_first
  then
    Alcotest.check_raises
      "length mismatch"
      (Invalid_argument "Builder.create_union_vector: tag and value lengths differ")
      (fun () -> ignore (B.create_union_vector P.TUByte b [| '\x03' |] [||]));
  let tags, values = B.create_union_vector P.TUByte b [| '\x03' |] [| Some label |] in
  let table = B.start_table b ~n_fields:2 in
  let table = B.push_slot_ref 0 tags table in
  let table = B.push_slot_ref 1 values table in
  B.finish P.Bytes b (B.end_table table)
;;

let check_mismatch_is_atomic () =
  Alcotest.(check bytes)
    "builder remains reusable"
    (build_after_optional_mismatch ~fail_first:false)
    (build_after_optional_mismatch ~fail_first:true)
;;

let check_unknown_and_inconsistent_tags () =
  let bytes = create_inventory () in
  let tag_field = root_field bytes 4 in
  let tags = tag_field + u32 bytes tag_field in
  let unknown = Bytes.copy bytes in
  Bytes.set unknown (tags + 4 + 1) '\x63';
  check_ok "unknown tag by default" (Inventory.verify P.Bytes unknown);
  (match
     Inventory.verify
       ~options:
         { Flatbuffers.Verifier.default_options with reject_unknown_union_tags = true }
       P.Bytes
       unknown
   with
   | Ok () -> Alcotest.fail "strict verification accepted an unknown tag"
   | Error _ -> ());
  let (Rt.Root (buf, inventory)) = Inventory.root P.Bytes unknown in
  Alcotest.(check string)
    "default callback"
    "unknown:<UnionVector.Item: 99>"
    (render_item buf inventory 1);
  let inconsistent = Bytes.copy bytes in
  Bytes.set_int32_le inconsistent tags 3l;
  match Inventory.verify P.Bytes inconsistent with
  | Ok () -> Alcotest.fail "accepted unequal tag and value vector lengths"
  | Error _ -> ()
;;

let test_cases =
  Alcotest.
    [ test_case "Zero-copy roundtrip" `Quick check_zero_copy_roundtrip
    ; test_case "Absent and required vectors" `Quick check_absent_optional_and_required
    ; test_case "Object API roundtrip" `Quick check_object_api_roundtrip
    ; test_case "Builder mismatch is atomic" `Quick check_mismatch_is_atomic
    ; test_case "Unknown and inconsistent tags" `Quick check_unknown_and_inconsistent_tags
    ]
;;
