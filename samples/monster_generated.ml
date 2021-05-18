(* automatically generated by the FlatBuffers compiler, do not modify *)

open FlatBuffers

module MyGame = struct

module Sample = struct

module Weapon = struct
    type t = {b: ByteBuffer.t; pos: t ByteBuffer.offset}

    let init b pos = {b;pos}

    let of_union b pos = {b=b; pos=ByteBuffer.offset_of_union pos}

    let getRootAsWeapon b =
        let offset = (ByteBuffer.read_ocaml_int32 b (ByteBuffer.position b)) + (ByteBuffer.position b) in
        init b offset

    (* Weapon *)
    let name t =
        let offset = ByteBuffer.__offset t.b t.pos 4 in
        if ByteBuffer.not_null offset then Some (ByteBuffer.__string t.b (t.pos + offset))
        else None

    (* Weapon *)
    let damage t =
        let offset = ByteBuffer.__offset t.b t.pos 6 in
        if ByteBuffer.not_null offset then let offset = t.pos + offset in ByteBuffer.readInt16 t.b offset
        else 0

    let start builder =
        Builder.startObject  builder 2

    let addName builder name =
        Builder.addFieldOffset builder 0 name (0)

    let addDamage builder damage =
        Builder.addFieldInt16 builder 1 damage (0)

    let end_ builder =
        Builder.endObject builder


    let create ~builder~name ~damage =
        start builder;
        addName builder name;
        addDamage builder damage;
        end_ builder
end

module Vec3 = struct
    type t = {b: ByteBuffer.t; pos: t ByteBuffer.offset}

    let init b pos = {b;pos}

    let of_union b pos = {b=b; pos=ByteBuffer.offset_of_union pos}

    (* Vec3 *)
    let x t =
        let offset = t.pos + 0 in
        ByteBuffer.readFloat32 t.b offset

    (* Vec3 *)
    let y t =
        let offset = t.pos + 4 in
        ByteBuffer.readFloat32 t.b offset

    (* Vec3 *)
    let z t =
        let offset = t.pos + 8 in
        ByteBuffer.readFloat32 t.b offset


    let create ~builder ~x ~y ~z =
        Builder.prep builder ~additional_bytes:4 12;
        Builder.addFloat32 builder z;
        Builder.addFloat32 builder y;
        Builder.addFloat32 builder x;
        Builder.offset builder
end

module Equipment = struct
     type t =
        | NONE
        | Weapon

    let of_int = function
        | 0 -> NONE
        | 1 -> Weapon
        | _ -> failwith "Invalid value"

    let to_int = function
        | NONE -> 0
        | Weapon -> 1

end

module Color = struct
     type t =
        | Red
        | Green
        | Blue

    let of_int = function
        | 0 -> Red
        | 1 -> Green
        | 2 -> Blue
        | _ -> failwith "Invalid value"

    let to_int = function
        | Red -> 0
        | Green -> 1
        | Blue -> 2

end

module Monster = struct
    type t = {b: ByteBuffer.t; pos: t ByteBuffer.offset}

    let init b pos = {b;pos}

    let of_union b pos = {b=b; pos=ByteBuffer.offset_of_union pos}

    let getRootAsMonster b =
        let offset = (ByteBuffer.read_ocaml_int32 b (ByteBuffer.position b)) + (ByteBuffer.position b) in
        init b offset

    (* Monster *)
    let pos t =
        let offset = ByteBuffer.__offset t.b t.pos 4 in
        if ByteBuffer.not_null offset then Some (let offset = t.pos + offset in Vec3.init t.b offset)
        else None

    (* Monster *)
    let mana t =
        let offset = ByteBuffer.__offset t.b t.pos 6 in
        if ByteBuffer.not_null offset then let offset = t.pos + offset in ByteBuffer.readInt16 t.b offset
        else 150

    (* Monster *)
    let hp t =
        let offset = ByteBuffer.__offset t.b t.pos 8 in
        if ByteBuffer.not_null offset then let offset = t.pos + offset in ByteBuffer.readInt16 t.b offset
        else 100

    (* Monster *)
    let name t =
        let offset = ByteBuffer.__offset t.b t.pos 10 in
        if ByteBuffer.not_null offset then Some (ByteBuffer.__string t.b (t.pos + offset))
        else None

    let inventoryLength t =
        let offset = ByteBuffer.__offset t.b t.pos 14 in
        if(ByteBuffer.not_null offset) then ByteBuffer.__vector_len t.b (t.pos + offset)
        else ByteBuffer.null

    let inventory t index =
        let offset = ByteBuffer.__offset t.b t.pos 14 in
        if(ByteBuffer.not_null offset) then
            let index = index in
            let offset = (ByteBuffer.__vector t.b (t.pos + offset)) + index in
            ByteBuffer.readUint8 t.b offset
        else 0

    (* Monster *)
    let color t =
        let offset = ByteBuffer.__offset t.b t.pos 16 in
        if ByteBuffer.not_null offset then Color.of_int (let offset = t.pos + offset in ByteBuffer.readInt8 t.b offset)
        else Color.Blue

    let weaponsLength t =
        let offset = ByteBuffer.__offset t.b t.pos 18 in
        if(ByteBuffer.not_null offset) then ByteBuffer.__vector_len t.b (t.pos + offset)
        else ByteBuffer.null

    let weapons t index =
        let offset = ByteBuffer.__offset t.b t.pos 18 in
        if(ByteBuffer.not_null offset) then
            let index = index * 4 in
            let offset = (ByteBuffer.__vector t.b (t.pos + offset)) + index in
            let offset = ByteBuffer.__indirect t.b offset in
            Some (Weapon.init t.b offset)
        else None

    (* Monster *)
    let equipped_type t =
        let offset = ByteBuffer.__offset t.b t.pos 20 in
        if ByteBuffer.not_null offset then Equipment.of_int (let offset = t.pos + offset in ByteBuffer.readUint8 t.b offset)
        else Equipment.NONE

    (* Monster *)
    let equipped t =
        ByteBuffer.__union t.b t.pos 22

    let pathLength t =
        let offset = ByteBuffer.__offset t.b t.pos 24 in
        if(ByteBuffer.not_null offset) then ByteBuffer.__vector_len t.b (t.pos + offset)
        else ByteBuffer.null

    let path t index =
        let offset = ByteBuffer.__offset t.b t.pos 24 in
        if(ByteBuffer.not_null offset) then
            let index = index * 12 in
            let offset = (ByteBuffer.__vector t.b (t.pos + offset)) + index in
            let offset = ByteBuffer.__indirect t.b offset in
            Some (Vec3.init t.b offset)
        else None

    let start builder =
        Builder.startObject  builder 11

    let addPos builder pos =
        Builder.addFieldOffset builder 0 pos (0)

    let addMana builder mana =
        Builder.addFieldInt16 builder 1 mana (150)

    let addHp builder hp =
        Builder.addFieldInt16 builder 2 hp (100)

    let addName builder name =
        Builder.addFieldOffset builder 3 name (0)

    let addInventory builder inventory =
        Builder.addFieldOffset builder 5 inventory (0)

    let startMonsterInventory builder numElems =
        Builder.startVector builder 1 numElems 1

    let addColor builder color =
        Builder.addFieldInt8 builder 6 color (2)

    let addWeapons builder weapons =
        Builder.addFieldOffset builder 7 weapons (0)

    let startMonsterWeapons builder numElems =
        Builder.startVector builder 4 numElems 4

    let addEquippedType builder equippedType =
        Builder.addFieldUint8 builder 8 equippedType (0)

    let addEquipped builder equipped =
        Builder.addFieldOffset builder 9 equipped (0)

    let addPath builder path =
        Builder.addFieldOffset builder 10 path (0)

    let startMonsterPath builder numElems =
        Builder.startVector builder 12 numElems 4

    let end_ builder =
        Builder.endObject builder


    let create ~builder~pos ~mana ~hp ~name ~inventory ~color ~weapons ~equippedType ~equipped ~path =
        start builder;
        addPos builder pos;
        addMana builder mana;
        addHp builder hp;
        addName builder name;
        addInventory builder inventory;
        addColor builder color;
        addWeapons builder weapons;
        addEquippedType builder equippedType;
        addEquipped builder equipped;
        addPath builder path;
        end_ builder
end

end (* Sample *)
end (* MyGame *)
