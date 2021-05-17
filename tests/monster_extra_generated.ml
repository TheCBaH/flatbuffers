(* automatically generated by the FlatBuffers compiler, do not modify *)

open FlatBuffers

module MyGame = struct

module MonsterExtra = struct
    type t    type offset = {b: ByteBuffer.t; pos: t ByteBuffer.offset}

    let init b pos = {b;pos}

    let of_union b pos = {b=b; pos=ByteBuffer.offset_of_union pos}

    let getRootAsMonsterExtra b =
        let offset = (ByteBuffer.read_ocaml_int32 b (ByteBuffer.position b)) + (ByteBuffer.position b) in
        init b offset

    (* MonsterExtra *)
    let d0 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 4 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat64 t.b offset
        else Float.nan

    (* MonsterExtra *)
    let d1 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 6 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat64 t.b offset
        else Float.nan

    (* MonsterExtra *)
    let d2 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 8 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat64 t.b offset
        else Float.infinity

    (* MonsterExtra *)
    let d3 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 10 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat64 t.b offset
        else Float.neg_infinity

    (* MonsterExtra *)
    let f0 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 12 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat32 t.b offset
        else Float.nan

    (* MonsterExtra *)
    let f1 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 14 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat32 t.b offset
        else Float.nan

    (* MonsterExtra *)
    let f2 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 16 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat32 t.b offset
        else Float.infinity

    (* MonsterExtra *)
    let f3 (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 18 in
        if(offset!=0) then let offset = t.pos + offset in ByteBuffer.readFloat32 t.b offset
        else Float.neg_infinity

    let dvecLength (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 20 in
        if(ByteBuffer.not_null offset) then ByteBuffer.__vector_len t.b (t.pos + offset)
        else ByteBuffer.null

    let dvec (t:offset) index =
        let offset = ByteBuffer.__offset t.b t.pos 20 in
        if(ByteBuffer.not_null offset) then
            let index = index * 8 in
            let offset = (ByteBuffer.__vector t.b (t.pos + offset)) + index in
            ByteBuffer.readFloat64 t.b offset
        else 0.0

    let fvecLength (t:offset) =
        let offset = ByteBuffer.__offset t.b t.pos 22 in
        if(ByteBuffer.not_null offset) then ByteBuffer.__vector_len t.b (t.pos + offset)
        else ByteBuffer.null

    let fvec (t:offset) index =
        let offset = ByteBuffer.__offset t.b t.pos 22 in
        if(ByteBuffer.not_null offset) then
            let index = index * 4 in
            let offset = (ByteBuffer.__vector t.b (t.pos + offset)) + index in
            ByteBuffer.readFloat32 t.b offset
        else 0.0

    let start builder =
        Builder.startObject  builder 11

    let addD0 builder d0 =
        Builder.addFieldFloat64 builder 0 d0 (Float.nan)

    let addD1 builder d1 =
        Builder.addFieldFloat64 builder 1 d1 (Float.nan)

    let addD2 builder d2 =
        Builder.addFieldFloat64 builder 2 d2 (Float.infinity)

    let addD3 builder d3 =
        Builder.addFieldFloat64 builder 3 d3 (Float.neg_infinity)

    let addF0 builder f0 =
        Builder.addFieldFloat32 builder 4 f0 (Float.nan)

    let addF1 builder f1 =
        Builder.addFieldFloat32 builder 5 f1 (Float.nan)

    let addF2 builder f2 =
        Builder.addFieldFloat32 builder 6 f2 (Float.infinity)

    let addF3 builder f3 =
        Builder.addFieldFloat32 builder 7 f3 (Float.neg_infinity)

    let addDvec builder dvec =
        Builder.addFieldOffset builder 8 dvec (0)

    let startMonsterExtraDvec builder numElems =
        Builder.startVector builder 8 numElems 8

    let addFvec builder fvec =
        Builder.addFieldOffset builder 9 fvec (0)

    let startMonsterExtraFvec builder numElems =
        Builder.startVector builder 4 numElems 4

    let end_ builder =
        Builder.endObject builder


    let create ~builder~d0 ~d1 ~d2 ~d3 ~f0 ~f1 ~f2 ~f3 ~dvec ~fvec =
        start builder;
        addD0 builder d0;
        addD1 builder d1;
        addD2 builder d2;
        addD3 builder d3;
        addF0 builder f0;
        addF1 builder f1;
        addF2 builder f2;
        addF3 builder f3;
        addDvec builder dvec;
        addFvec builder fvec;
        end_ builder
end

end (* MyGame *)
