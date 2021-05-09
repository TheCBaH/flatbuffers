let _SIZEOF_SHORT = 2

let _SIZEOF_INT = 4

let _FILE_IDENTIFIER_LENGTH = 4

let isLittleEndian = not Sys.big_endian

module ByteBuffer = struct
  type t =
    { mutable position: int
    ; bytes:
        (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    }

  type union

  type 'a offset = {b: t; pos: int}

  let offset t pos = {b= t; pos}

  let offset_of_union (o : union offset) = offset o.b o.pos

  let allocate byte_size =
    { bytes= Bigarray.Array1.create Bigarray.char Bigarray.c_layout byte_size
    ; position= 0 }

  let of_strings sl =
    let size = List.fold_left (fun l s -> l + String.length s) 0 sl in
    let t = allocate size in
    ignore
      (List.fold_left
         (fun o s ->
           let len = String.length s in
           for i = 0 to len - 1 do
             Bigarray.Array1.set t.bytes (o + i) (String.get s i)
           done ;
           o + len )
         0 sl ) ;
    t

  let of_string s = of_strings [s]

  let clear t = t.position <- 0

  let bytes t = t.bytes

  let position t = t.position

  let setPosition t position = t.position <- position

  let capacity t = Bigarray.Array1.dim t.bytes

  let int_length = if max_int == 1073741823 then 31 else 63

  let readUint8 t offset = Char.code (Bigarray.Array1.get t.bytes offset)

  let write_byte t offset value =
    let b = value land 0xFF in
    Bigarray.Array1.set t.bytes offset (Char.chr b)

  let writeUint8 t offset value =
    Bigarray.Array1.set t.bytes offset (Char.chr value)

  let readBool t offset = readUint8 t offset != 0

  let sign_extend_int n bits =
    let shift = int_length - bits in
    (n lsl shift) asr shift

  let readInt8 t offset =
    let uint8 = readUint8 t offset in
    sign_extend_int uint8 8

  let writeInt8 t offset value =
    if value > 0x7F || value < -0x80 then invalid_arg "Builder.writeUint16" ;
    write_byte t offset value

  let readUint16 t offset =
    readUint8 t offset lor (readUint8 t (offset + 1) lsl 8)

  let writeUint16 t offset value =
    if value > 0xFFFF || value < 0 then invalid_arg "ByteBuffer.writeUint16" ;
    write_byte t offset value ;
    write_byte t (offset + 1) (value lsr 8)

  let readInt16 t offset =
    let uint16 = readUint16 t offset in
    sign_extend_int uint16 16

  let writeInt16 t offset value =
    if value > 0x7FFF || value < -0x8000 then
      invalid_arg "ByteBuffer.writeInt16" ;
    write_byte t offset value ;
    write_byte t (offset + 1) (value lsr 8)

  let readUint32 t offset =
    Int64.logor
      (Int64.of_int (readUint16 t offset))
      (Int64.shift_left (Int64.of_int (readUint16 t (offset + 2))) 16)

  let write_uint24 t offset value =
    write_byte t offset value ;
    write_byte t (offset + 1) (value lsr 8) ;
    write_byte t (offset + 2) (value lsr 16)

  let writeUint32 t offset value =
    if Int64.compare value 0xFFFFFFFFL > 0 || Int64.compare value 0L < 0 then
      invalid_arg "ByteBuffer.writeUint32" ;
    let b012 = Int64.to_int (Int64.logand value 0xFFFFFFL) in
    write_uint24 t offset b012 ;
    write_byte t (offset + 3)
      (Int64.to_int (Int64.shift_right_logical value 24))

  let read_ocaml_int32 t offset =
    let lo = readUint16 t offset and hi = readUint16 t (offset + 2) in
    let int = lo lor (hi lsl 16) in
    if int_length == 63 then sign_extend_int int 32
    else
      let msb = hi lsr 14 in
      if msb == 0 || msb == 3 then int
      else failwith "FlatBuffers: read_int overflow"

  let write_ocaml_int32 t offset value =
    write_uint24 t offset value ;
    let b = value lsr 24 in
    let b = if int_length = 31 then sign_extend_int b 7 else b in
    write_byte t (offset + 3) b

  let readInt32 t offset =
    Int32.logor
      (Int32.of_int (readUint16 t offset))
      (Int32.shift_left (Int32.of_int (readUint16 t (offset + 2))) 16)

  let writeInt32 t offset value =
    let b012 = Int32.to_int (Int32.logand value 0xFFFFFFl) in
    write_uint24 t offset b012 ;
    write_byte t (offset + 3)
      (Int32.to_int (Int32.shift_right_logical value 24))

  let readInt64 t offset =
    Int64.logor (readUint32 t offset)
      (Int64.shift_left (readUint32 t (offset + 4)) 32)

  let writeInt64 t offset value =
    let b012 = Int64.to_int (Int64.logand value 0xFFFFFFL) in
    write_uint24 t offset b012 ;
    let b345 =
      Int64.to_int (Int64.logand (Int64.shift_right_logical value 24) 0xFFFFFFL)
    in
    write_uint24 t (offset + 3) b345 ;
    let b67 =
      Int64.to_int (Int64.logand (Int64.shift_right_logical value 48) 0xFFFFFFL)
    in
    write_byte t (offset + 6) b67 ;
    write_byte t (offset + 7) (b67 lsr 8)

  let readUint64 = readInt64

  let writeUint64 = writeInt64

  let readFloat32 t offset = Int32.float_of_bits (readInt32 t offset)

  let writeFloat32 t offset value =
    writeInt32 t offset (Int32.bits_of_float value)

  let readFloat64 t offset = Int64.float_of_bits (readInt64 t offset)

  let writeFloat64 t offset value =
    writeInt64 t offset (Int64.bits_of_float value)

  let read_string t ~offset ~length =
    let str = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set str i (Char.chr (readUint8 t (offset + i)))
    done ;
    Bytes.to_string str

  let write_string t s offset =
    let length = String.length s in
    for i = 0 to length - 1 do
      writeUint8 t (offset + i) (Char.code (String.get s i))
    done

  let getBufferIndentifier t =
    if capacity t < t.position + _SIZEOF_INT + _FILE_IDENTIFIER_LENGTH then
      failwith "FlatBuffers: ByteBuffer is too short to contain an identifier"
    else
      read_string t ~offset:(t.position + _SIZEOF_INT)
        ~length:_FILE_IDENTIFIER_LENGTH

  let __offset t bb_pos vtable_offset =
    let vtable = bb_pos - read_ocaml_int32 t bb_pos in
    if vtable_offset < readInt16 t vtable then
      readInt16 t (vtable + vtable_offset)
    else 0

  let __union o field_offset : union offset option =
    let offset = __offset o.b o.pos field_offset in
    if offset != 0 then
      let offset = o.pos + offset in
      let pos = offset + read_ocaml_int32 o.b offset in
      Some {b= o.b; pos}
    else None

  let __string t offset =
    let offset = offset + read_ocaml_int32 t offset in
    let length = read_ocaml_int32 t offset in
    let offset = offset + _SIZEOF_INT in
    read_string t ~offset ~length

  let __indirect t offset = offset + read_ocaml_int32 t offset

  let __vector t offset = offset + read_ocaml_int32 t offset + _SIZEOF_INT

  let __vector_len t offset =
    read_ocaml_int32 t (offset + read_ocaml_int32 t offset)

  let __has_identifier t ident =
    if String.length ident != _FILE_IDENTIFIER_LENGTH then
      failwith "FlatBuffers: file identifier invalid"
    else
      let rec loop t ident i =
        if i == _FILE_IDENTIFIER_LENGTH then true
        else if
          Char.code (String.get ident i)
          == readUint8 t (t.position + _SIZEOF_INT + i)
        then loop t ident (i + 1)
        else false
      in
      loop t ident 0
end

let do_test r w v s =
  let offset = 0 in
  let t = ByteBuffer.of_string s in
  let v' = r t offset in
  assert (v = v') ;
  let len = String.length s in
  let t = ByteBuffer.allocate len in
  w t offset v ;
  let s' = ByteBuffer.read_string t 0 len in
  assert (s' = s) ;
  true

let _ = assert (do_test ByteBuffer.readInt8 ByteBuffer.writeInt8 0 "\x00")

let _ = assert (do_test ByteBuffer.readUint8 ByteBuffer.writeUint8 0 "\x00")

let _ = assert (do_test ByteBuffer.readInt8 ByteBuffer.writeInt8 (-1) "\xFF")

let _ =
  assert (do_test ByteBuffer.readInt16 ByteBuffer.writeInt16 (-1) "\xFF\xFF")

let _ =
  assert (do_test ByteBuffer.readUint16 ByteBuffer.writeUint16 0xFFFF "\xFF\xFF")

let _ =
  assert (do_test ByteBuffer.readInt16 ByteBuffer.writeInt16 (-32768) "\x00\x80")

let _ =
  assert (do_test ByteBuffer.readUint16 ByteBuffer.writeUint16 0xF0FF "\xFF\xF0")

let _ =
  assert (
    do_test ByteBuffer.readUint32 ByteBuffer.writeUint32 0x80030201L
      "\x01\x02\x03\x80" )

let _ =
  assert (
    do_test ByteBuffer.readInt32 ByteBuffer.writeInt32 (-2147286527l)
      "\x01\x02\x03\x80" )

let _ =
  assert (
    do_test ByteBuffer.read_ocaml_int32 ByteBuffer.write_ocaml_int32 0x24030201
      "\x01\x02\x03\x24" )

let _ =
  assert (
    do_test ByteBuffer.read_ocaml_int32 ByteBuffer.write_ocaml_int32
      (-1006435839) "\x01\x02\x03\xC4" )

let _ =
  assert (
    do_test ByteBuffer.readUint64 ByteBuffer.writeUint64 0x0807060504030201L
      "\x01\x02\x03\x04\x05\x06\x07\x08" )

module DIntArray = struct
  type t = {mutable len: int; mutable table: int array}

  let create () = {len= 0; table= Array.make 1 0}

  let length t = t.len

  let get t o =
    if o < t.len then t.table.(o) else invalid_arg "index out of bounds"

  let set t o v =
    if o < t.len then t.table.(o) <- v else invalid_arg "index out of bounds"

  let push t v =
    let o = t.len + 1 in
    if o >= Array.length t.table then (
      let len = 2 * Array.length t.table in
      let table = Array.make len 0 in
      Array.blit t.table 0 table 0 t.len ;
      t.table <- table ) ;
    t.len <- o ;
    set t o v
end

module Builder = struct
  (* module Builder = struct *)
  type t =
    { mutable bb: ByteBuffer.t
    ; mutable vtable: int array
    ; mutable vtables: DIntArray.t
    ; mutable space: int
    ; mutable minalign: int
    ; mutable vtable_in_use: int
    ; mutable isNested: bool
    ; mutable object_start: int
    ; mutable vector_num_elems: int
    ; mutable force_defaults: bool }

  let create ?(initial_size = 256) () =
    { bb= ByteBuffer.allocate initial_size
    ; vtable= Array.make 0 0
    ; vtables= DIntArray.create ()
    ; space= initial_size
    ; minalign= 1
    ; vtable_in_use= 0
    ; isNested= false
    ; object_start= 0
    ; vector_num_elems= 0
    ; force_defaults= false }

  let clear t =
    ByteBuffer.clear t.bb ;
    t.vtable <- Array.make 0 0 ;
    t.vtables <- DIntArray.create () ;
    t.space <- ByteBuffer.capacity t.bb ;
    t.minalign <- 1 ;
    t.vtable_in_use <- 0 ;
    t.isNested <- false ;
    t.object_start <- 0 ;
    t.vector_num_elems <- 0 ;
    t.force_defaults <- false

  let forceDefaulta t forceDefaulta = t.force_defaults <- forceDefaulta

  let dataBuffer t = t.bb

  let offset t = ByteBuffer.capacity t.bb - t.space

  let asByteArray t =
    Bigarray.Array1.sub t.bb.ByteBuffer.bytes (ByteBuffer.position t.bb)
      (offset t)

  let growByteBuffer bb =
    let old_buf_size = ByteBuffer.capacity bb in
    if old_buf_size > max_int / 2 then
      failwith "FlatBuffers: cannot grow buffer beyond its limits" ;
    let new_buf_size = old_buf_size * 2 in
    let nbb = ByteBuffer.allocate new_buf_size in
    ByteBuffer.setPosition nbb (new_buf_size - old_buf_size) ;
    let sub =
      Bigarray.Array1.sub nbb.ByteBuffer.bytes
        (new_buf_size - old_buf_size)
        old_buf_size
    in
    Bigarray.Array1.blit bb.ByteBuffer.bytes sub ;
    nbb

  let rec pad t byte_size =
    if byte_size > 0 then (
      t.space <- t.space - 1 ;
      ByteBuffer.write_byte t.bb t.space 0 ;
      pad t (byte_size - 1) )

  let prep t ?(additional_bytes = 0) size =
    if size > t.minalign then t.minalign <- size ;
    let align_size =
      lnot (ByteBuffer.capacity t.bb - t.space + additional_bytes + 1)
      land (size - 1)
    in
    let rec grow t align_size size additional_bytes =
      if t.space < align_size + size + additional_bytes then (
        let old_buf_size = ByteBuffer.capacity t.bb in
        t.bb <- growByteBuffer t.bb ;
        t.space <- t.space + (ByteBuffer.capacity t.bb - old_buf_size) ;
        grow t align_size size additional_bytes )
      else ()
    in
    grow t align_size size additional_bytes ;
    pad t align_size

  let writeInt8 t value =
    t.space <- t.space - 1 ;
    ByteBuffer.writeInt8 t.bb t.space value

  let writeUint8 t value =
    t.space <- t.space - 1 ;
    ByteBuffer.writeUint8 t.bb t.space value

  let writeInt16 t value =
    t.space <- t.space - 2 ;
    ByteBuffer.writeInt16 t.bb t.space value

  let writeUint16 t value =
    t.space <- t.space - 2 ;
    ByteBuffer.writeUint16 t.bb t.space value

  let writeInt32 t value =
    t.space <- t.space - 4 ;
    ByteBuffer.writeInt32 t.bb t.space value

  let writeUint32 t value =
    t.space <- t.space - 4 ;
    ByteBuffer.writeUint32 t.bb t.space value

  let write_ocaml_int32 t value =
    t.space <- t.space - 4 ;
    ByteBuffer.write_ocaml_int32 t.bb t.space value

  let writeInt64 t value =
    t.space <- t.space - 8 ;
    ByteBuffer.writeInt64 t.bb t.space value

  let writeFloat32 t value =
    t.space <- t.space - 4 ;
    ByteBuffer.writeFloat32 t.bb t.space value

  let writeFloat64 t value =
    t.space <- t.space - 8 ;
    ByteBuffer.writeFloat64 t.bb t.space value

  let addInt8 t value = prep t 1 ; writeInt8 t value

  let addUint8 t value = prep t 1 ; writeUint8 t value

  let addInt16 t value = prep t 2 ; writeInt16 t value

  let addUint16 t value = prep t 2 ; writeUint16 t value

  let addInt32 t value = prep t 4 ; writeInt32 t value

  let addUint32 t value = prep t 4 ; writeUint32 t value

  let add_ocaml_int32 t value = prep t 4 ; write_ocaml_int32 t value

  let addInt64 t value = prep t 8 ; writeInt64 t value

  let addFloat32 t value = prep t 4 ; writeFloat32 t value

  let addFloat64 t value = prep t 8 ; writeFloat64 t value

  let slot t voffset = t.vtable.(voffset) <- offset t

  let addFieldInt8 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addInt8 t value ; slot t voffset )

  let addFieldUint8 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addUint8 t value ; slot t voffset )

  let addFieldBool t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addUint8 t (if value then 0 else 0xFF) ;
      slot t voffset )

  let addFieldInt16 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addInt16 t value ; slot t voffset )

  let addFieldUint16 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addUint16 t value ; slot t voffset )

  let addFieldInt32 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addInt32 t value ; slot t voffset )

  let addFieldUint32 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addUint32 t value ; slot t voffset )

  let addFieldInt64 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addInt64 t value ; slot t voffset )

  let addFieldUint64 = addFieldInt64

  let addFieldFloat32 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addFloat32 t value ; slot t voffset )

  let addFieldFloat64 t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addFloat64 t value ; slot t voffset )

  let addOffset t _offset =
    prep t _SIZEOF_INT ;
    write_ocaml_int32 t (offset t - _offset + _SIZEOF_INT)

  let addFieldOffset t voffset value defaultValue =
    if t.force_defaults || value != defaultValue then (
      addOffset t value ; slot t voffset )

  let nested t obj =
    if obj != offset t then
      failwith "FlatBuffers: struct must be serialized inline."

  let notNested t =
    if t.isNested then
      failwith "FlatBuffers: object serialization must not be nested."

  let addFieldStruct t voffset value defaultValue =
    if value != defaultValue then (nested t value ; slot t voffset)

  let startObject t numfields =
    notNested t ;
    if Array.length t.vtable < numfields then t.vtable <- Array.make numfields 0
    else
      for i = 0 to numfields - 1 do
        t.vtable.(i) <- 0
      done ;
    t.vtable_in_use <- numfields ;
    t.isNested <- true ;
    t.object_start <- offset t

  let endObject t =
    if Array.length t.vtable == 0 || not t.isNested then
      failwith "FlatBuffers: endObject called without startObject" ;
    add_ocaml_int32 t 0 ;
    let vtableloc = offset t in
    (* Trim trailing zeroes. *)
    let rec trim_trailing_zeros t i =
      if i >= 0 && t.vtable.(i) == 0 then trim_trailing_zeros t (i - 1) else i
    in
    let i = trim_trailing_zeros t (t.vtable_in_use - 1) in
    let trimmed_size = i + 1 in
    (* Writ^e out the current vtable. *)
    let rec write_current_vtable t vtableloc i =
      if i >= 0 then (
        (* Offset relative to the start of the table *)
        let offset =
          if t.vtable.(i) != 0 then vtableloc - t.vtable.(i) else 0
        in
        addInt16 t offset ;
        write_current_vtable t vtableloc (i - 1) )
    in
    write_current_vtable t vtableloc i ;
    let standard_fields = 2 in
    (* The fields below: *)
    addInt16 t (vtableloc - t.object_start) ;
    let len = (trimmed_size + standard_fields) * _SIZEOF_SHORT in
    addInt16 t len ;
    (* Search for an existing vtable that matches the current one. *)
    let vt1 = t.space in
    let rec search_existing_vtables t vt1 i =
      if i < DIntArray.length t.vtables then
        let vt2 = ByteBuffer.capacity t.bb - DIntArray.get t.vtables i in
        if len = ByteBuffer.readInt16 t.bb vt2 then
          search_vtable t vt1 vt2 len _SIZEOF_SHORT
        else 0
      else 0
    and search_vtable t len vt1 vt2 j =
      if j < len then
        if
          ByteBuffer.readInt16 t.bb (vt1 + j)
          != ByteBuffer.readInt16 t.bb (vt2 + j)
        then search_existing_vtables t vt1 (i + 1)
        else search_vtable t len vt1 vt2 (j + 1)
      else
        let existing_vtable = DIntArray.get t.vtables i in
        existing_vtable
    in
    let existing_vtable = search_existing_vtables t vt1 0 in
    if existing_vtable != 0 then (
      (* Found a match:
         Remove the current vtable. *)
      t.space <- ByteBuffer.capacity t.bb - vtableloc ;
      ByteBuffer.write_ocaml_int32 t.bb t.space (existing_vtable - vtableloc) )
    else (
      (* No match
         Add the location of the current vtable to the list of vtables. *)
      DIntArray.push t.vtables (offset t) ;
      ByteBuffer.write_ocaml_int32 t.bb
        (ByteBuffer.capacity t.bb - vtableloc)
        (offset t - vtableloc) ) ;
    t.isNested <- false ;
    vtableloc

  let finish t ?(file_identifier = "") root_table =
    if String.length file_identifier > 0 then
      prep t t.minalign ~additional_bytes:(_SIZEOF_INT + _FILE_IDENTIFIER_LENGTH) ;
    if String.length file_identifier != _FILE_IDENTIFIER_LENGTH then
      invalid_arg
        ( "FlatBuffers: file identifier must be length "
        ^ string_of_int _FILE_IDENTIFIER_LENGTH ) ;
    for i = _FILE_IDENTIFIER_LENGTH - 1 downto 0 do
      writeUint8 t (Char.code (String.get file_identifier i))
    done ;
    prep t t.minalign ~additional_bytes:_SIZEOF_INT ;
    addOffset t root_table ;
    ByteBuffer.setPosition t.bb t.space

  let requiredField t table field =
    let table_start = ByteBuffer.capacity t.bb - table in
    let vtable_start =
      table_start - ByteBuffer.read_ocaml_int32 t.bb table_start
    in
    let ok = ByteBuffer.readInt16 t.bb (vtable_start + field) != 0 in
    if not ok then
      failwith ("FlatBuffers: field " ^ string_of_int field ^ " must be set")

  let startVector t elem_size num_elems alignment =
    notNested t ;
    t.vector_num_elems <- num_elems ;
    prep t _SIZEOF_INT ~additional_bytes:(elem_size * num_elems) ;
    prep t alignment ~additional_bytes:(elem_size * num_elems)

  let endVector t =
    write_ocaml_int32 t t.vector_num_elems ;
    offset t

  let createString t s =
    addInt8 t 0 ;
    let len = String.length s in
    startVector t 1 len 1 ;
    t.space <- t.space - len ;
    ByteBuffer.setPosition t.bb t.space ;
    ByteBuffer.write_string t.bb s t.space ;
    endVector t
end
