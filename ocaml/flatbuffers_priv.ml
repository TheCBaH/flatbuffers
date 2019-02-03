
let _ = print_endline "flatbuffers"

let _SIZEOF_SHORT = 2

let _SIZEOF_INT = 4

let file_identifier_length = 4


let encoding_utf8_bytes  = 1

let encoding_utf16_string = 2

let isLittleEndian = not Sys.big_endian


let long low high =
  Int64.logor
    (Int64.of_int32 low)
    (Int64.shift_left (Int64.of_int32 high) 32)


let long_toFloat64 t = Int64.bits_of_float t

let long_equals t1 t2 = Int64.equal t1 t2

let long_zero = Int64.zero

let _ = -1 lsr (24+32)



module ByteBuffer = struct
  type t = {
      mutable position: int;
      bytes: (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    }
  let allocate byte_size = {
      bytes = Bigarray.Array1.create Bigarray.char Bigarray.c_layout byte_size;
      position = 0
    }
  let of_strings sl =
    let size = List.fold_left (fun l s -> l + (String.length s)) 0 sl in
    let t = allocate size in
    ignore (List.fold_left (fun o s ->
                let len = String.length s in
                for i=0 to len-1 do
                  Bigarray.Array1.set t.bytes (o+i) (String.get s i)
                done;
                o+len
              ) 0 sl);
    t
  let of_string s  = of_strings [s]
  let clear t =  t.position <- 0
  let bytes t = t.bytes
  let position t = t.position
  let set_position t position = t.position <- position
  let capacity t = Bigarray.Array1.dim t.bytes
  let int_length = if max_int == 1073741823 then 31 else 63

  let readUint8 t offset =
    Char.code (Bigarray.Array1.get t.bytes offset)

  let write_byte t offset value =
    let b = value land 0xFF in
    Bigarray.Array1.set t.bytes offset (Char.chr b)

  let writeUint8 t offset value =
    Bigarray.Array1.set t.bytes offset (Char.chr value)

  let readBool t offset =
    readUint8 t offset != 0

  let sign_extend_int n bits =
    let shift = int_length - bits in
    (n lsl shift) asr shift

  let readInt8 t offset =
    let uint8 = readUint8 t offset in
    sign_extend_int uint8 8

  let writeInt8 t offset value =
    if value > 0x7F || value < -0x80 then
      raise (Invalid_argument "Builder.writeUint16");
    write_byte t offset value

  let readUint16 t offset =
    (readUint8 t offset) lor ((readUint8 t (offset+1)) lsl 8 )

  let writeUint15 t offset value =
    if value > 0xFFFF || value < 0 then
      raise (Invalid_argument "ByteBuffer.writeUint16");
    write_byte t offset value;
    write_byte t (offset+1) (value lsr 8)

  let readInt16 t offset =
    let uint16  = readUint16 t offset in
    sign_extend_int uint16 16

  let writeInt16 t offset value =
    if value > 0x7FFF || value < -0x8000 then
      raise (Invalid_argument "ByteBuffer.writeInt16");
    write_byte t offset value;
    write_byte t (offset+1) (value lsr 8)

  let readUint32 t offset =
    Int64.logor
     (Int64.of_int (readUint16 t offset))
      ((Int64.shift_left
      (Int64.of_int (readUint16 t (offset+2)))
      16))

  let write_uint24 t offset value =
    write_byte t offset value;
    write_byte t (offset+1) (value lsr 8);
    write_byte t (offset+2) (value lsr 16)

  let writeUint32 t offset value =
    if Int64.compare value 0xFFFFFFFL > 0
       || Int64.compare value 0L < 0 then
      raise (Invalid_argument "ByteBuffer.writeInt16");
    let b012 = Int64.to_int (Int64.logand value 0xFFFFFFL) in
    write_uint24 t offset b012;
    write_byte t (offset+3) (Int64.to_int (Int64.shift_right_logical value 24))

  let read_ocaml_int32 t offset =
    let lo = readUint16 t offset
    and hi = readUint16 t (offset + 2) in
    let int = (lo lor (hi lsl 16)) in
    if int_length == 63 then
      sign_extend_int int 32
    else
      let msb = hi lsr 14 in
      if msb == 0 || msb == 3 then
        int
      else
        failwith "FlatBuffers: read_int overflow"

  let write_ocaml_int32 t offset value =
    write_uint24 t offset value;
    let b = value lsr 24 in
    let b =
      if int_length = 31 then
        sign_extend_int b 7
      else b in
    write_byte t (offset+3) b

  let readInt32 t offset =
    Int32.logor
     (Int32.of_int (readUint16 t offset))
      ((Int32.shift_left
      (Int32.of_int (readUint16 t (offset+2)))
      16))

  let writeInt32 t offset value =
    let b012 = Int32.to_int (Int32.logand value 0xFFFFFFl) in
    write_uint24 t offset b012;
    write_byte t (offset+3) (Int32.to_int (Int32.shift_right_logical value 24))

  let readInt64 t offset =
    Int64.logor
      (readUint32 t offset)
      (Int64.shift_left (readUint32 t (offset+4)) 32)

  let writeInt64 t offset value =
    let b012 = Int64.to_int (Int64.logand value 0xFFFFFFL) in
    write_uint24 t offset b012;
    let b345 = Int64.to_int (Int64.logand (Int64.shift_right_logical value 24) 0xFFFFFFL) in
    write_uint24 t (offset+3) b345;
    let b67 = Int64.to_int (Int64.logand (Int64.shift_right_logical value 48) 0xFFFFFFL) in
    write_byte t (offset+6) b67;
    write_byte t (offset+7) (b67 lsr 8)

  let readUint64 = readInt64

  let writeUint64 = writeInt64

  let readFloat32 t offset =
    Int32.float_of_bits (readInt32 t offset)

  let writeFloat32 t offset value =
    writeInt32 t offset (Int32.bits_of_float value)

  let readFloat64 t offset =
    Int64.float_of_bits (readInt64 t offset)

  let writeFloat64 t offset value =
    writeInt64 t offset (Int64.bits_of_float value)

  let get_string t ~offset ~length =
    let str = Bytes.create length in
    for i=0 to length-1 do
      Bytes.set str i
        (Char.chr (readUint8 t (offset + i)))
    done;
    Bytes.to_string str

  let getBufferIndentifier t =
    if (capacity t) < t.position + sizeof_int + file_identifier_length  then
      failwith "FlatBuffers: ByteBuffer is too short to contain an identifier"
    else get_string t ~offset:(t.position + sizeof_int) ~length:file_identifier_length

  let __offset t bb_pos vtable_offset =
    let vtable = bb_pos - (read_ocaml_int32 t bb_pos) in
    if vtable_offset < (readInt16 t vtable) then
      readInt16 t (vtable + vtable_offset)
    else 0

  let __union t offset =
    let bb_pos = offset + (read_ocaml_int32 t offset) in
    (t,bb_pos)

  let __string t offset =
    let offset = offset + (read_ocaml_int32 t offset) in
    let length = read_ocaml_int32 t offset in
    let offset = offset + sizeof_int in
    get_string t ~offset ~length

  let __indirect t offset =
    offset + (read_ocaml_int32 t offset)

  let __vector t offset =
    offset + (read_ocaml_int32 t offset) + sizeof_int

  let __vector_len t offset =
    read_ocaml_int32 t (offset + (read_ocaml_int32 t offset))

  let __has_identifier t ident =
    if String.length ident != file_identifier_length then
      failwith "FlatBuffers: file identifier invalid"
    else
      let rec loop t ident i =
        if i == file_identifier_length then true
        else if Char.code (String.get ident i) == readUint8 t (t.position + sizeof_int + i) then
          loop t ident (i+1)
        else false in
      loop t ident 0

end

let do_test s f =
  let offset = 0  in
  let t= ByteBuffer.of_string s in
  f t offset

let _ =  assert( 0 = do_test "\x00" ByteBuffer.readInt8)

let _ =  assert( -1 = do_test "\xFF" ByteBuffer.readInt8)

let _ =  assert( 0xff = do_test "\xFF" ByteBuffer.readUint8)

let _ =  assert( -1 = do_test "\xFF\xFF" ByteBuffer.readInt16)

let _ =  assert( 0xffff = do_test "\xFF\xFF" ByteBuffer.readUint16)

let _ =  assert( -32768 = do_test "\x00\x80" ByteBuffer.readInt16)

let _ =  assert( 0xf0ff = do_test "\xFF\xF0" ByteBuffer.readUint16)

let _ =  assert( 0x80030201L = do_test "\x01\x02\x03\x80" ByteBuffer.readUint32)

let _ =  assert( -2147286527l = do_test "\x01\x02\x03\x80" ByteBuffer.readInt32)

let _ =  assert( 0x24030201 = do_test "\x01\x02\x03\x24" ByteBuffer.read_ocaml_int32)
let _ =  assert( -1006435839 = do_test "\x01\x02\x03\xC4" ByteBuffer.read_ocaml_int32)

let _ =  assert( 0x0807060504030201L = do_test "\x01\x02\x03\x04\x05\x06\x07\x08" ByteBuffer.readUint64)

let _ =  assert( 0x0807060504030201L = do_test "\x01\x02\x03\x04\x05\x06\x07\x08" ByteBuffer.readInt64)


module Color = struct
  type t =
    Red
  | Green
  | Blue

  let of_int = function
      0 -> Red
    | 1 -> Green
    | 2 -> Blue
    | _ -> failwith "Invalid value"

  let to_int = function
      Red -> 0
    | Green  -> 1
    | Blue -> 2

end
(*
MyGame.Sample.Color = {
  Red: 0, 0: 'Red',
  Green: 1, 1: 'Green',
  Blue: 2, 2: 'Blue'
};
*)



(* module Builder = struct *)
type t= {
    mutable bb: ByteBuffer.t;
    mutable space:int;
    mutable minalign:int;
    mutable vtable_in_use:int;
    mutable isNested:bool;
    mutable object_start:int;
    mutable vector_num_elems:int;
    mutable force_defaults:bool;
    mutable vtable: int array;
(*
  this.vtables = [];
 *)
  }

let clear t =
  ByteBuffer.clear t.bb;
  t.vtable <- Array.make 0 0;
  t.space <- ByteBuffer.capacity t.bb;
  t.minalign <- 1;
  t.vtable_in_use <- 0;
  t.isNested <- false;
  t.object_start <- 0;
  t.vector_num_elems <- 0;
  t.force_defaults <- false

let create ?(initial_size=256)  () =
  {
    bb = ByteBuffer.allocate initial_size;
    vtable = Array.make 0 0;
    space = initial_size;
    minalign = 1;
    vtable_in_use = 0;
    isNested = false;
    object_start = 0;
    vector_num_elems = 0;
    force_defaults  = false
  }
  (*
  this.vtable = null;
  this.vtables = [];
   *)


let forceDefaulta t forceDefaulta =
  t.force_defaults <- forceDefaulta

let dataBuffer t = t.bb

let offset t =
  (ByteBuffer.capacity t.bb) - t.space

let asByteArray t =
  Bigarray.Array1.sub t.bb.ByteBuffer.bytes (ByteBuffer.position t.bb) (offset t)

let growByteBuffer bb =
  let old_buf_size = ByteBuffer.capacity bb in
  if old_buf_size > (max_int / 2)  then
    failwith "FlatBuffers: cannot grow buffer beyond its limits";
  let new_buf_size = old_buf_size * 2 in
  let nbb = ByteBuffer.allocate new_buf_size in
  ByteBuffer.set_position nbb (new_buf_size - old_buf_size);
  let sub = Bigarray.Array1.sub nbb.ByteBuffer.bytes (new_buf_size - old_buf_size) old_buf_size in
  Bigarray.Array1.blit bb.ByteBuffer.bytes sub;
  nbb

let rec pad t byte_size =
  if byte_size > 0 then begin
    t.space <- t.space - 1 ;
    ByteBuffer.write_byte t.bb t.space 0;
    pad t (byte_size - 1)
  end

let prep t ?(additional_bytes=0) size =
  if size > t.minalign then
    t.minalign <- size;
  let align_size =
    (lnot (( ((ByteBuffer.capacity t.bb) - t.space + additional_bytes)) + 1)) land (size - 1) in
  let rec grow t align_size size additional_bytes =
    if t.space < align_size + size + additional_bytes then begin
        let old_buf_size = ByteBuffer.capacity t.bb in
        t.bb <- growByteBuffer t.bb;
        t.space <- t.space + ((ByteBuffer.capacity t.bb) - old_buf_size);
        grow t align_size size additional_bytes
      end
    else () in
  grow t align_size size additional_bytes;
  pad t align_size

let writeInt8 t value =
  t.space <- t.space - 1;
  ByteBuffer.writeInt8 t.bb t.space value

let writeInt16 t value =
  t.space <- t.space - 2;
  ByteBuffer.writeInt16 t.bb t.space value

let writeInt32 t value =
  t.space <- t.space - 4;
  ByteBuffer.writeInt32 t.bb t.space value

let write_ocaml_int32 t value =
  t.space <- t.space - 4;
  ByteBuffer.write_ocaml_int32 t.bb t.space value

let writeInt64 t value =
  t.space <- t.space - 8;
  ByteBuffer.writeInt64 t.bb t.space value

let writeFloat32 t value =
  t.space <- t.space - 4;
  ByteBuffer.writeFloat32 t.bb t.space value

let writeFloat64 t value =
  t.space <- t.space - 8;
  ByteBuffer.writeFloat64 t.bb t.soace value

let addInt8 t value =
  prep t 1;
  writeInt8 t value

let addInt16 t value =
  prep t 2;
  writeInt16 t value

let addInt32 t value =
  prep t 4;
  writeInt32 t value

let addInt64 t value =
  prep t 8;
  writeInt64 t value

let addFloat32 t value =
  prep t 4;
  writeFloat32 t value

let addFloat64 t value =
  prep t 8;
  writeFloat64 t value

let slot t voffset =
  t.vtable.(voffset) <- offset t

let addFieldInt8 t voffset value defaultValue =
  if (t.force_defaults || value != defaultValue) then begin
      addInt8 t value;
      slot t voffset;
  end

let addFieldInt16 t voffset value defaultValue =
  if (t.force_defaults || value != defaultValue) then begin
      addInt16 t value;
      slot t voffset;
  end

let addFieldInt32 t voffset value defaultValue =
  if (t.force_defaults || value != defaultValue) then begin
      addInt32 t value;
      slot t voffset;
    end

let addFieldInt64 t voffset value defaultValue =
  if (t.force_defaults || value != defaultValue) then begin
      addInt64 t value;
      slot t voffset;
  end

let addFieldFloat32 t voffset value defaultValue =
  if (t.force_defaults || value != defaultValue) then begin
      addFloat32 t value;
      slot t voffset;
  end

let addFieldFloat64 t voffset value defaultValue =
  if (t.force_defaults || value != defaultValue) then begin
      addFloat64 t value;
      slot t voffset;
  end

let addOffset t _offset =
  prep t _SIZEOF_INT;
  write_ocaml_int32 t ((offset t) - _offset + _SIZEOF_INT)

let addFieldOffset t voffset value defaultValue =
  if t.force_defaults || value != defaultValue then begin
    addOffset t value;
    slot t voffset;
  end

let nested t obj =
  if obj != (offset t) then
    failwith "FlatBuffers: struct must be serialized inline."

let notNested t  =
  if t.isNested then
    failwith "FlatBuffers: object serialization must not be nested."

let addFieldStruct t voffset value defaultValue =
  if value != defaultValue then begin
    nested t value;
    slot t voffset;
  end

let startObject t numfields =
  notNested t;
  if Array.length t.vtable < numfields then
    t.vtable <- Array.make numfields 0
  else
    for i = 0 to numfields - 1 do
      t.vtable.(i) <- 0
    done;
  t.vtable_in_use <- numfields;
  t.isNested <- true;
  t.object_start <- offset t



let _ = 1

let _ = 1

 let _ = 1 lor 2


let _ = 1

(*

/// @cond FLATBUFFERS_INTERNAL
;



/// @cond FLATBUFFERS_INTERNAL

/// @cond FLATBUFFERS_INTERNAL

/**
 * Finish off writing the object that is under construction.
 *
 * @returns {flatbuffers.Offset} The offset to the object inside `dataBuffer`
 */
flatbuffers.Builder.prototype.endObject = function() {
  if (this.vtable == null || !this.isNested) {
    throw new Error('FlatBuffers: endObject called without startObject');
  }

  this.addInt32(0);
  var vtableloc = this.offset();

  // Trim trailing zeroes.
  var i = this.vtable_in_use - 1;
  for (; i >= 0 && this.vtable[i] == 0; i--) {}
  var trimmed_size = i + 1;

  // Write out the current vtable.
  for (; i >= 0; i--) {
    // Offset relative to the start of the table.
    this.addInt16(this.vtable[i] != 0 ? vtableloc - this.vtable[i] : 0);
  }

  var standard_fields = 2; // The fields below:
  this.addInt16(vtableloc - this.object_start);
  var len = (trimmed_size + standard_fields) * flatbuffers.SIZEOF_SHORT;
  this.addInt16(len);

  // Search for an existing vtable that matches the current one.
  var existing_vtable = 0;
  var vt1 = this.space;
outer_loop:
  for (i = 0; i < this.vtables.length; i++) {
    var vt2 = this.bb.capacity() - this.vtables[i];
    if (len == this.bb.readInt16(vt2)) {
      for (var j = flatbuffers.SIZEOF_SHORT; j < len; j += flatbuffers.SIZEOF_SHORT) {
        if (this.bb.readInt16(vt1 + j) != this.bb.readInt16(vt2 + j)) {
          continue outer_loop;
        }
      }
      existing_vtable = this.vtables[i];
      break;
    }
  }

  if (existing_vtable) {
    // Found a match:
    // Remove the current vtable.
    this.space = this.bb.capacity() - vtableloc;

    // Point table to existing vtable.
    this.bb.writeInt32(this.space, existing_vtable - vtableloc);
  } else {
    // No match:
    // Add the location of the current vtable to the list of vtables.
    this.vtables.push(this.offset());

    // Point table to current vtable.
    this.bb.writeInt32(this.bb.capacity() - vtableloc, this.offset() - vtableloc);
  }

  this.isNested = false;
  return vtableloc;
};
/// @endcond

/**
 * Finalize a buffer, poiting to the given `root_table`.
 *
 * @param {flatbuffers.Offset} root_table
 * @param {string=} opt_file_identifier
 */
flatbuffers.Builder.prototype.finish = function(root_table, opt_file_identifier) {
  if (opt_file_identifier) {
    var file_identifier = opt_file_identifier;
    this.prep(this.minalign, flatbuffers.SIZEOF_INT +
      flatbuffers.FILE_IDENTIFIER_LENGTH);
    if (file_identifier.length != flatbuffers.FILE_IDENTIFIER_LENGTH) {
      throw new Error('FlatBuffers: file identifier must be length ' +
        flatbuffers.FILE_IDENTIFIER_LENGTH);
    }
    for (var i = flatbuffers.FILE_IDENTIFIER_LENGTH - 1; i >= 0; i--) {
      this.writeInt8(file_identifier.charCodeAt(i));
    }
  }
  this.prep(this.minalign, flatbuffers.SIZEOF_INT);
  this.addOffset(root_table);
  this.bb.setPosition(this.space);
};

/// @cond FLATBUFFERS_INTERNAL
/**
 * This checks a required field has been set in a given table that has
 * just been constructed.
 *
 * @param {flatbuffers.Offset} table
 * @param {number} field
 */
flatbuffers.Builder.prototype.requiredField = function(table, field) {
  var table_start = this.bb.capacity() - table;
  var vtable_start = table_start - this.bb.readInt32(table_start);
  var ok = this.bb.readInt16(vtable_start + field) != 0;

  // If this fails, the caller will show what field needs to be set.
  if (!ok) {
    throw new Error('FlatBuffers: field ' + field + ' must be set');
  }
};

/**
 * Start a new array/vector of objects.  Users usually will not call
 * this directly. The FlatBuffers compiler will create a start/end
 * method for vector types in generated code.
 *
 * @param {number} elem_size The size of each element in the array
 * @param {number} num_elems The number of elements in the array
 * @param {number} alignment The alignment of the array
 */
flatbuffers.Builder.prototype.startVector = function(elem_size, num_elems, alignment) {
  this.notNested();
  this.vector_num_elems = num_elems;
  this.prep(flatbuffers.SIZEOF_INT, elem_size * num_elems);
  this.prep(alignment, elem_size * num_elems); // Just in case alignment > int.
};

/**
 * Finish off the creation of an array and all its elements. The array must be
 * created with `startVector`.
 *
 * @returns {flatbuffers.Offset} The offset at which the newly created array
 * starts.
 */
flatbuffers.Builder.prototype.endVector = function() {
  this.writeInt32(this.vector_num_elems);
  return this.offset();
};
/// @endcond

/**
 * Encode the string `s` in the buffer using UTF-8. If a Uint8Array is passed
 * instead of a string, it is assumed to contain valid UTF-8 encoded data.
 *
 * @return {flatbuffers.Offset} The offset in the buffer where the encoded string starts
 */
flatbuffers.Builder.prototype.createString = function(s) {
  if (s instanceof Uint8Array) {
    var utf8 = s;
  } else {
    var utf8 = [];
    var i = 0;

    while (i < s.length) {
      var codePoint;

      // Decode UTF-16
      var a = s.charCodeAt(i++);
      if (a < 0xD800 || a >= 0xDC00) {
        codePoint = a;
      } else {
        var b = s.charCodeAt(i++);
        codePoint = (a << 10) + b + (0x10000 - (0xD800 << 10) - 0xDC00);
      }

      // Encode UTF-8
      if (codePoint < 0x80) {
        utf8.push(codePoint);
      } else {
        if (codePoint < 0x800) {
          utf8.push(((codePoint >> 6) & 0x1F) | 0xC0);
        } else {
          if (codePoint < 0x10000) {
            utf8.push(((codePoint >> 12) & 0x0F) | 0xE0);
          } else {
            utf8.push(
              ((codePoint >> 18) & 0x07) | 0xF0,
              ((codePoint >> 12) & 0x3F) | 0x80);
          }
          utf8.push(((codePoint >> 6) & 0x3F) | 0x80);
        }
        utf8.push((codePoint & 0x3F) | 0x80);
      }
    }
  }

  this.addInt8(0);
  this.startVector(1, utf8.length, 1);
  this.bb.setPosition(this.space -= utf8.length);
  for (var i = 0, offset = this.space, bytes = this.bb.bytes(); i < utf8.length; i++) {
    bytes[offset++] = utf8[i];
  }
  return this.endVector();
};

/**
 * A helper function to avoid generated code depending on this file directly.
 *
 * @param {number} low
 * @param {number} high
 * @returns {flatbuffers.Long}
 */
flatbuffers.Builder.prototype.createLong = function(low, high) {
  return flatbuffers.Long.create(low, high);
};
////////////////////////////////////////////////////////////////////////////////
/// @cond FLATBUFFERS_INTERNAL
/**
 * Create a new ByteBuffer with a given array of bytes (`Uint8Array`).
 *
 * @constructor
 * @param {Uint8Array} bytes
 */
flatbuffers.ByteBuffer = function(bytes) {
  /**
   * @type {Uint8Array}
   * @private
   */
  this.bytes_ = bytes;

  /**
   * @type {number}
   * @private
   */
  this.position_ = 0;

};

/**
 * A helper function to avoid generated code depending on this file directly.
 *
 * @param {number} low
 * @param {number} high
 * @returns {flatbuffers.Long}
 */
flatbuffers.ByteBuffer.prototype.createLong = function(low, high) {
  return flatbuffers.Long.create(low, high);
};

// Exports for Node.js and RequireJS
this.flatbuffers = flatbuffers;

/// @endcond
/// @}

/**
 * @type {Int32Array}
 * @const
 */
flatbuffers.int32 = new Int32Array(2);

/**
 * @type {Float32Array}
 * @const
 */
flatbuffers.float32 = new Float32Array(flatbuffers.int32.buffer);

/**
 * @type {Float64Array}
 * @const
 */
flatbuffers.float64 = new Float64Array(flatbuffers.int32.buffer);

////////////////////////////////////////////////////////////////////////////////

/**
 * @param {number} offset
 */
flatbuffers.ByteBuffer.prototype.writeInt8 = function(offset, value) {
  this.bytes_[offset] = /** @type {number} */(value);
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeUint8 = function(offset, value) {
  this.bytes_[offset] = value;
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeInt16 = function(offset, value) {
  this.bytes_[offset] = value;
  this.bytes_[offset + 1] = value >> 8;
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeUint16 = function(offset, value) {
    this.bytes_[offset] = value;
    this.bytes_[offset + 1] = value >> 8;
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeInt32 = function(offset, value) {
  this.bytes_[offset] = value;
  this.bytes_[offset + 1] = value >> 8;
  this.bytes_[offset + 2] = value >> 16;
  this.bytes_[offset + 3] = value >> 24;
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeUint32 = function(offset, value) {
    this.bytes_[offset] = value;
    this.bytes_[offset + 1] = value >> 8;
    this.bytes_[offset + 2] = value >> 16;
    this.bytes_[offset + 3] = value >> 24;
};

/**
 * @param {number} offset
 * @param {flatbuffers.Long} value
 */
flatbuffers.ByteBuffer.prototype.writeInt64 = function(offset, value) {
  this.writeInt32(offset, value.low);
  this.writeInt32(offset + 4, value.high);
};

/**
 * @param {number} offset
 * @param {flatbuffers.Long} value
 */
flatbuffers.ByteBuffer.prototype.writeUint64 = function(offset, value) {
    this.writeUint32(offset, value.low);
    this.writeUint32(offset + 4, value.high);
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeFloat32 = function(offset, value) {
  flatbuffers.float32[0] = value;
  this.writeInt32(offset, flatbuffers.int32[0]);
};

/**
 * @param {number} offset
 * @param {number} value
 */
flatbuffers.ByteBuffer.prototype.writeFloat64 = function(offset, value) {
  flatbuffers.float64[0] = value;
  this.writeInt32(offset, flatbuffers.int32[flatbuffers.isLittleEndian ? 0 : 1]);
  this.writeInt32(offset + 4, flatbuffers.int32[flatbuffers.isLittleEndian ? 1 : 0]);
};

 *)
