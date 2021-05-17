open FlatBuffers_priv

open FlatBuffers

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
