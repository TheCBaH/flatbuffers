open Js_of_ocaml

type t = Typed_array.dataView Js.t

let length (dv : t) =
  (Js.Unsafe.coerce dv)##.byteLength

let get (dv : t) i =
  Char.chr (dv##getUint8 i)

let get_int8 (dv : t) i =
  dv##getInt8 i

let get_uint16_le (dv : t) i =
  dv##getUint16_ i Js._true

let get_int16_le (dv : t) i =
  dv##getInt16_ i Js._true

let get_int32_le (dv : t) i =
  let n = dv##getInt32_ i Js._true in
  Int32.of_float (Js.float_of_number n)

let get_int64_le (dv : t) i =
  let lo = Int64.of_int32 (get_int32_le dv i) in
  let lo = Int64.logand lo 0xFFFFFFFFL in
  let hi = Int64.of_int32 (get_int32_le dv (i + 4)) in
  Int64.logor lo (Int64.shift_left hi 32)

let substring (dv : t) ~off ~len =
  let b = Bytes.create len in
  for j = 0 to len - 1 do
    Bytes.set b j (Char.chr (dv##getUint8 (off + j)))
  done;
  Bytes.unsafe_to_string b

let of_bytes (src : bytes) ~off ~len =
  let ab = new%js Typed_array.arrayBuffer len in
  let u8 = new%js Typed_array.uint8Array_fromBuffer ab in
  for j = 0 to len - 1 do
    Typed_array.set u8 j (Char.code (Bytes.get src (off + j)))
  done;
  new%js Typed_array.dataView ab
