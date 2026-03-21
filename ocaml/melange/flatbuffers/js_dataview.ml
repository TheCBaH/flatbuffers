type t = Js.Typed_array.DataView.t

let length (dv : t) =
  Js.Typed_array.DataView.byteLength dv

let get (dv : t) i =
  Char.chr (Js.Typed_array.DataView.getUint8 i dv)

let get_int8 (dv : t) i =
  Js.Typed_array.DataView.getInt8 i dv

let get_uint16_le (dv : t) i =
  Js.Typed_array.DataView.getUint16LittleEndian i dv

let get_int16_le (dv : t) i =
  Js.Typed_array.DataView.getInt16LittleEndian i dv

let get_int32_le (dv : t) i =
  Int32.of_int (Js.Typed_array.DataView.getInt32LittleEndian i dv)

let get_int64_le (dv : t) i =
  let lo = Int64.of_int32 (get_int32_le dv i) in
  let lo = Int64.logand lo 0xFFFFFFFFL in
  let hi = Int64.of_int32 (get_int32_le dv (i + 4)) in
  Int64.logor lo (Int64.shift_left hi 32)

let substring (dv : t) ~off ~len =
  let b = Bytes.create len in
  for j = 0 to len - 1 do
    Bytes.set b j (Char.chr (Js.Typed_array.DataView.getUint8 (off + j) dv))
  done;
  Bytes.unsafe_to_string b

let of_bytes (src : bytes) ~off ~len =
  let ab = Js.Typed_array.ArrayBuffer.make len in
  let u8 = Js.Typed_array.Uint8Array.fromBuffer ab () in
  for j = 0 to len - 1 do
    Js.Typed_array.Uint8Array.unsafe_set u8 j (Char.code (Bytes.get src (off + j)))
  done;
  Js.Typed_array.DataView.make ab
