module T = struct
  type bool = Bool.t
  type byte = Int.t
  type ubyte = Char.t
  type short = Int.t
  type ushort = Int.t
  type int = Int32.t
  type uint = Int32.t
  type long = Int64.t
  type ulong = Int64.t
  type float = Float.t
  type double = Float.t
end

type _ t =
  | Bytes : bytes t
  | String : string t
#ifdef BIGSTRING
  | Bigstring : Bigstringaf.t t
#endif
#ifdef JSDATAVIEW
  | JsDataView : Js_dataview.t t
#endif

let buf_of_bytes (type b) (prim : b t) b ~off ~len : b =
  match prim with
  | Bytes -> Bytes.sub b off len
  | String -> Bytes.sub_string b off len
#ifdef BIGSTRING
  | Bigstring ->
    let buf = Bigstringaf.create len in
    Bigstringaf.blit_from_bytes b ~src_off:off buf ~dst_off:0 ~len;
    buf
#endif
#ifdef JSDATAVIEW
  | JsDataView -> Js_dataview.of_bytes b ~off ~len
#endif
;;

let[@inline] length (type b) (prim : b t) (b : b) =
  match prim with
  | Bytes -> Bytes.length b
  | String -> String.length b
#ifdef BIGSTRING
  | Bigstring -> Bigstringaf.length b
#endif
#ifdef JSDATAVIEW
  | JsDataView -> Js_dataview.length b
#endif
;;

let[@inline] get_string (type b) (prim : b t) (b : b) ~off ~len =
  match prim with
  | Bytes -> Bytes.sub_string b off len
  | String -> String.sub b off len
#ifdef BIGSTRING
  | Bigstring -> Bigstringaf.substring b ~off ~len
#endif
#ifdef JSDATAVIEW
  | JsDataView -> Js_dataview.substring b ~off ~len
#endif
;;

(* Melange's JavaScript byte primitives always use little-endian layout, but
   its generated [Sys.big_endian] value reflects the architecture on which the
   Melange compiler itself was built.  On s390x this makes the standard
   [Bytes.get_*_le]/[set_*_le] and [String.get_*_le] functions swap values that
   are already little-endian.  Keep the native intrinsics on other backends and
   spell out the wire order for Melange so generated JavaScript is independent
   of the compiler host. *)
#ifdef MELANGE
let[@inline] check_bytes_bounds b i width =
  if i < 0 || i > Bytes.length b - width then invalid_arg "index out of bounds"
;;

let[@inline] check_string_bounds b i width =
  if i < 0 || i > String.length b - width then invalid_arg "index out of bounds"
;;

let[@inline] get_bytes_uint16_le b i =
  check_bytes_bounds b i 2;
  Char.code (Bytes.unsafe_get b i) lor (Char.code (Bytes.unsafe_get b (i + 1)) lsl 8)
;;

let[@inline] get_string_uint16_le b i =
  check_string_bounds b i 2;
  Char.code (String.unsafe_get b i) lor (Char.code (String.unsafe_get b (i + 1)) lsl 8)
;;

let[@inline] get_bytes_int32_le b i =
  check_bytes_bounds b i 4;
  let byte n = Int32.of_int (Char.code (Bytes.unsafe_get b (i + n))) in
  Int32.logor
    (byte 0)
    (Int32.logor
       (Int32.shift_left (byte 1) 8)
       (Int32.logor (Int32.shift_left (byte 2) 16) (Int32.shift_left (byte 3) 24)))
;;

let[@inline] get_string_int32_le b i =
  check_string_bounds b i 4;
  let byte n = Int32.of_int (Char.code (String.unsafe_get b (i + n))) in
  Int32.logor
    (byte 0)
    (Int32.logor
       (Int32.shift_left (byte 1) 8)
       (Int32.logor (Int32.shift_left (byte 2) 16) (Int32.shift_left (byte 3) 24)))
;;

let[@inline] get_bytes_int64_le b i =
  let lo = Int64.logand (Int64.of_int32 (get_bytes_int32_le b i)) 0xFFFFFFFFL in
  let hi = Int64.of_int32 (get_bytes_int32_le b (i + 4)) in
  Int64.logor lo (Int64.shift_left hi 32)
;;

let[@inline] get_string_int64_le b i =
  let lo = Int64.logand (Int64.of_int32 (get_string_int32_le b i)) 0xFFFFFFFFL in
  let hi = Int64.of_int32 (get_string_int32_le b (i + 4)) in
  Int64.logor lo (Int64.shift_left hi 32)
;;

let[@inline] set_int16_le b i x =
  check_bytes_bounds b i 2;
  Bytes.unsafe_set b i (Char.chr (x land 0xFF));
  Bytes.unsafe_set b (i + 1) (Char.chr ((x lsr 8) land 0xFF))
;;

let[@inline] set_int32_le b i x =
  check_bytes_bounds b i 4;
  let byte n = Int32.to_int (Int32.logand (Int32.shift_right_logical x n) 0xFFl) in
  Bytes.unsafe_set b i (Char.chr (byte 0));
  Bytes.unsafe_set b (i + 1) (Char.chr (byte 8));
  Bytes.unsafe_set b (i + 2) (Char.chr (byte 16));
  Bytes.unsafe_set b (i + 3) (Char.chr (byte 24))
;;

let[@inline] set_int64_le b i x =
  check_bytes_bounds b i 8;
  let byte n = Int64.to_int (Int64.logand (Int64.shift_right_logical x n) 0xFFL) in
  Bytes.unsafe_set b i (Char.chr (byte 0));
  Bytes.unsafe_set b (i + 1) (Char.chr (byte 8));
  Bytes.unsafe_set b (i + 2) (Char.chr (byte 16));
  Bytes.unsafe_set b (i + 3) (Char.chr (byte 24));
  Bytes.unsafe_set b (i + 4) (Char.chr (byte 32));
  Bytes.unsafe_set b (i + 5) (Char.chr (byte 40));
  Bytes.unsafe_set b (i + 6) (Char.chr (byte 48));
  Bytes.unsafe_set b (i + 7) (Char.chr (byte 56))
;;
#else
#ifdef ARCH_s390x
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let[@inline] get_bytes_uint16_le b i = swap16 (Bytes.get_uint16_ne b i)
let[@inline] get_string_uint16_le b i = swap16 (String.get_uint16_ne b i)
let[@inline] get_bytes_int32_le b i = swap32 (Bytes.get_int32_ne b i)
let[@inline] get_string_int32_le b i = swap32 (String.get_int32_ne b i)
let[@inline] get_bytes_int64_le b i = swap64 (Bytes.get_int64_ne b i)
let[@inline] get_string_int64_le b i = swap64 (String.get_int64_ne b i)
let[@inline] set_int16_le b i x = Bytes.set_int16_ne b i (swap16 x)
let[@inline] set_int32_le b i x = Bytes.set_int32_ne b i (swap32 x)
let[@inline] set_int64_le b i x = Bytes.set_int64_ne b i (swap64 x)
#else
let[@inline] get_bytes_uint16_le b i = Bytes.get_uint16_ne b i
let[@inline] get_string_uint16_le b i = String.get_uint16_ne b i
let[@inline] get_bytes_int32_le b i = Bytes.get_int32_ne b i
let[@inline] get_string_int32_le b i = String.get_int32_ne b i
let[@inline] get_bytes_int64_le b i = Bytes.get_int64_ne b i
let[@inline] get_string_int64_le b i = String.get_int64_ne b i
let[@inline] set_int16_le b i x = Bytes.set_int16_ne b i x
let[@inline] set_int32_le b i x = Bytes.set_int32_ne b i x
let[@inline] set_int64_le b i x = Bytes.set_int64_ne b i x
#endif
#endif

let[@inline] get_int16_le b i = Util.as_signed 16 (get_bytes_uint16_le b i)

let[@inline] get_uoffset (type b) (prim : b t) (b : b) i =
  let i =
    match prim with
    | Bytes -> get_bytes_int32_le b i
    | String -> get_string_int32_le b i
#ifdef BIGSTRING
    | Bigstring -> Bigstringaf.get_int32_le b i
#endif
#ifdef JSDATAVIEW
    | JsDataView -> Js_dataview.get_int32_le b i
#endif
  in
  (* note: flambda/closure both sensitive to where this call is. Calling in
     each match branch or match exp as argument both cause extra allocation. *)
  Util.int32_unsigned_to_int i
;;

let[@inline] get_uoffset64 (type b) (prim : b t) (b : b) i =
  let i =
    match prim with
    | Bytes -> get_bytes_int64_le b i
    | String -> get_string_int64_le b i
#ifdef BIGSTRING
    | Bigstring -> Bigstringaf.get_int64_le b i
#endif
#ifdef JSDATAVIEW
    | JsDataView -> Js_dataview.get_int64_le b i
#endif
  in
  Int64.to_int i
;;

let[@inline] get_voffset (type b) (prim : b t) (b : b) i =
  match prim with
  | Bytes -> get_bytes_uint16_le b i
  | String -> get_string_uint16_le b i
#ifdef BIGSTRING
  | Bigstring -> Bigstringaf.get_int16_le b i
#endif
#ifdef JSDATAVIEW
  | JsDataView -> Js_dataview.get_uint16_le b i
#endif
;;

let[@inline] get_soffset (type b) (prim : b t) (b : b) i =
  let i =
    match prim with
    | Bytes -> get_bytes_int32_le b i
    | String -> get_string_int32_le b i
#ifdef BIGSTRING
    | Bigstring -> Bigstringaf.get_int32_le b i
#endif
#ifdef JSDATAVIEW
    | JsDataView -> Js_dataview.get_int32_le b i
#endif
  in
  Int32.to_int i
;;

type _ ty =
  | TBool : T.bool ty
  | TByte : T.byte ty
  | TUByte : T.ubyte ty
  | TShort : T.short ty
  | TUShort : T.ushort ty
  | TInt : T.int ty
  | TUInt : T.uint ty
  | TLong : T.long ty
  | TULong : T.ulong ty
  | TFloat : T.float ty
  | TDouble : T.double ty

let[@inline] size_scalar (type a) : a ty -> int = function
  | TBool | TByte | TUByte -> 1
  | TShort | TUShort -> 2
  | TInt | TUInt | TFloat -> 4
  | TLong | TULong | TDouble -> 8
;;

let[@inline] get_scalar (type a b) (t : a ty) (prim : b t) (b : b) (i : int) : a =
  match t, prim with
  | TBool, Bytes -> Bytes.get b i == '\001'
  | TBool, String -> String.get b i == '\001'
#ifdef BIGSTRING
  | TBool, Bigstring -> Bigstringaf.get b i == '\001'
#endif
#ifdef JSDATAVIEW
  | TBool, JsDataView -> Js_dataview.get b i == '\001'
#endif
  | TByte, Bytes -> Bytes.get_int8 b i
  | TByte, String -> String.get_int8 b i
#ifdef BIGSTRING
  | TByte, Bigstring -> Util.as_signed 8 (Char.code (Bigstringaf.get b i))
#endif
#ifdef JSDATAVIEW
  | TByte, JsDataView -> Js_dataview.get_int8 b i
#endif
  | TUByte, Bytes -> Bytes.get b i
  | TUByte, String -> String.get b i
#ifdef BIGSTRING
  | TUByte, Bigstring -> Bigstringaf.get b i
#endif
#ifdef JSDATAVIEW
  | TUByte, JsDataView -> Js_dataview.get b i
#endif
  | TShort, Bytes -> Util.as_signed 16 (get_bytes_uint16_le b i)
  | TShort, String -> Util.as_signed 16 (get_string_uint16_le b i)
#ifdef BIGSTRING
  | TShort, Bigstring -> Util.as_signed 16 (Bigstringaf.get_int16_le b i)
#endif
#ifdef JSDATAVIEW
  | TShort, JsDataView -> Js_dataview.get_int16_le b i
#endif
  | TUShort, Bytes -> get_bytes_uint16_le b i
  | TUShort, String -> get_string_uint16_le b i
#ifdef BIGSTRING
  | TUShort, Bigstring -> Bigstringaf.get_int16_le b i
#endif
#ifdef JSDATAVIEW
  | TUShort, JsDataView -> Js_dataview.get_uint16_le b i
#endif
  | TInt, Bytes -> get_bytes_int32_le b i
  | TInt, String -> get_string_int32_le b i
#ifdef BIGSTRING
  | TInt, Bigstring -> Bigstringaf.get_int32_le b i
#endif
#ifdef JSDATAVIEW
  | TInt, JsDataView -> Js_dataview.get_int32_le b i
#endif
  | TUInt, Bytes -> get_bytes_int32_le b i
  | TUInt, String -> get_string_int32_le b i
#ifdef BIGSTRING
  | TUInt, Bigstring -> Bigstringaf.get_int32_le b i
#endif
#ifdef JSDATAVIEW
  | TUInt, JsDataView -> Js_dataview.get_int32_le b i
#endif
  | TLong, Bytes -> get_bytes_int64_le b i
  | TLong, String -> get_string_int64_le b i
#ifdef BIGSTRING
  | TLong, Bigstring -> Bigstringaf.get_int64_le b i
#endif
#ifdef JSDATAVIEW
  | TLong, JsDataView -> Js_dataview.get_int64_le b i
#endif
  | TULong, Bytes -> get_bytes_int64_le b i
  | TULong, String -> get_string_int64_le b i
#ifdef BIGSTRING
  | TULong, Bigstring -> Bigstringaf.get_int64_le b i
#endif
#ifdef JSDATAVIEW
  | TULong, JsDataView -> Js_dataview.get_int64_le b i
#endif
  | TFloat, Bytes -> Int32.float_of_bits (get_bytes_int32_le b i)
  | TFloat, String -> Int32.float_of_bits (get_string_int32_le b i)
#ifdef BIGSTRING
  | TFloat, Bigstring -> Int32.float_of_bits (Bigstringaf.get_int32_le b i)
#endif
#ifdef JSDATAVIEW
  | TFloat, JsDataView -> Int32.float_of_bits (Js_dataview.get_int32_le b i)
#endif
  | TDouble, Bytes -> Int64.float_of_bits (get_bytes_int64_le b i)
  | TDouble, String -> Int64.float_of_bits (get_string_int64_le b i)
#ifdef BIGSTRING
  | TDouble, Bigstring -> Int64.float_of_bits (Bigstringaf.get_int64_le b i)
#endif
#ifdef JSDATAVIEW
  | TDouble, JsDataView -> Int64.float_of_bits (Js_dataview.get_int64_le b i)
#endif
;;

let[@inline] set_scalar (type a) (t : a ty) b i (x : a) =
  match t with
  | TBool -> Bytes.set_int8 b i (if x then 1 else 0)
  | TByte -> Bytes.set_int8 b i x
  | TUByte -> Bytes.set_int8 b i (Char.code x)
  | TShort -> set_int16_le b i x
  | TUShort -> set_int16_le b i x
  | TInt -> set_int32_le b i x
  | TUInt -> set_int32_le b i x
  | TLong -> set_int64_le b i x
  | TULong -> set_int64_le b i x
  | TFloat -> set_int32_le b i (Int32.bits_of_float x)
  | TDouble -> set_int64_le b i (Int64.bits_of_float x)
;;

let of_default_bool = Fun.id
let of_default_byte = Int64.to_int
let of_default_ubyte x = Char.chr (Int64.to_int x)
let of_default_short = Int64.to_int
let of_default_ushort = Int64.to_int
let of_default_int = Int64.to_int32
let of_default_uint = Int64.to_int32
let of_default_long = Fun.id
let of_default_ulong = Fun.id
let of_default_float = Fun.id
let of_default_double = Fun.id
let to_default_bool = Fun.id
let to_default_byte = Int64.of_int
let to_default_ubyte x = Int64.of_int (Char.code x)
let to_default_short = Int64.of_int
let to_default_ushort = Int64.of_int
let to_default_int = Int64.of_int32
let to_default_uint = Int64.of_int32
let to_default_long = Fun.id
let to_default_ulong = Fun.id
let to_default_float = Fun.id
let to_default_double = Fun.id
