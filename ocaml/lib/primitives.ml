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
;;

let[@inline] get_string (type b) (prim : b t) (b : b) ~off ~len =
  match prim with
  | Bytes -> Bytes.sub_string b off len
  | String -> String.sub b off len
#ifdef BIGSTRING
  | Bigstring -> Bigstringaf.substring b ~off ~len
#endif
;;

let[@inline] get_uoffset (type b) (prim : b t) (b : b) i =
  let i =
    match prim with
    | Bytes -> Bytes.get_int32_le b i
    | String -> String.get_int32_le b i
#ifdef BIGSTRING
    | Bigstring -> Bigstringaf.get_int32_le b i
#endif
  in
  (* note: flambda/closure both sensitive to where this call is. Calling in
     each match branch or match exp as argument both cause extra allocation. *)
  Util.int32_unsigned_to_int i
;;

let[@inline] get_uoffset64 (type b) (prim : b t) (b : b) i =
  let i =
    match prim with
    | Bytes -> Bytes.get_int64_le b i
    | String -> String.get_int64_le b i
#ifdef BIGSTRING
    | Bigstring -> Bigstringaf.get_int64_le b i
#endif
  in
  Int64.to_int i
;;

let[@inline] get_voffset (type b) (prim : b t) (b : b) i =
  match prim with
  | Bytes -> Bytes.get_uint16_le b i
  | String -> String.get_uint16_le b i
#ifdef BIGSTRING
  | Bigstring -> Bigstringaf.get_int16_le b i
#endif
;;

let[@inline] get_soffset (type b) (prim : b t) (b : b) i =
  let i =
    match prim with
    | Bytes -> Bytes.get_int32_le b i
    | String -> String.get_int32_le b i
#ifdef BIGSTRING
    | Bigstring -> Bigstringaf.get_int32_le b i
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
  | TByte, Bytes -> Bytes.get_int8 b i
  | TByte, String -> String.get_int8 b i
#ifdef BIGSTRING
  | TByte, Bigstring -> Util.as_signed 8 (Char.code (Bigstringaf.get b i))
#endif
  | TUByte, Bytes -> Bytes.get b i
  | TUByte, String -> String.get b i
#ifdef BIGSTRING
  | TUByte, Bigstring -> Bigstringaf.get b i
#endif
  | TShort, Bytes -> Bytes.get_int16_le b i
  | TShort, String -> String.get_int16_le b i
#ifdef BIGSTRING
  | TShort, Bigstring -> Util.as_signed 16 (Bigstringaf.get_int16_le b i)
#endif
  | TUShort, Bytes -> Bytes.get_uint16_le b i
  | TUShort, String -> String.get_uint16_le b i
#ifdef BIGSTRING
  | TUShort, Bigstring -> Bigstringaf.get_int16_le b i
#endif
  | TInt, Bytes -> Bytes.get_int32_le b i
  | TInt, String -> String.get_int32_le b i
#ifdef BIGSTRING
  | TInt, Bigstring -> Bigstringaf.get_int32_le b i
#endif
  | TUInt, Bytes -> Bytes.get_int32_le b i
  | TUInt, String -> String.get_int32_le b i
#ifdef BIGSTRING
  | TUInt, Bigstring -> Bigstringaf.get_int32_le b i
#endif
  | TLong, Bytes -> Bytes.get_int64_le b i
  | TLong, String -> String.get_int64_le b i
#ifdef BIGSTRING
  | TLong, Bigstring -> Bigstringaf.get_int64_le b i
#endif
  | TULong, Bytes -> Bytes.get_int64_le b i
  | TULong, String -> String.get_int64_le b i
#ifdef BIGSTRING
  | TULong, Bigstring -> Bigstringaf.get_int64_le b i
#endif
  | TFloat, Bytes -> Int32.float_of_bits (Bytes.get_int32_le b i)
  | TFloat, String -> Int32.float_of_bits (String.get_int32_le b i)
#ifdef BIGSTRING
  | TFloat, Bigstring -> Int32.float_of_bits (Bigstringaf.get_int32_le b i)
#endif
  | TDouble, Bytes -> Int64.float_of_bits (Bytes.get_int64_le b i)
  | TDouble, String -> Int64.float_of_bits (String.get_int64_le b i)
#ifdef BIGSTRING
  | TDouble, Bigstring -> Int64.float_of_bits (Bigstringaf.get_int64_le b i)
#endif
;;

let[@inline] set_scalar (type a) (t : a ty) b i (x : a) =
  match t with
  | TBool -> Bytes.set_int8 b i (if x then 1 else 0)
  | TByte -> Bytes.set_int8 b i x
  | TUByte -> Bytes.set_int8 b i (Char.code x)
  | TShort -> Bytes.set_int16_le b i x
  | TUShort -> Bytes.set_int16_le b i x
  | TInt -> Bytes.set_int32_le b i x
  | TUInt -> Bytes.set_int32_le b i x
  | TLong -> Bytes.set_int64_le b i x
  | TULong -> Bytes.set_int64_le b i x
  | TFloat -> Bytes.set_int32_le b i (Int32.bits_of_float x)
  | TDouble -> Bytes.set_int64_le b i (Int64.bits_of_float x)
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
