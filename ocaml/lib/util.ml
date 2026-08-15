#if TARGET_INT_SIZE <> 31 && TARGET_INT_SIZE <> 32 && TARGET_INT_SIZE <> 63
#error "TARGET_INT_SIZE must be 31, 32, or 63"
#endif

let as_signed bits i =
  let shift = TARGET_INT_SIZE - bits in
  (i lsl shift) asr shift
;;

(* [Int32.unsigned_to_int] returns an option. Baseline native and JavaScript
   compilers allocate its [Some] result in this offset-read hot path, so keep
   the same bounds semantics without the intermediate option. *)
#if TARGET_INT_SIZE = 63
let int32_unsigned_to_int n = Int32.to_int n land ((0xFFFF lsl 16) lor 0xFFFF)
#else
let int32_unsigned_max_int = Int32.of_int Stdlib.max_int

let int32_unsigned_to_int n =
  if compare Int32.zero n <= 0 && compare n int32_unsigned_max_int <= 0
  then Int32.to_int n
  else failwith "int32_unsigned_to_int overflow"
#endif
;;

(* common fb mappings when using stdlib types *)
(* module StdlibT = struct *)
(*   type bool = Bool.t *)
(*   type byte = Int.t *)
(*   type ubyte = Char.t *)
(*   type short = Int.t *)
(*   type ushort = Int.t *)
(*   type int = Int32.t *)
(*   type uint = Int32.t *)
(*   type long = Int64.t *)
(*   type ulong = Int64.t *)
(*   type float = Float.t *)
(*   type double = Float.t *)
(* end *)
