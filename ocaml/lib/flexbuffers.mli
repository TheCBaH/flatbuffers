(** Zero-copy FlexBuffers reader and semantic verifier.

    Values retain the supplied backing buffer. The accessors do not perform
    bounds checks; call {!root_verified} or {!verify} before traversing
    untrusted data. *)

type value_type =
  | Null
  | Int
  | UInt
  | Float
  | Key
  | String
  | Indirect_int
  | Indirect_uint
  | Indirect_float
  | Map
  | Vector
  | Vector_int
  | Vector_uint
  | Vector_float
  | Vector_key
  | Vector_string_deprecated
  | Vector_int2
  | Vector_uint2
  | Vector_float2
  | Vector_int3
  | Vector_uint3
  | Vector_float3
  | Vector_int4
  | Vector_uint4
  | Vector_float4
  | Blob
  | Bool
  | Vector_bool

type t

val value_type : t -> value_type
val is_null : t -> bool
val as_bool : t -> bool option
val as_int64 : t -> int64 option
val as_uint64_bits : t -> int64 option
val as_float : t -> float option
val as_string : t -> string option
val as_key : t -> string option

module Blob : sig
  type value = t
  type t

  val length : t -> int
  val get : t -> int -> char
  val to_bytes : t -> bytes
end

val as_blob : t -> Blob.t option

module Vector : sig
  type value = t
  type t

  val length : t -> int
  val element_type : t -> value_type option
  val get : t -> int -> value
  val iter : (value -> unit) -> t -> unit
  val to_list : t -> value list
  val to_array : t -> value array
end

val as_vector : t -> Vector.t option

module Map : sig
  type value = t
  type t

  val length : t -> int
  val keys : t -> Vector.t
  val values : t -> Vector.t
  val find : t -> string -> value option
  val get : t -> int -> string * value
end

val as_map : t -> Map.t option

(** Read the FlexBuffer from [off] (inclusive) through [off + len] (exclusive).
    [len] defaults to the remainder of the supplied buffer. Raises
    [Invalid_argument] if the root trailer itself is malformed. *)
val root : ?off:int -> ?len:int -> 'b Primitives.t -> 'b -> t

type options =
  { max_depth : int
  ; max_values : int
  ; max_apparent_size : int
  ; check_alignment : bool
  ; check_string_terminator : bool
  ; check_utf8 : bool
  ; check_map_order : bool
  }

val default_options : options

type error_kind =
  | Out_of_bounds
  | Arithmetic_overflow
  | Invalid_byte_width of int
  | Invalid_type of int
  | Invalid_offset
  | Invalid_alignment of int
  | Missing_terminator
  | Invalid_utf8
  | Map_length_mismatch
  | Unsorted_map_keys
  | Depth_limit_exceeded
  | Value_limit_exceeded
  | Apparent_size_limit_exceeded

type error = private
  { kind : error_kind
  ; offset : int
  }

val pp_error_kind : Format.formatter -> error_kind -> unit
val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string

(** Verify the FlexBuffer from [off] (inclusive) through [off + len]
    (exclusive). The verifier never raises for malformed input. *)
val verify
  :  ?options:options
  -> ?off:int
  -> ?len:int
  -> 'b Primitives.t
  -> 'b
  -> (unit, error) result

val root_verified
  :  ?options:options
  -> ?off:int
  -> ?len:int
  -> 'b Primitives.t
  -> 'b
  -> (t, error) result
