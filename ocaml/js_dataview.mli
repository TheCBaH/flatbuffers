(** Thin abstraction over JavaScript DataView for FlatBuffer reads.
    Implemented separately for js_of_ocaml and Melange. *)

type t

val length : t -> int
val get : t -> int -> char
val get_int8 : t -> int -> int
val get_uint16_le : t -> int -> int
val get_int16_le : t -> int -> int
val get_int32_le : t -> int -> int32
val get_int64_le : t -> int -> int64
val substring : t -> off:int -> len:int -> string
val of_bytes : bytes -> off:int -> len:int -> t
