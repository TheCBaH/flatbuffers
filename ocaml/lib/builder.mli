(** Back-to-front construction of FlatBuffers messages. *)

(** An offset identifies a completed object already serialized by a builder.
    Offsets are valid only with the builder and build cycle that created them;
    using one after {!reset} or {!finish}, or with another builder, raises
    [Invalid_argument]. There is no public null/sentinel offset: optional
    references are represented by omitting their table slot. *)
type offset

type t

(** [create ~init_capacity ()] creates an idle builder. The capacity is a hint;
    it grows automatically and is retained when the builder is reused. *)
val create : ?init_capacity:int -> unit -> t

(** Clear an idle builder for reuse. Raises [Invalid_argument] if a table or
    vector is open. *)
val reset : t -> unit

(** Begin a table with field IDs from zero (inclusive) to [n_fields]
    (exclusive). A table cannot be nested inside another table or an open
    vector. *)
val start_table : t -> n_fields:int -> t

(** Finish the open table and return its offset. Raises [Invalid_argument] when
    no table is open. *)
val end_table : t -> offset

(** [finish prim b root] writes the root header, returns the completed buffer,
    and resets [b] for reuse. [root] must have been created by [b] during the
    current build cycle. The builder must be idle. *)
val finish
  :  ?identifier:string
  -> ?size_prefixed:bool
  -> 'a Primitives.t
  -> t
  -> offset
  -> 'a

(** Add scalar, reference, union, and inline-struct fields to the open table.
    Field IDs must be within the count passed to {!start_table}. References must
    identify objects already built by the same builder. A scalar equal to its
    default is omitted. Struct writers may only write within the reserved
    [size]-byte region and should use {!Unsafe.set_scalar} and
    {!Unsafe.set_padding}. All misuse is reported with [Invalid_argument]. *)
val push_slot_scalar : 'a Primitives.ty -> int -> 'a -> t -> t

val push_slot_scalar_default : 'a Primitives.ty -> int -> default:'a -> 'a -> t -> t
val push_slot_ref : int -> offset -> t -> t
val push_slot_ref64 : int -> offset -> t -> t
val push_slot_union : int -> int -> Primitives.T.ubyte -> offset -> t -> t
val push_slot_struct : (t -> int -> 'a -> unit) -> int -> int -> int -> 'a -> t -> t

(** Create complete vectors and strings while the builder is idle. Reference
    arrays must contain completed-object offsets previously returned by the
    same builder in its current build cycle; all offsets are validated before
    the builder moves. Repeated offsets are allowed and element order is
    preserved. The [size] supplied to a struct-vector writer is the exact
    writable size of each element. *)
val create_vector : 'a Primitives.ty -> t -> 'a array -> offset

val create_vector_ref : t -> offset array -> offset
val create_vector_ref64 : t -> offset array -> offset

(** [create_union_vector tag_type b tags values] creates the parallel tag and
    value vectors required by a vector of unions and returns them in that
    order. [None] is the internal null payload for a [NONE] tag. Tag/value
    lengths and every present offset are validated before the builder moves. *)
val create_union_vector
  :  'a Primitives.ty
  -> t
  -> 'a array
  -> offset option array
  -> offset * offset

val create_vector64 : 'a Primitives.ty -> t -> 'a array -> offset

val create_vector64_struct
  :  (t -> int -> 'a -> unit)
  -> size:int
  -> t
  -> 'a array
  -> offset

val create_vector_struct : (t -> int -> 'a -> unit) -> size:int -> t -> 'a array -> offset

(** Serialize one standalone struct so it can be referenced by a union. The
    writer receives a reserved [size]-byte region at setter position zero.
    [size] must be positive and [align] must be a positive power of two. *)
val create_struct : (t -> int -> 'a -> unit) -> size:int -> align:int -> t -> 'a -> offset

val create_string : t -> string -> offset
val create_shared_string : t -> string -> offset
val create_nested_vector : t -> bytes -> offset

(** Low-level operations used by generated struct writers and custom code.

    These operations expose the builder's backwards-growing storage model and
    are less stable than the constructors above. A call that reserves storage
    may grow the backing buffer, so positions are meaningful only as indices
    passed to setters during the current callback or open-vector operation.
    Setters do not bounds-check the writable region. Misuse can corrupt the
    message even though invalid builder state and invalid sizes are rejected. *)
module Unsafe : sig
  (** [reserve ~align ~bytes b] reserves and zero-aligns a writable region,
      advances the builder, and returns the region's first setter position.
      It is valid only while the builder is idle or a table is open. *)
  val reserve : align:int -> bytes:int -> t -> int

  (** Start/end manual vector construction. [start_vector] reserves the entire
      payload; fill it from setter position zero up to [n_elts * elt_size]
      (exclusive) without otherwise advancing the builder, then call the
      matching [end]. *)
  val start_vector : t -> n_elts:int -> elt_size:int -> unit

  val end_vector : t -> offset
  val start_vector64 : t -> n_elts:int -> elt_size:int -> unit
  val end_vector64 : t -> offset

  (** Current backwards offset. It is primarily useful for diagnostics and
      specialized generators; it is not a substitute for a constructor's
      returned offset. *)
  val current_offset : t -> offset

  (** Write within storage already reserved by a constructor, [reserve], or a
      manual vector start. Positions are relative to the beginning of the
      current reserved region. *)
  val set_scalar : 'a Primitives.ty -> t -> int -> 'a -> unit

  val set_uoffset : t -> int -> offset -> unit
  val set_uoffset64 : t -> int -> offset -> unit
  val set_string : t -> int -> string -> unit
  val set_padding : t -> int -> int -> unit
end

(** {1 Deprecated low-level compatibility aliases}

    These names predate {!Unsafe}. New code should use complete constructors or
    the corresponding [Unsafe] operation. [save_slot] has no low-level
    replacement; use a [push_slot_*] operation so reservation, writing, and
    vtable bookkeeping remain atomic. *)

val start_vector : t -> n_elts:int -> elt_size:int -> unit
[@@deprecated "Use Builder.Unsafe.start_vector"]

val end_vector : t -> offset [@@deprecated "Use Builder.Unsafe.end_vector"]

val save_slot : id:int -> t -> unit
[@@deprecated "Use the appropriate Builder.push_slot_* operation"]

val prep : align:int -> bytes:int -> t -> unit [@@deprecated "Use Builder.Unsafe.reserve"]
val current_offset : t -> offset [@@deprecated "Use Builder.Unsafe.current_offset"]

val set_scalar : 'a Primitives.ty -> t -> int -> 'a -> unit
[@@deprecated "Use Builder.Unsafe.set_scalar"]

val set_uoffset : t -> int -> offset -> unit
[@@deprecated "Use Builder.Unsafe.set_uoffset"]

val set_uoffset64 : t -> int -> offset -> unit
[@@deprecated "Use Builder.Unsafe.set_uoffset64"]

val set_string : t -> int -> string -> unit [@@deprecated "Use Builder.Unsafe.set_string"]
val set_padding : t -> int -> int -> unit [@@deprecated "Use Builder.Unsafe.set_padding"]
