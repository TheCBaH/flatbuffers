(** Structural verification of untrusted FlatBuffers.

    The zero-copy readers in {!Read} and {!Runtime} perform no bounds checking:
    they assume the buffer was produced by a well-behaved encoder. This module
    provides an opt-in, single-pass structural verifier that proves a buffer is
    safe to traverse with those readers.

    Generated code emits one internal callback per table and one dispatcher per
    union, plus [verify] / [root_verified] entry points on the schema root
    table. The values below marked {e generated code only} are the callback
    surface used by that generated code; they are not intended for hand-written
    use, but they are exposed so generated modules can be compiled without a
    privileged interface.

    Verification never raises for malformed input: every read is range-checked
    before it happens, and all offset/length arithmetic is overflow-checked. *)

(** {1 Options} *)

type options =
  { max_depth : int (** Maximum nesting of tables/vectors. Upstream default: 64. *)
  ; max_tables : int (** Maximum number of table visits. Upstream default: 1_000_000. *)
  ; max_apparent_size : int
    (** Maximum total number of bytes {e apparently} visited. Shared
            sub-objects in a DAG are counted once per visit, so this bounds
            expansion attacks that [max_tables] alone does not. *)
  ; check_alignment : bool (** Verify natural alignment of every read. *)
  ; check_string_terminator : bool
    (** Verify the NUL byte that follows string contents. *)
  ; check_nested_flatbuffers : bool
    (** Recurse into fields annotated [nested_flatbuffer]. When false, only
            the containing byte vector is checked. *)
  ; reject_unknown_union_tags : bool
    (** When false (default, matching upstream), a union discriminator that
            is not known to this schema is accepted after structural checks;
            its payload is not traversed. When true, it is rejected with
            {!Unknown_union_tag}. *)
  }

val default_options : options

(** {1 Errors} *)

type path_element =
  | Field of string
  | Index of int
  | Union_variant of string
  | Nested_buffer

type error_kind =
  | Out_of_bounds of
      { length : int
      ; region_end : int
      }
  | Arithmetic_overflow
  | Invalid_offset
  | Invalid_vtable
  | Invalid_alignment of { alignment : int }
  | Missing_required_field
  | Missing_string_terminator
  | Invalid_identifier of
      { expected : string
      ; actual : string option
      }
  | Invalid_size_prefix
  | Inconsistent_union
  | Unknown_union_tag of int64
  | Depth_limit_exceeded
  | Table_limit_exceeded
  | Apparent_size_limit_exceeded

type error = private
  { kind : error_kind
  ; offset : int (** absolute byte offset in the supplied buffer *)
  ; path : path_element list (** root-to-leaf schema path *)
  }

val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string
val pp_error_kind : Format.formatter -> error_kind -> unit
val pp_path : Format.formatter -> path_element list -> unit

(** {1 Traversal state}

    {e Generated code only.} *)

type t

(** Verifier for one table, given the absolute position of the table. *)
type table_fn = t -> int -> bool

(** Verifier for one union payload, given the discriminator value and the
    absolute position of the slot holding the payload offset. *)
type union_fn = t -> int64 -> int -> bool

(** {1 Entry points} *)

val verify_root
  :  ?options:options
  -> ?size_prefixed:bool
  -> ?off:int
  -> ?identifier:string
  -> 'b Primitives.t
  -> 'b
  -> table_fn
  -> (unit, error) result

(** {1 Table structure}

    {e Generated code only.} *)

val enter_table : t -> int -> bool
val exit_table : t -> bool -> bool

(** {1 Fields}

    {e Generated code only.} Each looks the field up in the vtable of the table
    most recently opened with {!enter_table}, so they must only be called
    between a successful [enter_table] and the matching {!exit_table}. *)

val field_inline
  :  t
  -> name:string
  -> voff:int
  -> size:int
  -> align:int
  -> required:bool
  -> bool

val field_string : t -> name:string -> voff:int -> required:bool -> off64:bool -> bool

val field_table
  :  t
  -> name:string
  -> voff:int
  -> required:bool
  -> off64:bool
  -> table_fn
  -> bool

val field_vector
  :  t
  -> name:string
  -> voff:int
  -> required:bool
  -> off64:bool
  -> vec64:bool
  -> elem_size:int
  -> bool

val field_vector_string
  :  t
  -> name:string
  -> voff:int
  -> required:bool
  -> off64:bool
  -> vec64:bool
  -> bool

val field_vector_table
  :  t
  -> name:string
  -> voff:int
  -> required:bool
  -> off64:bool
  -> vec64:bool
  -> table_fn
  -> bool

val field_nested_buffer
  :  t
  -> name:string
  -> voff:int
  -> required:bool
  -> off64:bool
  -> vec64:bool
  -> table_fn
  -> bool

val field_union
  :  t
  -> name:string
  -> type_voff:int
  -> voff:int
  -> required:bool
  -> tag_size:int
  -> union_fn
  -> bool

val field_union_vector
  :  t
  -> name:string
  -> type_voff:int
  -> voff:int
  -> required:bool
  -> tag_size:int
  -> union_fn
  -> bool

(** {1 Union payloads}

    {e Generated code only.} Called from generated union dispatchers with the
    position of the slot holding the payload offset. *)

val union_table : t -> int -> variant:string -> table_fn -> bool
val union_string : t -> int -> variant:string -> bool
val union_struct : t -> int -> variant:string -> size:int -> align:int -> bool
val union_none : t -> int -> bool
val union_unknown : t -> int64 -> int -> bool
