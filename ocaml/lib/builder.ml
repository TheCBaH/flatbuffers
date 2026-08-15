(** FlatBuffers are constructed back-to-front. [index] is relative to the end
    of the buffer; the remaining fields prevent an offset from escaping its
    builder, build cycle, or completed-object constructor. *)
type offset =
  { index : int
  ; owner : bytes ref
  ; cycle : unit ref
  ; object_offset : bool
  }

(* dynamic array of offsets, sorted by a compare function. Used to dedupe vtables *)
module IndCache = struct
  type t =
    { mutable buf : int array
    ; mutable length : int
    ; compare : int -> int -> int
    }

  let make n compare = { buf = Array.make n (-1); length = 0; compare }

  let grow s =
    let new_len = Array.length s.buf * 2 in
    let new_buf = Array.make new_len 0 in
    Array.blit s.buf 0 new_buf 0 s.length;
    s.buf <- new_buf
  ;;

  let find s x =
    let rec loop s x lo hi =
      if hi <= lo
      then -(lo + 1)
      else (
        let mid = (lo + hi) / 2 in
        let cmp = s.compare x s.buf.(mid) in
        if cmp == 0
        then mid
        else if cmp < 0
        then loop s x lo mid
        else loop s x (mid + 1) hi)
    in
    loop s x 0 s.length
  ;;

  let insert s i x =
    if s.length == Array.length s.buf then grow s;
    Array.blit s.buf i s.buf (i + 1) (s.length - i);
    s.buf.(i) <- x;
    s.length <- s.length + 1
  ;;

  let find_or_insert s x =
    let ind = find s x in
    if ind < 0
    then (
      insert s (-ind - 1) x;
      x)
    else s.buf.(ind)
  ;;

  let reset s = s.length <- 0
end

let compare_vtable_offsets b o p =
  let b = !b in
  let o = Bytes.length b - o in
  let p = Bytes.length b - p in
  let leno = Primitives.get_int16_le b o in
  let lenp = Primitives.get_int16_le b p in
  let cmp = Int.compare leno lenp in
  if cmp != 0
  then cmp
  else (
    let cmp = ref 0 in
    let i = ref 2 in
    while !i < leno && !cmp == 0 do
      cmp := Char.compare (Bytes.get b (o + !i)) (Bytes.get b (p + !i));
      incr i
    done;
    !cmp)
;;

type table_state =
  { start : int
  ; n_fields : int
  }

type vector_state =
  { start : int
  ; prefix_size : int
  ; n_elts : int
  ; elt_size : int
  }

type state =
  | Idle
  | Table of table_state
  | Vector of vector_state

type t =
  { buf : bytes ref
  ; mutable cycle : unit ref
  ; mutable length : int
  ; mutable cur_vtable : int array
  ; mutable cur_vtable_len : int
  ; mutable minalign : int
  ; mutable state : state
  ; strings : (string, offset) Hashtbl.t
  ; vtables : IndCache.t
  }

let create ?(init_capacity = 1024) () =
  let buf = ref (Bytes.create (Int.max init_capacity 16)) in
  { buf
  ; cycle = ref ()
  ; length = 0
  ; cur_vtable = [||]
  ; cur_vtable_len = 0
  ; minalign = 1
  ; state = Idle
  ; strings = Hashtbl.create 0
  ; vtables = IndCache.make 16 (compare_vtable_offsets buf)
  }
;;

let invalid_state name expected state =
  let actual =
    match state with
    | Idle -> "idle"
    | Table _ -> "building a table"
    | Vector { prefix_size = 4; _ } -> "building a 32-bit vector"
    | Vector _ -> "building a 64-bit vector"
  in
  invalid_arg
    (Printf.sprintf "Builder.%s: expected %s, but builder is %s" name expected actual)
;;

let require_idle name b =
  match b.state with
  | Idle -> ()
  | state -> invalid_state name "an idle builder" state
;;

let reset_unchecked b =
  b.cycle <- ref ();
  b.length <- 0;
  b.cur_vtable_len <- 0;
  b.minalign <- 1;
  b.state <- Idle;
  Hashtbl.reset b.strings;
  IndCache.reset b.vtables
;;

(* TODO: option to shrink buffer? *)
let reset b =
  require_idle "reset" b;
  reset_unchecked b
;;

let invalid_size name message = invalid_arg ("Builder." ^ name ^ ": " ^ message)

let checked_add_size name a b =
  if a < 0 || b < 0 || a > max_int - b then invalid_size name "size overflow";
  a + b
;;

let ensure_capacity b n =
  let old_len = Bytes.length !(b.buf) in
  if old_len < n
  then (
    let new_len = ref old_len in
    while !new_len < n do
      new_len := if !new_len > max_int / 2 then n else 2 * !new_len
    done;
    let buf' = Bytes.extend !(b.buf) (!new_len - old_len) 0 in
    b.buf := buf')
;;

(* Current index (for writing into buf) *)
let current b = Bytes.length !(b.buf) - b.length

let make_offset b ~object_offset index =
  { index; owner = b.buf; cycle = b.cycle; object_offset }
;;

(* Current position, not a completed-object offset. *)
let current_offset b = make_offset b ~object_offset:false b.length
let completed_offset b index = make_offset b ~object_offset:true index

let validate_offset name b o =
  if o.owner != b.buf
  then invalid_arg (Printf.sprintf "Builder.%s: offset belongs to another builder" name);
  if o.cycle != b.cycle
  then
    invalid_arg
      (Printf.sprintf "Builder.%s: offset belongs to an earlier build cycle" name);
  if not o.object_offset
  then
    invalid_arg
      (Printf.sprintf "Builder.%s: offset does not identify a completed object" name);
  if o.index <= 0 || o.index > b.length
  then
    invalid_arg
      (Printf.sprintf "Builder.%s: offset is not behind the write position" name)
;;

let relative_offset name ~width b i o =
  validate_offset name b o;
  let relative = b.length - i - o.index in
  if relative <= 0
  then invalid_arg (Printf.sprintf "Builder.%s: offset is not behind its reference" name);
  if width = 4 && Int64.of_int relative > 0xffff_ffffL
  then invalid_arg (Printf.sprintf "Builder.%s: relative offset exceeds 32 bits" name);
  relative
;;

(* Add padding so that, after writing [additional_bytes], the buffer size is a
     multiple of [align]. Ensures space for [additional_bytes] plus
     [reserve_bytes], but only the alignment padding advances the builder. *)
let prealign b ?(additional_bytes = 0) ?(reserve_bytes = 0) align =
  if align <= 0 then invalid_size "prealign" "alignment must be positive";
  let unpadded_length = checked_add_size "prealign" b.length additional_bytes in
  let pad_bytes = -unpadded_length land (align - 1) in
  let padded_length = checked_add_size "prealign" unpadded_length pad_bytes in
  let required_capacity = checked_add_size "prealign" padded_length reserve_bytes in
  b.minalign <- Int.max align b.minalign;
  ensure_capacity b required_capacity;
  if pad_bytes != 0
  then (
    b.length <- b.length + pad_bytes;
    Bytes.fill !(b.buf) (current b) pad_bytes '\x00')
;;

let prep ~align ~bytes b =
  (match b.state with
   | Vector _ as state -> invalid_state "prep" "an idle builder or an open table" state
   | Idle | Table _ -> ());
  prealign b align ~additional_bytes:bytes;
  b.length <- b.length + bytes
;;

let prep_with_prefix ~align ~bytes ~prefix_bytes b =
  prealign b align ~additional_bytes:bytes ~reserve_bytes:prefix_bytes;
  b.length <- b.length + bytes
;;

let vector_payload_size name ~n_elts ~elt_size =
  if n_elts < 0 then invalid_size name "element count must be non-negative";
  if elt_size <= 0 then invalid_size name "element size must be positive";
  if n_elts > max_int / elt_size then invalid_size name "size overflow";
  n_elts * elt_size
;;

let require_table name b =
  match b.state with
  | Table table -> table
  | state -> invalid_state name "an open table" state
;;

let validate_slot_id name table id =
  if id < 0
  then invalid_arg (Printf.sprintf "Builder.%s: field ID must be non-negative" name);
  if id >= table.n_fields
  then
    invalid_arg
      (Printf.sprintf
         "Builder.%s: field ID %d is outside table field count %d"
         name
         id
         table.n_fields)
;;

let save_slot ~id b =
  let table = require_table "save_slot" b in
  validate_slot_id "save_slot" table id;
  if id >= b.cur_vtable_len then b.cur_vtable_len <- id + 1;
  b.cur_vtable.(id) <- b.length
;;

let[@inline] set_scalar t b i x = Primitives.set_scalar t !(b.buf) (current b + i) x
let set_string b i s = Bytes.blit_string s 0 !(b.buf) (current b + i) (String.length s)
let set_padding b i n = Bytes.fill !(b.buf) (current b + i) n '\000'

(* convert offset to relative *)
let set_uoffset b i o =
  let i' = current b + i in
  let b' = !(b.buf) in
  Primitives.set_int32_le
    b'
    i'
    (Int32.of_int (relative_offset "set_uoffset" ~width:4 b i o))
;;

let push_slot_scalar t f x b =
  validate_slot_id "push_slot_scalar" (require_table "push_slot_scalar" b) f;
  let size = Primitives.size_scalar t in
  prep ~align:size ~bytes:size b;
  set_scalar t b 0 x;
  save_slot ~id:f b;
  b
;;

let push_slot_scalar_default t f ~default x b =
  validate_slot_id
    "push_slot_scalar_default"
    (require_table "push_slot_scalar_default" b)
    f;
  (* use compare since nan <> nan *)
  if compare x default = 0 then b else push_slot_scalar t f x b
;;

let push_slot_ref f x b =
  validate_slot_id "push_slot_ref" (require_table "push_slot_ref" b) f;
  validate_offset "push_slot_ref" b x;
  let size = 4 in
  prep ~align:size ~bytes:size b;
  set_uoffset b 0 x;
  save_slot ~id:f b;
  b
;;

(* convert offset to 64-bit relative *)
let set_uoffset64 b i o =
  let i' = current b + i in
  let b' = !(b.buf) in
  Primitives.set_int64_le
    b'
    i'
    (Int64.of_int (relative_offset "set_uoffset64" ~width:8 b i o))
;;

let push_slot_ref64 f x b =
  validate_slot_id "push_slot_ref64" (require_table "push_slot_ref64" b) f;
  validate_offset "push_slot_ref64" b x;
  let size = 8 in
  prep ~align:size ~bytes:size b;
  set_uoffset64 b 0 x;
  save_slot ~id:f b;
  b
;;

let[@inline] push_slot_union ft fo t o b =
  let table = require_table "push_slot_union" b in
  validate_slot_id "push_slot_union" table ft;
  validate_slot_id "push_slot_union" table fo;
  validate_offset "push_slot_union" b o;
  let size = 4 in
  prep ~align:1 ~bytes:1 b;
  set_scalar TUByte b 0 t;
  save_slot ~id:ft b;
  prep ~align:size ~bytes:size b;
  set_uoffset b 0 o;
  save_slot ~id:fo b;
  b
;;

let[@inline] push_slot_struct set size align f s b =
  validate_slot_id "push_slot_struct" (require_table "push_slot_struct" b) f;
  prep ~align ~bytes:size b;
  set b 0 s;
  save_slot ~id:f b;
  b
;;

let find_shared_string b s = Hashtbl.find_opt b.strings s
let add_shared_string b s o = Hashtbl.add b.strings s o

(* size of vector length field *)
let vector_len_size = 4

let start_vector_with_prefix name b ~prefix_size ~n_elts ~elt_size =
  require_idle name b;
  let bytes = vector_payload_size name ~n_elts ~elt_size in
  prep_with_prefix
    b
    ~align:(Int.max prefix_size elt_size)
    ~bytes
    ~prefix_bytes:prefix_size;
  if prefix_size = vector_len_size
  then Primitives.set_int32_le !(b.buf) (current b - prefix_size) (Int32.of_int n_elts)
  else Primitives.set_int64_le !(b.buf) (current b - prefix_size) (Int64.of_int n_elts);
  b.state <- Vector { start = b.length; prefix_size; n_elts; elt_size }
;;

let start_vector b ~n_elts ~elt_size =
  start_vector_with_prefix "start_vector" b ~prefix_size:vector_len_size ~n_elts ~elt_size
;;

let end_vector_with_prefix name expected_prefix b =
  match b.state with
  | Vector vector when vector.prefix_size = expected_prefix ->
    if b.length <> vector.start
    then
      invalid_arg
        (Printf.sprintf
           "Builder.%s: vector payload moved after start (%d elements of %d bytes)"
           name
           vector.n_elts
           vector.elt_size);
    b.length <- b.length + expected_prefix;
    b.state <- Idle;
    completed_offset b b.length
  | state ->
    invalid_state
      name
      (if expected_prefix = vector_len_size
       then "an open 32-bit vector"
       else "an open 64-bit vector")
      state
;;

let end_vector b = end_vector_with_prefix "end_vector" vector_len_size b

let create_vector t b a =
  let size = Primitives.size_scalar t in
  let len = Array.length a in
  start_vector b ~n_elts:len ~elt_size:size;
  for i = 0 to len - 1 do
    set_scalar t b (i * size) a.(i)
  done;
  end_vector b
;;

(* 64-bit length vector *)
let vector64_len_size = 8

let start_vector64 b ~n_elts ~elt_size =
  start_vector_with_prefix
    "start_vector64"
    b
    ~prefix_size:vector64_len_size
    ~n_elts
    ~elt_size
;;

let end_vector64 b = end_vector_with_prefix "end_vector64" vector64_len_size b

let projected_vector_payload_end name b ~prefix_size ~n_elts ~elt_size =
  let bytes = vector_payload_size name ~n_elts ~elt_size in
  let align = Int.max prefix_size elt_size in
  let unpadded_length = checked_add_size name b.length bytes in
  let padding = -unpadded_length land (align - 1) in
  checked_add_size name unpadded_length padding
;;

let validate_vector_offsets name b ~width ~payload_end ~offset_of a =
  Array.iteri
    (fun i value ->
       match offset_of value with
       | None -> ()
       | Some o ->
         validate_offset name b o;
         let position = i * width in
         let relative = payload_end - position - o.index in
         if relative <= 0
         then
           invalid_arg
             (Printf.sprintf "Builder.%s: offset is not behind its reference" name);
         if width = 4 && Int64.of_int relative > 0xffff_ffffL
         then
           invalid_arg (Printf.sprintf "Builder.%s: relative offset exceeds 32 bits" name))
    a
;;

let set_null_uoffset b width i =
  if width = 4
  then Primitives.set_int32_le !(b.buf) (current b + i) 0l
  else Primitives.set_int64_le !(b.buf) (current b + i) 0L
;;

let create_vector_ref_with name b ~width ~start ~finish ~set ~offset_of a =
  require_idle name b;
  let len = Array.length a in
  let payload_end =
    projected_vector_payload_end name b ~prefix_size:width ~n_elts:len ~elt_size:width
  in
  validate_vector_offsets name b ~width ~payload_end ~offset_of a;
  start b ~n_elts:len ~elt_size:width;
  for i = 0 to len - 1 do
    let position = i * width in
    match offset_of a.(i) with
    | None -> set_null_uoffset b width position
    | Some offset -> set b position offset
  done;
  finish b
;;

let create_vector_ref b a =
  create_vector_ref_with
    "create_vector_ref"
    b
    ~width:vector_len_size
    ~start:start_vector
    ~finish:end_vector
    ~set:set_uoffset
    ~offset_of:(fun offset -> Some offset)
    a
;;

let create_vector_ref64 b a =
  create_vector_ref_with
    "create_vector_ref64"
    b
    ~width:vector64_len_size
    ~start:start_vector64
    ~finish:end_vector64
    ~set:set_uoffset64
    ~offset_of:(fun offset -> Some offset)
    a
;;

let create_sorted_vector_ref b ~compare a =
  require_idle "create_sorted_vector_ref" b;
  let payload_end =
    projected_vector_payload_end
      "create_sorted_vector_ref"
      b
      ~prefix_size:vector_len_size
      ~n_elts:(Array.length a)
      ~elt_size:vector_len_size
  in
  validate_vector_offsets
    "create_sorted_vector_ref"
    b
    ~width:vector_len_size
    ~payload_end
    ~offset_of:(fun offset -> Some offset)
    a;
  let sorted = Array.copy a in
  Array.sort compare sorted;
  create_vector_ref b sorted
;;

let table_field_position b offset voff =
  validate_offset "table_key" b offset;
  let bytes = !(b.buf) in
  let table = Bytes.length bytes - offset.index in
  let vtable = table - Primitives.get_soffset Primitives.Bytes bytes table in
  let vtable_size = Primitives.get_voffset Primitives.Bytes bytes vtable in
  if voff >= vtable_size
  then -1
  else (
    let field = Primitives.get_voffset Primitives.Bytes bytes (vtable + voff) in
    if field = 0 then -1 else table + field)
;;

let compare_table_scalar_key ty ~voff ~default b left right =
  let read offset =
    let position = table_field_position b offset voff in
    if position < 0
    then default
    else Primitives.get_scalar ty Primitives.Bytes !(b.buf) position
  in
  Primitives.compare_scalar ty (read left) (read right)
;;

let compare_table_string_key ~voff b left right =
  let read offset =
    let field = table_field_position b offset voff in
    if field < 0 then invalid_arg "Builder.compare_table_string_key: key is absent";
    let bytes = !(b.buf) in
    let position = field + Primitives.get_uoffset Primitives.Bytes bytes field in
    let length = Primitives.get_uoffset Primitives.Bytes bytes position in
    Primitives.get_string Primitives.Bytes bytes ~off:(position + 4) ~len:length
  in
  String.compare (read left) (read right)
;;

let create_union_vector tag_type b tags values =
  require_idle "create_union_vector" b;
  let length = Array.length tags in
  if Array.length values <> length
  then invalid_arg "Builder.create_union_vector: tag and value lengths differ";
  ignore
    (vector_payload_size
       "create_union_vector"
       ~n_elts:length
       ~elt_size:(Primitives.size_scalar tag_type));
  let values =
    create_vector_ref_with
      "create_union_vector"
      b
      ~width:vector_len_size
      ~start:start_vector
      ~finish:end_vector
      ~set:set_uoffset
      ~offset_of:(fun offset -> offset)
      values
  in
  let tags = create_vector tag_type b tags in
  tags, values
;;

let create_vector64 t b a =
  let size = Primitives.size_scalar t in
  let len = Array.length a in
  start_vector64 b ~n_elts:len ~elt_size:size;
  for i = 0 to len - 1 do
    set_scalar t b (i * size) a.(i)
  done;
  end_vector64 b
;;

let create_vector64_struct set ~size b a =
  let len = Array.length a in
  start_vector64 b ~n_elts:len ~elt_size:size;
  for i = 0 to len - 1 do
    set b (i * size) a.(i)
  done;
  end_vector64 b
;;

let create_vector_struct set ~size b a =
  let len = Array.length a in
  start_vector b ~n_elts:len ~elt_size:size;
  for i = 0 to len - 1 do
    set b (i * size) a.(i)
  done;
  end_vector b
;;

let create_struct set ~size ~align b value =
  require_idle "create_struct" b;
  if size <= 0 then invalid_size "create_struct" "size must be positive";
  if align <= 0 then invalid_size "create_struct" "alignment must be positive";
  if align land (align - 1) <> 0
  then invalid_size "create_struct" "alignment must be a power of two";
  prep ~align ~bytes:size b;
  set b 0 value;
  completed_offset b b.length
;;

let create_string b s =
  require_idle "create_string" b;
  (* ensure null terminator; there may be more padding inserted *)
  prep b ~align:1 ~bytes:1;
  set_padding b 0 1;
  (* string is a regular ubyte vectory otherwise *)
  start_vector b ~n_elts:(String.length s) ~elt_size:1;
  set_string b 0 s;
  end_vector b
;;

let create_nested_vector b (finished_buf : bytes) =
  require_idle "create_nested_vector" b;
  let len = Bytes.length finished_buf in
  (* nested flatbuffers need alignment >= 4 for the root offset *)
  prep_with_prefix
    b
    ~align:(Int.max vector_len_size 4)
    ~bytes:len
    ~prefix_bytes:vector_len_size;
  Primitives.set_int32_le !(b.buf) (current b - vector_len_size) (Int32.of_int len);
  Bytes.blit finished_buf 0 !(b.buf) (current b) len;
  b.state
  <- Vector
       { start = b.length; prefix_size = vector_len_size; n_elts = len; elt_size = 1 };
  end_vector b
;;

let create_shared_string b s =
  match find_shared_string b s with
  | Some o -> o
  | None ->
    let o = create_string b s in
    add_shared_string b s o;
    o
;;

let start_table b ~n_fields =
  require_idle "start_table" b;
  if n_fields < 0 then invalid_arg "Builder.start_table: field count must be non-negative";
  if Array.length b.cur_vtable < n_fields then b.cur_vtable <- Array.make n_fields 0;
  b.cur_vtable_len <- 0;
  Array.fill b.cur_vtable 0 n_fields 0;
  b.state <- Table { start = b.length; n_fields };
  b
;;

let create_vtable b (table : table_state) =
  let table_offset = b.length in
  let table_len = table_offset - table.start in
  let vtable_len = 2 * (2 + b.cur_vtable_len) in
  prealign b 2 ~additional_bytes:vtable_len;
  b.length <- b.length + vtable_len;
  let ind = current b in
  let buf = !(b.buf) in
  Primitives.set_int16_le buf ind vtable_len;
  Primitives.set_int16_le buf (ind + 2) table_len;
  for i = 0 to b.cur_vtable_len - 1 do
    let wip_offset = b.cur_vtable.(i) in
    let real_offset = if wip_offset = 0 then 0 else table_offset - wip_offset in
    Primitives.set_int16_le buf (ind + 4 + (i * 2)) real_offset
  done;
  b.length
;;

let end_table b =
  let table = require_table "end_table" b in
  (* add vtable (signed) offset, to be patched later *)
  prep ~align:4 ~bytes:4 b;
  let table_offset = b.length in
  (* serialize vtable *)
  let vt_offset' = create_vtable b table in
  (* reuse an existing vtable if found, removing extra vtable *)
  let vt_offset = IndCache.find_or_insert b.vtables vt_offset' in
  if vt_offset != vt_offset' then b.length <- table_offset;
  (* patch the *backwards* offset from table to vtable *)
  Primitives.set_int32_le
    !(b.buf)
    (Bytes.length !(b.buf) - table_offset)
    (Int32.of_int (vt_offset - table_offset));
  b.cur_vtable_len <- 0;
  b.state <- Idle;
  completed_offset b table_offset
;;

let finish ?identifier ?(size_prefixed = false) prim b o =
  require_idle "finish" b;
  validate_offset "finish" b o;
  let ident_length = Option.fold identifier ~none:0 ~some:String.length in
  let offset_length = 4 in
  let prefix_length = if size_prefixed then 4 else 0 in
  let header_size = prefix_length + offset_length + ident_length in
  prep ~align:(Int.max 4 b.minalign) ~bytes:header_size b;
  (match identifier with
   | None -> ()
   | Some s -> set_string b (prefix_length + offset_length) s);
  set_uoffset b prefix_length o;
  if size_prefixed
  then
    Primitives.set_int32_le !(b.buf) (current b) (Int32.of_int (b.length - prefix_length));
  let res = Primitives.buf_of_bytes prim !(b.buf) ~off:(current b) ~len:b.length in
  reset_unchecked b;
  res
;;

module Unsafe = struct
  let reserve ~align ~bytes b =
    prep ~align ~bytes b;
    0
  ;;

  let start_vector = start_vector
  let end_vector = end_vector
  let start_vector64 = start_vector64
  let end_vector64 = end_vector64
  let current_offset = current_offset
  let set_scalar = set_scalar
  let set_uoffset = set_uoffset
  let set_uoffset64 = set_uoffset64
  let set_string = set_string
  let set_padding = set_padding
end
