(* Structural verification of untrusted FlatBuffers.

   Behavioral reference: flatbuffers/include/flatbuffers/verifier.h. Deliberate
   differences from upstream are documented in ocaml/README.md.

   Implementation notes:

   - The verifier is backend independent: it only reads through a small record
     of closures built once from [Primitives.t], so [bytes], [string],
     [Bigstringaf.t] and JavaScript [DataView] all share this code.
   - Every read validates its full byte range before touching the buffer, and
     all offset/length arithmetic is done on values already proven to fit in an
     OCaml [int]. No unchecked read is reachable from here.
   - Checks return [bool] and record the first failure in the state. This keeps
     the success path allocation free: [&&] chains short-circuit, the schema
     path is kept in preallocated arrays, and the [error] record is built only
     when a check fails. *)

type options =
  { max_depth : int
  ; max_tables : int
  ; max_apparent_size : int
  ; check_alignment : bool
  ; check_string_terminator : bool
  ; check_nested_flatbuffers : bool
  ; reject_unknown_union_tags : bool
  }

let default_options =
  { max_depth = 64
  ; max_tables = 1_000_000
  ; (* Generous enough never to fire on a well-formed buffer, small enough to
       bound expansion through heavily shared sub-objects. *)
    max_apparent_size = (if Sys.int_size > 32 then 1 lsl 34 else max_int)
  ; check_alignment = true
  ; check_string_terminator = true
  ; check_nested_flatbuffers = true
  ; reject_unknown_union_tags = false
  }
;;

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

type error =
  { kind : error_kind
  ; offset : int
  ; path : path_element list
  }

(* ------------------------------------------------------------------ *)
(* Formatting                                                          *)
(* ------------------------------------------------------------------ *)

let pp_error_kind ppf = function
  | Out_of_bounds { length; region_end } ->
    Format.fprintf ppf "out of bounds (length %d, region ends at %d)" length region_end
  | Arithmetic_overflow -> Format.pp_print_string ppf "arithmetic overflow"
  | Invalid_offset -> Format.pp_print_string ppf "invalid offset"
  | Invalid_vtable -> Format.pp_print_string ppf "invalid vtable"
  | Invalid_alignment { alignment } -> Format.fprintf ppf "misaligned (expected %d)" alignment
  | Missing_required_field -> Format.pp_print_string ppf "missing required field"
  | Missing_string_terminator -> Format.pp_print_string ppf "missing string terminator"
  | Invalid_identifier { expected; actual } ->
    Format.fprintf
      ppf
      "invalid file identifier (expected %S, got %s)"
      expected
      (match actual with
       | None -> "<truncated>"
       | Some s -> Printf.sprintf "%S" s)
  | Invalid_size_prefix -> Format.pp_print_string ppf "invalid size prefix"
  | Inconsistent_union -> Format.pp_print_string ppf "inconsistent union type/value pair"
  | Unknown_union_tag t -> Format.fprintf ppf "unknown union tag %Ld" t
  | Depth_limit_exceeded -> Format.pp_print_string ppf "maximum depth exceeded"
  | Table_limit_exceeded -> Format.pp_print_string ppf "maximum table count exceeded"
  | Apparent_size_limit_exceeded -> Format.pp_print_string ppf "maximum apparent size exceeded"
;;

let pp_path ppf path =
  if path = []
  then Format.pp_print_string ppf "<root>"
  else
    List.iter
      (function
        | Field f -> Format.fprintf ppf ".%s" f
        | Index i -> Format.fprintf ppf "[%d]" i
        | Union_variant v -> Format.fprintf ppf ":%s" v
        | Nested_buffer -> Format.pp_print_string ppf "/nested")
      path
;;

let pp_error ppf { kind; offset; path } =
  Format.fprintf ppf "%a at offset %d (%a)" pp_error_kind kind offset pp_path path
;;

let error_to_string e = Format.asprintf "%a" pp_error e

(* ------------------------------------------------------------------ *)
(* Checked arithmetic and conversions                                  *)
(* ------------------------------------------------------------------ *)

(* [-1] means "does not fit in a non-negative OCaml int". Callers must treat a
   negative result as a failure; they never feed it to a read. *)

let max_int_64 = Int64.of_int max_int

let u64_to_int (x : int64) : int =
  if Int64.compare x 0L < 0 || Int64.compare x max_int_64 > 0 then -1 else Int64.to_int x
;;

(* [a + b] for non-negative [a] and [b], or [-1] on overflow. *)
let[@inline] checked_add a b = if a < 0 || b < 0 || a > max_int - b then -1 else a + b

(* [a * b] for non-negative [a] and [b], or [-1] on overflow. *)
let[@inline] checked_mul a b =
  if a < 0 || b < 0 then -1 else if a <> 0 && b > max_int / a then -1 else a * b
;;

let[@inline] is_pow2 n = n > 0 && n land (n - 1) = 0

(* ------------------------------------------------------------------ *)
(* Buffer reader                                                       *)
(* ------------------------------------------------------------------ *)

(* Reads are routed through closures built once per verification so the rest of
   this module is backend independent. They all yield an OCaml [int] rather
   than a boxed [int32]/[int64], which keeps the traversal allocation free. A
   width that cannot be represented as a non-negative [int] on this platform
   yields [-1], which every caller treats as a failure. *)
type reader =
  { len : int
  ; u8 : int -> int
  ; u16 : int -> int
  ; s32 : int -> int (* signed 32-bit *)
  ; u32 : int -> int (* unsigned 32-bit, or -1 *)
  ; u64 : int -> int (* unsigned 64-bit, or -1 *)
  ; sub : int -> int -> string
  }

(* Reading an unsigned 32-bit field needs care about the width of [int].

   Where [int] is at least 32 bits — 63 natively, 32 under js_of_ocaml and
   Melange — [Primitives.get_soffset] is an exact, non-allocating conversion,
   and a negative result means the unsigned value is 2^31 or above, which no
   valid 32-bit offset or length reaches: FlatBuffers caps a 32-bit-addressed
   buffer at 2^31-1 bytes.

   Where [int] is 31 bits (linux/i386, linux/arm/v7) it truncates to the low 31
   bits, and the truncation is not detectable from the result: 0x80000000 comes
   back as 0, a perfectly plausible length, so the verifier would accept a
   buffer whose length the reader then refuses to convert. Inspect the [int32]
   before converting there. *)
let u32_reader (type b) (p : b Primitives.t) (b : b) =
  if Sys.int_size >= 32
  then fun i ->
    let s = Primitives.get_soffset p b i in
    if s >= 0 then s else -1
  else fun i ->
    let x = Primitives.get_scalar Primitives.TInt p b i in
    if Int32.compare x 0l < 0
    then -1
    else (
      let n = Int32.to_int x in
      if n < 0 then -1 else n)
;;

let reader (type b) (p : b Primitives.t) (b : b) =
  { len = Primitives.length p b
  ; u8 = (fun i -> Char.code (Primitives.get_scalar Primitives.TUByte p b i))
  ; u16 = (fun i -> Primitives.get_voffset p b i)
  ; (* Signed, so the same truncation cannot masquerade as a valid value: a
       31-bit int loses information here too, but [Read.get_indirect] converts
       vtable displacements with this very function, so reader and verifier
       agree on the position and the verifier range-checks whatever it is. *)
    s32 = (fun i -> Primitives.get_soffset p b i)
  ; u32 = u32_reader p b
  ; u64 = (fun i -> u64_to_int (Primitives.get_scalar Primitives.TLong p b i))
  ; sub = (fun off len -> Primitives.get_string p b ~off ~len)
  }
;;

(* ------------------------------------------------------------------ *)
(* Traversal state                                                     *)
(* ------------------------------------------------------------------ *)

type t =
  { rd : reader
  ; opts : options
  ; mutable region_start : int (* inclusive *)
  ; mutable region_end : int (* exclusive *)
  ; (* Alignment is measured from the start of the framed buffer, not from
       [region_start]: a size prefix shifts the message by four bytes but
       encoders still align payloads against the buffer origin, exactly as the
       upstream C++ verifier does. *)
    mutable align_base : int
  ; mutable depth : int
  ; mutable tables : int
  ; mutable apparent : int
  ; (* vtable of the table currently being verified *)
    mutable cur_table : int
  ; mutable cur_vtable : int
  ; mutable cur_vtsize : int
  ; (* saved table contexts, indexed by depth *)
    mutable sv_table : int array
  ; mutable sv_vtable : int array
  ; mutable sv_vtsize : int array
  ; (* schema path, kept in parallel arrays so pushing costs no allocation *)
    mutable p_kind : int array
  ; mutable p_name : string array
  ; mutable p_index : int array
  ; mutable p_len : int
  ; mutable err : error option
  }

type table_fn = t -> int -> bool
type union_fn = t -> int64 -> int -> bool

let[@inline] has_error v =
  match v.err with
  | None -> false
  | Some _ -> true
;;

(* --- path -------------------------------------------------------- *)

let path_kind_field = 0
let path_kind_index = 1
let path_kind_variant = 2
let path_kind_nested = 3

let grow_path v =
  let n = Array.length v.p_kind in
  let n' = if n = 0 then 8 else n * 2 in
  let k = Array.make n' 0 in
  Array.blit v.p_kind 0 k 0 n;
  v.p_kind <- k;
  let s = Array.make n' "" in
  Array.blit v.p_name 0 s 0 n;
  v.p_name <- s;
  let i = Array.make n' 0 in
  Array.blit v.p_index 0 i 0 n;
  v.p_index <- i
;;

let[@inline] push_path v kind name index =
  if v.p_len >= Array.length v.p_kind then grow_path v;
  v.p_kind.(v.p_len) <- kind;
  v.p_name.(v.p_len) <- name;
  v.p_index.(v.p_len) <- index;
  v.p_len <- v.p_len + 1
;;

let[@inline] pop_path v = v.p_len <- v.p_len - 1

let capture_path v =
  let rec build i acc =
    if i < 0
    then acc
    else (
      let e =
        let k = v.p_kind.(i) in
        if k = path_kind_field
        then Field v.p_name.(i)
        else if k = path_kind_index
        then Index v.p_index.(i)
        else if k = path_kind_variant
        then Union_variant v.p_name.(i)
        else Nested_buffer
      in
      build (i - 1) (e :: acc))
  in
  build (v.p_len - 1) []
;;

(* --- failures ---------------------------------------------------- *)

let fail v kind offset =
  if not (has_error v) then v.err <- Some { kind; offset; path = capture_path v };
  false
;;

(* --- range and alignment ----------------------------------------- *)

let account v len =
  if v.apparent > v.opts.max_apparent_size - len
  then fail v Apparent_size_limit_exceeded v.region_start
  else (
    v.apparent <- v.apparent + len;
    true)
;;

(* Prove that [pos, pos + len) lies inside the current region. *)
let check_range v pos len =
  if len < 0 || pos < v.region_start || pos > v.region_end - len
  then fail v (Out_of_bounds { length = len; region_end = v.region_end }) pos
  else account v len
;;

let check_align v pos align =
  if (not v.opts.check_alignment) || (pos - v.align_base) land (align - 1) = 0
  then true
  else fail v (Invalid_alignment { alignment = align }) pos
;;

let[@inline] check_scalar v pos size align = check_align v pos align && check_range v pos size

(* --- checked reads ------------------------------------------------ *)

(* All return [-1] on failure, having recorded the error. *)

let read_u16 v pos = if check_scalar v pos 2 2 then v.rd.u16 pos else -1

let read_u32 v pos =
  if check_scalar v pos 4 4
  then (
    let x = v.rd.u32 pos in
    if x < 0
    then (
      ignore (fail v Arithmetic_overflow pos : bool);
      -1)
    else x)
  else -1
;;

let read_u64 v pos =
  if check_scalar v pos 8 8
  then (
    let x = v.rd.u64 pos in
    if x < 0
    then (
      ignore (fail v Arithmetic_overflow pos : bool);
      -1)
    else x)
  else -1
;;

let[@inline] read_uoffset v pos ~wide = if wide then read_u64 v pos else read_u32 v pos

(* Unsigned integer of [size] bytes, as int64. Returns [Int64.minus_one] on
   failure; callers must consult [has_error]. *)
let read_uint64_of_size v pos size =
  if not (check_scalar v pos size size)
  then Int64.minus_one
  else if size = 1
  then Int64.of_int (v.rd.u8 pos)
  else if size = 2
  then Int64.of_int (v.rd.u16 pos)
  else if size = 4
  then Int64.of_int (v.rd.u32 pos)
  else if size = 8
  then (
    let x = v.rd.u64 pos in
    if x < 0
    then (
      ignore (fail v Arithmetic_overflow pos : bool);
      Int64.minus_one)
    else Int64.of_int x)
  else (
    ignore (fail v Invalid_offset pos : bool);
    Int64.minus_one)
;;

(* --- forward offsets ---------------------------------------------- *)

(* Verify the forward offset stored at [pos] and return its target, or [-1]. *)
let verify_offset v pos ~wide =
  let o = read_uoffset v pos ~wide in
  if o < 0
  then -1
  else if o = 0
  then (
    (* May not point to itself. *)
    ignore (fail v Invalid_offset pos : bool);
    -1)
  else if o > v.region_end - pos
  then (
    ignore (fail v (Out_of_bounds { length = o; region_end = v.region_end }) pos : bool);
    -1)
  else (
    let target = pos + o in
    if check_range v target 1 then target else -1)
;;

(* ------------------------------------------------------------------ *)
(* Tables                                                              *)
(* ------------------------------------------------------------------ *)

let grow_stack v =
  let n = Array.length v.sv_table in
  let n' = if n = 0 then 8 else n * 2 in
  let grow a =
    let b = Array.make n' 0 in
    Array.blit a 0 b 0 n;
    b
  in
  v.sv_table <- grow v.sv_table;
  v.sv_vtable <- grow v.sv_vtable;
  v.sv_vtsize <- grow v.sv_vtsize
;;

let enter_table v pos =
  v.depth <- v.depth + 1;
  v.tables <- v.tables + 1;
  let ok =
    if v.depth > v.opts.max_depth
    then fail v Depth_limit_exceeded pos
    else if v.tables > v.opts.max_tables
    then fail v Table_limit_exceeded pos
    else if not (check_scalar v pos 4 4)
    then false
    else (
      (* The vtable is at [pos - soffset]. Bound the displacement before doing
         the subtraction so it cannot wrap on a 32-bit int. *)
      let soff = v.rd.s32 pos in
      if soff > pos - v.region_start || soff <= pos - v.region_end
      then fail v Invalid_vtable pos
      else (
        let vt = pos - soff in
        let vsize = read_u16 v vt in
        if vsize < 0
        then false
        else if vsize land 1 <> 0
        then fail v Invalid_vtable vt
        else if not (check_range v vt vsize)
        then false
        else (
          if v.depth > Array.length v.sv_table then grow_stack v;
          v.sv_table.(v.depth - 1) <- v.cur_table;
          v.sv_vtable.(v.depth - 1) <- v.cur_vtable;
          v.sv_vtsize.(v.depth - 1) <- v.cur_vtsize;
          v.cur_table <- pos;
          v.cur_vtable <- vt;
          v.cur_vtsize <- vsize;
          true)))
  in
  if not ok then v.depth <- v.depth - 1;
  ok
;;

let exit_table v r =
  v.depth <- v.depth - 1;
  v.cur_table <- v.sv_table.(v.depth);
  v.cur_vtable <- v.sv_vtable.(v.depth);
  v.cur_vtsize <- v.sv_vtsize.(v.depth);
  r
;;

(* Position of field [voff] in the current table, or [-1] when the field is
   absent. On a structural failure the error is recorded, so callers must check
   [has_error] before treating [-1] as "absent". *)
let field_pos v voff =
  if voff < 0 || voff + 2 > v.cur_vtsize
  then -1
  else (
    let foff = v.rd.u16 (v.cur_vtable + voff) in
    if foff = 0
    then -1
    else if foff > v.region_end - v.cur_table
    then (
      ignore
        (fail v (Out_of_bounds { length = foff; region_end = v.region_end }) v.cur_table
         : bool);
      -1)
    else v.cur_table + foff)
;;

let missing v ~name ~required =
  if not required
  then true
  else (
    push_path v path_kind_field name 0;
    let r = fail v Missing_required_field v.cur_table in
    pop_path v;
    r)
;;

(* ------------------------------------------------------------------ *)
(* Payload verification                                                *)
(* ------------------------------------------------------------------ *)

(* Common shape of vectors and strings: a length prefix followed by
   [len * elem_size] contiguous bytes. Returns the position just past the
   contents, or [-1]. *)
let verify_vector_bytes v pos ~len_size ~elem_size =
  let n = read_uoffset v pos ~wide:(len_size = 8) in
  if n < 0
  then -1
  else (
    let bytes = checked_mul n elem_size in
    if bytes < 0
    then (
      ignore (fail v Arithmetic_overflow pos : bool);
      -1)
    else (
      let total = checked_add len_size bytes in
      if total < 0
      then (
        ignore (fail v Arithmetic_overflow pos : bool);
        -1)
      else if check_range v pos total
      then pos + total
      else -1))
;;

let vector_length v pos ~len_size = read_uoffset v pos ~wide:(len_size = 8)

let verify_string_at v pos =
  let e = verify_vector_bytes v pos ~len_size:4 ~elem_size:1 in
  if e < 0
  then false
  else if not v.opts.check_string_terminator
  then true
  else if not (check_range v e 1)
  then false
  else if v.rd.u8 e = 0
  then true
  else fail v Missing_string_terminator e
;;

let verify_struct_at v pos ~size ~align =
  (is_pow2 align || fail v (Invalid_alignment { alignment = align }) pos)
  && check_align v pos align
  && check_range v pos size
;;

(* [pos] holds a forward offset to a string. *)
let verify_string_ref v pos ~wide =
  let t = verify_offset v pos ~wide in
  t >= 0 && verify_string_at v t
;;

let verify_table_ref v pos ~wide (fn : table_fn) =
  let t = verify_offset v pos ~wide in
  t >= 0 && fn v t
;;

let verify_vector_of_strings v data n =
  let rec loop i =
    if i >= n
    then true
    else (
      push_path v path_kind_index "" i;
      let r = verify_string_ref v (data + (4 * i)) ~wide:false in
      pop_path v;
      r && loop (i + 1))
  in
  loop 0
;;

let verify_vector_of_tables v data n (fn : table_fn) =
  let rec loop i =
    if i >= n
    then true
    else (
      push_path v path_kind_index "" i;
      let r = verify_table_ref v (data + (4 * i)) ~wide:false fn in
      pop_path v;
      r && loop (i + 1))
  in
  loop 0
;;

(* ------------------------------------------------------------------ *)
(* Field verifiers used by generated code                              *)
(* ------------------------------------------------------------------ *)

(* Shared prologue: locate the field and handle absence. The result is either
   the field position, with the field name pushed onto the path and a matching
   [end_field] owed by the caller, or one of two sentinels. A closure-taking
   wrapper would be tidier, but this runs once per field of every visited table
   and closures would make verification allocate proportionally to the buffer. *)
let field_absent_ok = -1
let field_stop = -2

let begin_field v ~name ~voff ~required =
  let p = field_pos v voff in
  if has_error v
  then field_stop
  else if p < 0
  then if missing v ~name ~required then field_absent_ok else field_stop
  else (
    push_path v path_kind_field name 0;
    p)
;;

let[@inline] end_field v r =
  pop_path v;
  r
;;

let field_inline v ~name ~voff ~size ~align ~required =
  let p = begin_field v ~name ~voff ~required in
  if p < 0 then p = field_absent_ok else end_field v (verify_struct_at v p ~size ~align)
;;

let field_string v ~name ~voff ~required ~off64 =
  let p = begin_field v ~name ~voff ~required in
  if p < 0 then p = field_absent_ok else end_field v (verify_string_ref v p ~wide:off64)
;;

let field_table v ~name ~voff ~required ~off64 fn =
  let p = begin_field v ~name ~voff ~required in
  if p < 0
  then p = field_absent_ok
  else end_field v (verify_table_ref v p ~wide:off64 fn)
;;

let field_vector v ~name ~voff ~required ~off64 ~vec64 ~elem_size =
  let p = begin_field v ~name ~voff ~required in
  if p < 0
  then p = field_absent_ok
  else (
    let t = verify_offset v p ~wide:off64 in
    end_field
      v
      (t >= 0
       && verify_vector_bytes v t ~len_size:(if vec64 then 8 else 4) ~elem_size >= 0))
;;

let field_vector_string v ~name ~voff ~required ~off64 ~vec64 =
  let p = begin_field v ~name ~voff ~required in
  if p < 0
  then p = field_absent_ok
  else (
    let len_size = if vec64 then 8 else 4 in
    let t = verify_offset v p ~wide:off64 in
    end_field
      v
      (t >= 0
       && verify_vector_bytes v t ~len_size ~elem_size:4 >= 0
       && verify_vector_of_strings v (t + len_size) (vector_length v t ~len_size)))
;;

let field_vector_table v ~name ~voff ~required ~off64 ~vec64 fn =
  let p = begin_field v ~name ~voff ~required in
  if p < 0
  then p = field_absent_ok
  else (
    let len_size = if vec64 then 8 else 4 in
    let t = verify_offset v p ~wide:off64 in
    end_field
      v
      (t >= 0
       && verify_vector_bytes v t ~len_size ~elem_size:4 >= 0
       && verify_vector_of_tables v (t + len_size) (vector_length v t ~len_size) fn))
;;

(* ------------------------------------------------------------------ *)
(* Nested FlatBuffers                                                  *)
(* ------------------------------------------------------------------ *)

let verify_root_in_region v ?identifier (fn : table_fn) =
  let start = v.region_start in
  if v.region_end - start < 4
  then fail v (Out_of_bounds { length = 4; region_end = v.region_end }) start
  else (
    let ident_ok =
      match identifier with
      | None -> true
      | Some id ->
        if v.region_end - start < 8
        then fail v (Invalid_identifier { expected = id; actual = None }) start
        else (
          let actual = v.rd.sub (start + 4) 4 in
          if String.equal actual id
          then true
          else fail v (Invalid_identifier { expected = id; actual = Some actual }) (start + 4))
    in
    ident_ok
    &&
    let root = verify_offset v start ~wide:false in
    root >= 0 && fn v root)
;;

(* Verify the contents of a [nested_flatbuffer] byte vector as a buffer in its
   own right, with the verifier's region narrowed to exactly the vector payload
   so nothing outside it can be reached. The payload is never copied. *)
let verify_nested_at v t ~len_size fn =
  if not v.opts.check_nested_flatbuffers
  then true
  else (
    let n = vector_length v t ~len_size in
    let data = t + len_size in
    (* An empty vector means "not present". *)
    if n = 0
    then true
    else (
        let saved_start = v.region_start
        and saved_end = v.region_end
        and saved_align = v.align_base
        and saved_table = v.cur_table
        and saved_vtable = v.cur_vtable
        and saved_vtsize = v.cur_vtsize in
        v.region_start <- data;
        v.region_end <- data + n;
        v.align_base <- data;
        v.cur_table <- -1;
        v.cur_vtable <- -1;
        v.cur_vtsize <- 0;
        push_path v path_kind_nested "" 0;
        let r = verify_root_in_region v fn in
        pop_path v;
        v.region_start <- saved_start;
        v.region_end <- saved_end;
        v.align_base <- saved_align;
        v.cur_table <- saved_table;
        v.cur_vtable <- saved_vtable;
        v.cur_vtsize <- saved_vtsize;
        r))
;;

let field_nested_buffer v ~name ~voff ~required ~off64 ~vec64 fn =
  let p = begin_field v ~name ~voff ~required in
  if p < 0
  then p = field_absent_ok
  else (
    let len_size = if vec64 then 8 else 4 in
    let t = verify_offset v p ~wide:off64 in
    end_field
      v
      (t >= 0
       && verify_vector_bytes v t ~len_size ~elem_size:1 >= 0
       && verify_nested_at v t ~len_size fn))
;;

(* ------------------------------------------------------------------ *)
(* Unions                                                              *)
(* ------------------------------------------------------------------ *)

let union_table v slot ~variant fn =
  push_path v path_kind_variant variant 0;
  let r = verify_table_ref v slot ~wide:false fn in
  pop_path v;
  r
;;

let union_string v slot ~variant =
  push_path v path_kind_variant variant 0;
  let r = verify_string_ref v slot ~wide:false in
  pop_path v;
  r
;;

let union_struct v slot ~variant ~size ~align =
  push_path v path_kind_variant variant 0;
  let t = verify_offset v slot ~wide:false in
  let r = t >= 0 && verify_struct_at v t ~size ~align in
  pop_path v;
  r
;;

(* Reachable only from a union vector, where a NONE element carries no
   payload. *)
let union_none _v _slot = true

let union_unknown v tag slot =
  if v.opts.reject_unknown_union_tags then fail v (Unknown_union_tag tag) slot else true
;;

let field_union v ~name ~type_voff ~voff ~required ~tag_size (dispatch : union_fn) =
  let tp = field_pos v type_voff in
  if has_error v
  then false
  else (
    let vp = field_pos v voff in
    if has_error v
    then false
    else if tp < 0 && vp < 0
    then missing v ~name ~required
    else (
      push_path v path_kind_field name 0;
      let r =
        if tp < 0
        then (* value without discriminator *)
          fail v Inconsistent_union vp
        else (
          let tag = read_uint64_of_size v tp tag_size in
          if has_error v
          then false
          else if Int64.equal tag 0L
          then if vp < 0 then true else fail v Inconsistent_union vp
          else if vp < 0
          then fail v Inconsistent_union tp
          else dispatch v tag vp)
      in
      pop_path v;
      r))
;;

let field_union_vector v ~name ~type_voff ~voff ~required ~tag_size (dispatch : union_fn) =
  let tp = field_pos v type_voff in
  if has_error v
  then false
  else (
    let vp = field_pos v voff in
    if has_error v
    then false
    else if tp < 0 && vp < 0
    then missing v ~name ~required
    else (
      push_path v path_kind_field name 0;
      let r =
        if tp < 0 || vp < 0
        then fail v Inconsistent_union (if tp < 0 then vp else tp)
        else (
          let tv = verify_offset v tp ~wide:false in
          let vv = verify_offset v vp ~wide:false in
          tv >= 0
          && vv >= 0
          && verify_vector_bytes v tv ~len_size:4 ~elem_size:tag_size >= 0
          && verify_vector_bytes v vv ~len_size:4 ~elem_size:4 >= 0
          &&
          let tn = vector_length v tv ~len_size:4 in
          let vn = vector_length v vv ~len_size:4 in
          if tn <> vn
          then fail v Inconsistent_union vv
          else (
            let tdata = tv + 4 and vdata = vv + 4 in
            let rec loop i =
              if i >= vn
              then true
              else (
                push_path v path_kind_index "" i;
                let tag = read_uint64_of_size v (tdata + (i * tag_size)) tag_size in
                let r =
                  if has_error v
                  then false
                  else if Int64.equal tag 0L
                  then true
                  else dispatch v tag (vdata + (4 * i))
                in
                pop_path v;
                r && loop (i + 1))
            in
            loop 0))
      in
      pop_path v;
      r))
;;

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

let create rd opts =
  { rd
  ; opts
  ; region_start = 0
  ; region_end = rd.len
  ; align_base = 0
  ; depth = 0
  ; tables = 0
  ; apparent = 0
  ; cur_table = -1
  ; cur_vtable = -1
  ; cur_vtsize = 0
  ; sv_table = [||]
  ; sv_vtable = [||]
  ; sv_vtsize = [||]
  ; p_kind = [||]
  ; p_name = [||]
  ; p_index = [||]
  ; p_len = 0
  ; err = None
  }
;;

let verify_root
  ?(options = default_options)
  ?(size_prefixed = false)
  ?(off = 0)
  ?identifier
  p
  b
  (fn : table_fn)
  =
  let rd = reader p b in
  let options =
    if options.max_depth >= 1 && options.max_tables >= 1 && options.max_apparent_size >= 0
    then options
    else
      { options with
        max_depth = max 1 options.max_depth
      ; max_tables = max 1 options.max_tables
      ; max_apparent_size = max 0 options.max_apparent_size
      }
  in
  let v = create rd options in
  let ok =
    if off < 0 || off > rd.len
    then fail v (Out_of_bounds { length = 0; region_end = rd.len }) off
    else (
      v.region_start <- off;
      v.region_end <- rd.len;
      v.align_base <- off;
      if not size_prefixed
      then verify_root_in_region v ?identifier fn
      else if rd.len - off < 4
      then fail v Invalid_size_prefix off
      else (
        let n = read_u32 v off in
        if n < 0
        then false
        else if n > rd.len - off - 4
        then fail v Invalid_size_prefix off
        else (
          (* The prefix bounds the message; trailing data is allowed. *)
          v.region_start <- off + 4;
          v.region_end <- off + 4 + n;
          verify_root_in_region v ?identifier fn)))
  in
  match v.err with
  | Some e -> Error e
  | None -> if ok then Ok () else Error { kind = Invalid_offset; offset = off; path = [] }
;;
