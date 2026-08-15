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

let int_of_type = function
  | Null -> 0
  | Int -> 1
  | UInt -> 2
  | Float -> 3
  | Key -> 4
  | String -> 5
  | Indirect_int -> 6
  | Indirect_uint -> 7
  | Indirect_float -> 8
  | Map -> 9
  | Vector -> 10
  | Vector_int -> 11
  | Vector_uint -> 12
  | Vector_float -> 13
  | Vector_key -> 14
  | Vector_string_deprecated -> 15
  | Vector_int2 -> 16
  | Vector_uint2 -> 17
  | Vector_float2 -> 18
  | Vector_int3 -> 19
  | Vector_uint3 -> 20
  | Vector_float3 -> 21
  | Vector_int4 -> 22
  | Vector_uint4 -> 23
  | Vector_float4 -> 24
  | Blob -> 25
  | Bool -> 26
  | Vector_bool -> 36
;;

let type_of_int = function
  | 0 -> Some Null
  | 1 -> Some Int
  | 2 -> Some UInt
  | 3 -> Some Float
  | 4 -> Some Key
  | 5 -> Some String
  | 6 -> Some Indirect_int
  | 7 -> Some Indirect_uint
  | 8 -> Some Indirect_float
  | 9 -> Some Map
  | 10 -> Some Vector
  | 11 -> Some Vector_int
  | 12 -> Some Vector_uint
  | 13 -> Some Vector_float
  | 14 -> Some Vector_key
  | 15 -> Some Vector_string_deprecated
  | 16 -> Some Vector_int2
  | 17 -> Some Vector_uint2
  | 18 -> Some Vector_float2
  | 19 -> Some Vector_int3
  | 20 -> Some Vector_uint3
  | 21 -> Some Vector_float3
  | 22 -> Some Vector_int4
  | 23 -> Some Vector_uint4
  | 24 -> Some Vector_float4
  | 25 -> Some Blob
  | 26 -> Some Bool
  | 36 -> Some Vector_bool
  | _ -> None
;;

let valid_width = function
  | 1 | 2 | 4 | 8 -> true
  | _ -> false
;;

type reader =
  { length : int
  ; u8 : int -> int
  ; i8 : int -> int
  ; u16 : int -> int
  ; i16 : int -> int
  ; i32 : int -> int32
  ; i64 : int -> int64
  ; f32 : int -> float
  ; f64 : int -> float
  ; sub : int -> int -> string
  }

let reader (type b) (p : b Primitives.t) (b : b) =
  let u8 pos = Char.code (Primitives.get_scalar Primitives.TUByte p b pos) in
  { length = Primitives.length p b
  ; u8
  ; i8 =
      (fun pos ->
        let x = u8 pos in
        if x land 0x80 = 0 then x else x - 0x100)
  ; u16 = (fun pos -> Primitives.get_voffset p b pos)
  ; i16 = (fun pos -> Primitives.get_scalar Primitives.TShort p b pos)
  ; i32 = (fun pos -> Primitives.get_scalar Primitives.TInt p b pos)
  ; i64 = (fun pos -> Primitives.get_scalar Primitives.TLong p b pos)
  ; f32 = (fun pos -> Primitives.get_scalar Primitives.TFloat p b pos)
  ; f64 = (fun pos -> Primitives.get_scalar Primitives.TDouble p b pos)
  ; sub = (fun off len -> Primitives.get_string p b ~off ~len)
  }
;;

let read_int rd pos = function
  | 1 -> Int64.of_int (rd.i8 pos)
  | 2 -> Int64.of_int (rd.i16 pos)
  | 4 -> Int64.of_int32 (rd.i32 pos)
  | 8 -> rd.i64 pos
  | _ -> invalid_arg "Flexbuffers: invalid byte width"
;;

let read_uint_bits rd pos = function
  | 1 -> Int64.of_int (rd.u8 pos)
  | 2 -> Int64.of_int (rd.u16 pos)
  | 4 -> Int64.logand (Int64.of_int32 (rd.i32 pos)) 0xffff_ffffL
  | 8 -> rd.i64 pos
  | _ -> invalid_arg "Flexbuffers: invalid byte width"
;;

let read_float rd pos = function
  | 1 -> Float.of_int (rd.i8 pos)
  | 2 -> Float.of_int (rd.i16 pos)
  | 4 -> rd.f32 pos
  | 8 -> rd.f64 pos
  | _ -> invalid_arg "Flexbuffers: invalid byte width"
;;

let uint_bits_to_int x =
  if Int64.compare x 0L < 0 || Int64.compare x (Int64.of_int max_int) > 0
  then None
  else Some (Int64.to_int x)
;;

type t =
  { rd : reader
  ; region_start : int
  ; region_end : int
  ; pos : int
  ; parent_width : int
  ; byte_width : int
  ; value_type : value_type
  }

type blob_view =
  { value : t
  ; data : int
  ; length : int
  }

type vector_view =
  { value : t
  ; data : int
  ; width : int
  ; length : int
  ; element_type : value_type option
  ; type_table : int option
  }

type map_view =
  { keys_view : vector_view
  ; values_view : vector_view
  }

let value_type t = t.value_type
let is_null t = t.value_type = Null

let indirect t =
  match uint_bits_to_int (read_uint_bits t.rd t.pos t.parent_width) with
  | Some n when n <= t.pos - t.region_start -> t.pos - n
  | _ -> invalid_arg "Flexbuffers: invalid indirect offset"
;;

let as_bool t =
  if t.value_type = Bool
  then Some (not (Int64.equal (read_uint_bits t.rd t.pos t.parent_width) 0L))
  else None
;;

let as_int64 t =
  match t.value_type with
  | Int -> Some (read_int t.rd t.pos t.parent_width)
  | Indirect_int -> Some (read_int t.rd (indirect t) t.byte_width)
  | _ -> None
;;

let as_uint64_bits t =
  match t.value_type with
  | UInt -> Some (read_uint_bits t.rd t.pos t.parent_width)
  | Indirect_uint -> Some (read_uint_bits t.rd (indirect t) t.byte_width)
  | _ -> None
;;

let as_float t =
  match t.value_type with
  | Float -> Some (read_float t.rd t.pos t.parent_width)
  | Indirect_float -> Some (read_float t.rd (indirect t) t.byte_width)
  | _ -> None
;;

let sized_target t =
  let data = indirect t in
  let size_pos = data - t.byte_width in
  match uint_bits_to_int (read_uint_bits t.rd size_pos t.byte_width) with
  | Some length -> data, length
  | None -> invalid_arg "Flexbuffers: size does not fit in an OCaml int"
;;

let key_length t data =
  let rec loop pos =
    if pos >= t.region_end
    then invalid_arg "Flexbuffers: unterminated key"
    else if t.rd.u8 pos = 0
    then pos - data
    else loop (pos + 1)
  in
  loop data
;;

let as_string t =
  if t.value_type <> String
  then None
  else (
    let data, length = sized_target t in
    Some (t.rd.sub data length))
;;

let as_key t =
  if t.value_type <> Key
  then None
  else (
    let data = indirect t in
    Some (t.rd.sub data (key_length t data)))
;;

module Blob = struct
  type value = t
  type t = blob_view

  let length (t : t) = t.length

  let get (t : t) i =
    if i < 0 || i >= t.length then invalid_arg "Flexbuffers.Blob.get";
    Char.chr (t.value.rd.u8 (t.data + i))
  ;;

  let to_bytes (t : t) = Bytes.init t.length (get t)
end

let as_blob t =
  if t.value_type <> Blob
  then None
  else (
    let data, length = sized_target t in
    Some { value = t; data; length })
;;

let fixed_vector_info = function
  | Vector_int2 -> Some (Int, 2)
  | Vector_uint2 -> Some (UInt, 2)
  | Vector_float2 -> Some (Float, 2)
  | Vector_int3 -> Some (Int, 3)
  | Vector_uint3 -> Some (UInt, 3)
  | Vector_float3 -> Some (Float, 3)
  | Vector_int4 -> Some (Int, 4)
  | Vector_uint4 -> Some (UInt, 4)
  | Vector_float4 -> Some (Float, 4)
  | _ -> None
;;

let typed_vector_element = function
  | Vector_int -> Some Int
  | Vector_uint -> Some UInt
  | Vector_float -> Some Float
  | Vector_key | Vector_string_deprecated -> Some Key
  | Vector_bool -> Some Bool
  | _ -> None
;;

let vector_of_value t =
  let data = indirect t in
  match fixed_vector_info t.value_type with
  | Some (element_type, length) ->
    { value = t
    ; data
    ; width = t.byte_width
    ; length
    ; element_type = Some element_type
    ; type_table = None
    }
  | None ->
    let length =
      match uint_bits_to_int (read_uint_bits t.rd (data - t.byte_width) t.byte_width) with
      | Some n -> n
      | None -> invalid_arg "Flexbuffers: vector length does not fit in an OCaml int"
    in
    let element_type = typed_vector_element t.value_type in
    { value = t
    ; data
    ; width = t.byte_width
    ; length
    ; element_type
    ; type_table =
        (if t.value_type = Vector || t.value_type = Map
         then Some (data + (length * t.byte_width))
         else None)
    }
;;

let as_vector t =
  match t.value_type with
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
  | Vector_bool -> Some (vector_of_value t)
  | _ -> None
;;

let vector_get t i =
  if i < 0 || i >= t.length then invalid_arg "Flexbuffers.Vector.get";
  let packed_type =
    match t.type_table, t.element_type with
    | Some types, _ -> t.value.rd.u8 (types + i)
    | None, Some typ -> int_of_type typ lsl 2
    | None, None -> assert false
  in
  let value_type =
    match type_of_int (packed_type lsr 2) with
    | Some typ -> typ
    | None -> invalid_arg "Flexbuffers: invalid vector element type"
  in
  { t.value with
    pos = t.data + (i * t.width)
  ; parent_width = t.width
  ; byte_width = 1 lsl (packed_type land 3)
  ; value_type
  }
;;

module Vector = struct
  type value = t
  type t = vector_view

  let length (t : t) = t.length
  let element_type (t : t) = t.element_type
  let get = vector_get

  let iter f (t : t) =
    for i = 0 to t.length - 1 do
      f (get t i)
    done
  ;;

  let to_list (t : t) = List.init t.length (get t)
  let to_array (t : t) = Array.init t.length (get t)
end

let map_of_value t =
  let values_view = vector_of_value t in
  let data = values_view.data in
  let width = values_view.width in
  let keys_slot = data - (3 * width) in
  let keys_width =
    match uint_bits_to_int (read_uint_bits t.rd (keys_slot + width) width) with
    | Some n -> n
    | None -> invalid_arg "Flexbuffers: invalid map key-vector width"
  in
  let keys_data =
    match uint_bits_to_int (read_uint_bits t.rd keys_slot width) with
    | Some n -> keys_slot - n
    | None -> invalid_arg "Flexbuffers: invalid map key-vector offset"
  in
  let key_length =
    match uint_bits_to_int (read_uint_bits t.rd (keys_data - keys_width) keys_width) with
    | Some n -> n
    | None -> invalid_arg "Flexbuffers: map key count does not fit in an OCaml int"
  in
  let keys_view =
    { value = t
    ; data = keys_data
    ; width = keys_width
    ; length = key_length
    ; element_type = Some Key
    ; type_table = None
    }
  in
  { keys_view; values_view }
;;

let as_map t = if t.value_type = Map then Some (map_of_value t) else None

module Map = struct
  type value = t
  type t = map_view

  let length (t : t) = t.values_view.length
  let keys (t : t) = t.keys_view
  let values (t : t) = t.values_view

  let key (t : t) i =
    match as_key (vector_get t.keys_view i) with
    | Some key -> key
    | None -> assert false
  ;;

  let get (t : t) i = key t i, vector_get t.values_view i

  let find (t : t) wanted =
    let rec search lo hi =
      if lo >= hi
      then None
      else (
        let mid = lo + ((hi - lo) / 2) in
        match String.compare (key t mid) wanted with
        | 0 -> Some (vector_get t.values_view mid)
        | n when n < 0 -> search (mid + 1) hi
        | _ -> search lo mid)
    in
    search 0 (length t)
  ;;
end

let root ?(off = 0) ?len p b =
  let rd = reader p b in
  let len = Option.value len ~default:(rd.length - off) in
  if off < 0 || len < 3 || off > rd.length - len
  then invalid_arg "Flexbuffers.root: truncated root trailer";
  let region_end = off + len in
  let parent_width = rd.u8 (region_end - 1) in
  if not (valid_width parent_width)
  then invalid_arg "Flexbuffers.root: invalid root byte width";
  let packed_type = rd.u8 (region_end - 2) in
  let byte_width = 1 lsl (packed_type land 3) in
  let value_type =
    match type_of_int (packed_type lsr 2) with
    | Some typ -> typ
    | None -> invalid_arg "Flexbuffers.root: invalid root type"
  in
  let pos = region_end - 2 - parent_width in
  if pos < off then invalid_arg "Flexbuffers.root: truncated root value";
  { rd; region_start = off; region_end; pos; parent_width; byte_width; value_type }
;;

type options =
  { max_depth : int
  ; max_values : int
  ; max_apparent_size : int
  ; check_alignment : bool
  ; check_string_terminator : bool
  ; check_utf8 : bool
  ; check_map_order : bool
  }

let default_options =
  { max_depth = 64
  ; max_values = 1_000_000
  ; max_apparent_size = max_int
  ; check_alignment = true
  ; check_string_terminator = true
  ; check_utf8 = true
  ; check_map_order = true
  }
;;

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

type error =
  { kind : error_kind
  ; offset : int
  }

let pp_error_kind ppf = function
  | Out_of_bounds -> Format.pp_print_string ppf "out of bounds"
  | Arithmetic_overflow -> Format.pp_print_string ppf "arithmetic overflow"
  | Invalid_byte_width width -> Format.fprintf ppf "invalid byte width %d" width
  | Invalid_type typ -> Format.fprintf ppf "invalid type %d" typ
  | Invalid_offset -> Format.pp_print_string ppf "invalid backward offset"
  | Invalid_alignment width -> Format.fprintf ppf "misaligned (expected %d)" width
  | Missing_terminator -> Format.pp_print_string ppf "missing NUL terminator"
  | Invalid_utf8 -> Format.pp_print_string ppf "invalid UTF-8"
  | Map_length_mismatch -> Format.pp_print_string ppf "map key/value length mismatch"
  | Unsorted_map_keys -> Format.pp_print_string ppf "map keys are not strictly sorted"
  | Depth_limit_exceeded -> Format.pp_print_string ppf "maximum depth exceeded"
  | Value_limit_exceeded -> Format.pp_print_string ppf "maximum value count exceeded"
  | Apparent_size_limit_exceeded ->
    Format.pp_print_string ppf "maximum apparent size exceeded"
;;

let pp_error ppf { kind; offset } =
  Format.fprintf ppf "%a at offset %d" pp_error_kind kind offset
;;

let error_to_string error = Format.asprintf "%a" pp_error error

exception Verification_error of error

type verification =
  { rd : reader
  ; start : int
  ; finish : int
  ; options : options
  ; mutable values : int
  ; mutable apparent : int
  }

let fail kind offset = raise (Verification_error { kind; offset })

let checked_add a b offset =
  if a < 0 || b < 0 || a > max_int - b then fail Arithmetic_overflow offset else a + b
;;

let checked_mul a b offset =
  if a < 0 || b < 0 || (a <> 0 && b > max_int / a)
  then fail Arithmetic_overflow offset
  else a * b
;;

let account v n offset =
  if n < 0 || v.apparent > v.options.max_apparent_size - n
  then fail Apparent_size_limit_exceeded offset;
  v.apparent <- v.apparent + n
;;

let range v pos length =
  if length < 0 || pos < v.start || pos > v.finish - length then fail Out_of_bounds pos;
  account v length pos
;;

let alignment v pos width =
  if v.options.check_alignment && (pos - v.start) land (width - 1) <> 0
  then fail (Invalid_alignment width) pos
;;

let verify_width width offset =
  if not (valid_width width) then fail (Invalid_byte_width width) offset
;;

let read_size v pos width =
  verify_width width pos;
  range v pos width;
  match uint_bits_to_int (read_uint_bits v.rd pos width) with
  | Some n -> n
  | None -> fail Arithmetic_overflow pos
;;

let verify_utf8 v data length =
  let last = data + length in
  let continuation pos = pos < last && v.rd.u8 pos land 0xc0 = 0x80 in
  let rec loop pos =
    if pos >= last
    then ()
    else (
      let a = v.rd.u8 pos in
      if a < 0x80
      then loop (pos + 1)
      else if a >= 0xc2 && a <= 0xdf && continuation (pos + 1)
      then loop (pos + 2)
      else if
        a = 0xe0
        && pos + 2 < last
        && v.rd.u8 (pos + 1) >= 0xa0
        && v.rd.u8 (pos + 1) <= 0xbf
        && continuation (pos + 2)
      then loop (pos + 3)
      else if a >= 0xe1 && a <= 0xec && continuation (pos + 1) && continuation (pos + 2)
      then loop (pos + 3)
      else if
        a = 0xed
        && pos + 2 < last
        && v.rd.u8 (pos + 1) >= 0x80
        && v.rd.u8 (pos + 1) <= 0x9f
        && continuation (pos + 2)
      then loop (pos + 3)
      else if a >= 0xee && a <= 0xef && continuation (pos + 1) && continuation (pos + 2)
      then loop (pos + 3)
      else if
        a = 0xf0
        && pos + 3 < last
        && v.rd.u8 (pos + 1) >= 0x90
        && v.rd.u8 (pos + 1) <= 0xbf
        && continuation (pos + 2)
        && continuation (pos + 3)
      then loop (pos + 4)
      else if
        a >= 0xf1
        && a <= 0xf3
        && continuation (pos + 1)
        && continuation (pos + 2)
        && continuation (pos + 3)
      then loop (pos + 4)
      else if
        a = 0xf4
        && pos + 3 < last
        && v.rd.u8 (pos + 1) >= 0x80
        && v.rd.u8 (pos + 1) <= 0x8f
        && continuation (pos + 2)
        && continuation (pos + 3)
      then loop (pos + 4)
      else fail Invalid_utf8 pos)
  in
  loop data
;;

let verify_key v data =
  let rec loop pos =
    if pos >= v.finish
    then fail Missing_terminator pos
    else if v.rd.u8 pos = 0
    then pos - data
    else loop (pos + 1)
  in
  let length = loop data in
  account v (length + 1) data;
  if v.options.check_utf8 then verify_utf8 v data length;
  length
;;

let bump_value v pos =
  if v.values >= v.options.max_values then fail Value_limit_exceeded pos;
  v.values <- v.values + 1
;;

let enter v depth pos = if depth > v.options.max_depth then fail Depth_limit_exceeded pos

let target v t =
  let distance = read_size v t.pos t.parent_width in
  if distance <= 0 || distance > t.pos - v.start then fail Invalid_offset t.pos;
  let pos = t.pos - distance in
  alignment v pos t.byte_width;
  pos
;;

let verify_sized_bytes v t data ~terminator =
  let size_pos = data - t.byte_width in
  let length = read_size v size_pos t.byte_width in
  range v data length;
  if terminator && v.options.check_string_terminator
  then (
    range v (data + length) 1;
    if v.rd.u8 (data + length) <> 0 then fail Missing_terminator (data + length));
  if terminator && v.options.check_utf8 then verify_utf8 v data length;
  length
;;

let rec verify_ref v depth t =
  bump_value v t.pos;
  verify_width t.parent_width t.pos;
  verify_width t.byte_width t.pos;
  match t.value_type with
  | Null | Int | UInt | Float | Bool -> ()
  | _ ->
    let data = target v t in
    (match t.value_type with
     | Indirect_int | Indirect_uint | Indirect_float -> range v data t.byte_width
     | Key -> ignore (verify_key v data : int)
     | String -> ignore (verify_sized_bytes v t data ~terminator:true : int)
     | Blob -> ignore (verify_sized_bytes v t data ~terminator:false : int)
     | Vector -> verify_vector v (depth + 1) t data None
     | Vector_int -> verify_vector v (depth + 1) t data (Some Int)
     | Vector_uint -> verify_vector v (depth + 1) t data (Some UInt)
     | Vector_float -> verify_vector v (depth + 1) t data (Some Float)
     | Vector_key | Vector_string_deprecated ->
       verify_vector v (depth + 1) t data (Some Key)
     | Vector_bool -> verify_vector v (depth + 1) t data (Some Bool)
     | Map -> verify_map v (depth + 1) t data
     | Vector_int2
     | Vector_uint2
     | Vector_float2
     | Vector_int3
     | Vector_uint3
     | Vector_float3
     | Vector_int4
     | Vector_uint4
     | Vector_float4 ->
       let _, length = Option.get (fixed_vector_info t.value_type) in
       enter v (depth + 1) data;
       range v data (checked_mul length t.byte_width data)
     | Null | Int | UInt | Float | Bool -> assert false)

and verify_vector v depth t data element_type =
  enter v depth data;
  let length = read_size v (data - t.byte_width) t.byte_width in
  let payload = checked_mul length t.byte_width data in
  range v data payload;
  match element_type with
  | None ->
    let types = checked_add data payload data in
    range v types length;
    for i = 0 to length - 1 do
      let packed = v.rd.u8 (types + i) in
      let typ =
        match type_of_int (packed lsr 2) with
        | Some typ -> typ
        | None -> fail (Invalid_type (packed lsr 2)) (types + i)
      in
      verify_ref
        v
        depth
        { t with
          pos = data + (i * t.byte_width)
        ; parent_width = t.byte_width
        ; byte_width = 1 lsl (packed land 3)
        ; value_type = typ
        }
    done
  | Some typ ->
    for i = 0 to length - 1 do
      verify_ref
        v
        depth
        { t with
          pos = data + (i * t.byte_width)
        ; parent_width = t.byte_width
        ; byte_width = 1
        ; value_type = typ
        }
    done

and verify_map v depth t data =
  enter v depth data;
  let width = t.byte_width in
  let prefixes = checked_mul 3 width data in
  if data < v.start || data - v.start < prefixes then fail Out_of_bounds data;
  let keys_slot = data - prefixes in
  let keys_width = read_size v (keys_slot + width) width in
  verify_width keys_width (keys_slot + width);
  let value_length = read_size v (keys_slot + (2 * width)) width in
  let payload = checked_mul value_length width data in
  range v data payload;
  let types = checked_add data payload data in
  range v types value_length;
  let key_distance = read_size v keys_slot width in
  if key_distance <= 0 || key_distance > keys_slot - v.start
  then fail Invalid_offset keys_slot;
  let keys_data = keys_slot - key_distance in
  alignment v keys_data keys_width;
  let key_length = read_size v (keys_data - keys_width) keys_width in
  if key_length <> value_length then fail Map_length_mismatch keys_data;
  range v keys_data (checked_mul key_length keys_width keys_data);
  let previous = ref None in
  for i = 0 to key_length - 1 do
    let key_ref =
      { t with
        pos = keys_data + (i * keys_width)
      ; parent_width = keys_width
      ; byte_width = 1
      ; value_type = Key
      }
    in
    verify_ref v depth key_ref;
    if v.options.check_map_order
    then (
      let key_data = target v key_ref in
      let key_len = verify_key v key_data in
      let key = v.rd.sub key_data key_len in
      (match !previous with
       | Some prev when String.compare prev key >= 0 -> fail Unsorted_map_keys key_data
       | _ -> ());
      previous := Some key)
  done;
  for i = 0 to value_length - 1 do
    let packed = v.rd.u8 (types + i) in
    let typ =
      match type_of_int (packed lsr 2) with
      | Some typ -> typ
      | None -> fail (Invalid_type (packed lsr 2)) (types + i)
    in
    verify_ref
      v
      depth
      { t with
        pos = data + (i * width)
      ; parent_width = width
      ; byte_width = 1 lsl (packed land 3)
      ; value_type = typ
      }
  done
;;

let verify ?(options = default_options) ?(off = 0) ?len p b =
  let rd = reader p b in
  let len = Option.value len ~default:(rd.length - off) in
  let finish = if off < 0 || len < 0 || off > rd.length - len then -1 else off + len in
  try
    if finish < 0 then fail Out_of_bounds off;
    let v = { rd; start = off; finish; options; values = 0; apparent = 0 } in
    if options.max_depth < 1 || options.max_values < 1 || options.max_apparent_size < 0
    then fail Value_limit_exceeded off;
    if len < 3 then fail Out_of_bounds off;
    let parent_width = rd.u8 (finish - 1) in
    verify_width parent_width (finish - 1);
    let packed = rd.u8 (finish - 2) in
    let value_type =
      match type_of_int (packed lsr 2) with
      | Some typ -> typ
      | None -> fail (Invalid_type (packed lsr 2)) (finish - 2)
    in
    let pos = finish - 2 - parent_width in
    range v pos parent_width;
    verify_ref
      v
      0
      { rd
      ; region_start = off
      ; region_end = finish
      ; pos
      ; parent_width
      ; byte_width = 1 lsl (packed land 3)
      ; value_type
      };
    Ok ()
  with
  | Verification_error error -> Error error
;;

let root_verified ?options ?off ?len p b =
  match verify ?options ?off ?len p b with
  | Error error -> Error error
  | Ok () -> Ok (root ?off ?len p b)
;;
