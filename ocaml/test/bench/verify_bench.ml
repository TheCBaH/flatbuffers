(* Cost of structural verification, against the unchecked reader baseline.

   Three shapes are measured: a small scalar-only table, the vector- and
   string-heavy example monster, and a deeply nested chain of tables. For each
   we time the unchecked traversal, verification alone, and verification
   followed by the same traversal, and report bytes allocated per iteration. *)

open Generated.Monster_test
open MyGame.Example
module P = Flatbuffers.Primitives

let small_monster () =
  let b = Rt.Builder.create () in
  let name = Rt.String.create b "small" in
  Monster.Builder.(start b |> add_name name |> add_hp 80 |> add_mana 150 |> finish)
  |> Monster.finish_buf P.Bytes b
;;

let wide_monster () =
  let b = Rt.Builder.create () in
  let name = Rt.String.create b "MyMonster" in
  let name2 = Rt.String.create b "Fred" in
  let inv = Rt.UByte.Vector.create b (Array.init 64 (fun i -> Char.chr (i land 0xFF))) in
  let mon2 = Monster.Builder.(start b |> add_name name2 |> finish) in
  let test4 = Test.Vector.create b (Array.init 32 (fun i -> i, i land 0x7F)) in
  let strings =
    Rt.String.Vector.create
      b
      (Array.init 16 (fun i -> Rt.String.create b (Printf.sprintf "string-%d" i)))
  in
  let longs = Rt.Long.Vector.create b (Array.init 64 Int64.of_int) in
  let doubles = Rt.Double.Vector.create b (Array.init 64 float_of_int) in
  Monster.Builder.(
    start b
    |> add_pos (1.0, 2.0, 3.0, 3.0, Color.green, (5, 6))
    |> add_hp 80
    |> add_name name
    |> add_inventory inv
    |> add_test_monster mon2
    |> add_test4 test4
    |> add_testarrayofstring strings
    |> add_vector_of_longs longs
    |> add_vector_of_doubles doubles
    |> finish)
  |> Monster.finish_buf P.Bytes b
;;

let deep_monster depth =
  let b = Rt.Builder.create () in
  let rec build i =
    let inner = if i >= depth then None else Some (build (i + 1)) in
    let name = Rt.String.create b (string_of_int i) in
    let t = Monster.Builder.(start b |> add_name name |> add_hp i) in
    let t =
      match inner with
      | None -> t
      | Some e -> Monster.Builder.add_enemy e t
    in
    Monster.Builder.finish t
  in
  build 1 |> Monster.finish_buf P.Bytes b
;;

(* An unchecked traversal that touches everything the verifier does. *)
let rec read_monster buf m =
  let sum = ref 0 in
  sum := !sum + Monster.hp buf m + Monster.mana buf m;
  sum := !sum + Rt.String.length buf (Monster.name buf m);
  Rt.Option.iter
    (fun v -> sum := !sum + Rt.UByte.Vector.length buf v)
    (Monster.inventory buf m);
  Rt.Option.iter (fun v -> sum := !sum + Test.Vector.length buf v) (Monster.test4 buf m);
  Rt.Option.iter
    (fun v -> Rt.String.Vector.iter buf (fun s -> sum := !sum + Rt.String.length buf s) v)
    (Monster.testarrayofstring buf m);
  Rt.Option.iter
    (fun v -> sum := !sum + Rt.Long.Vector.length buf v)
    (Monster.vector_of_longs buf m);
  Rt.Option.iter
    (fun v -> sum := !sum + Rt.Double.Vector.length buf v)
    (Monster.vector_of_doubles buf m);
  sum
  := !sum
     + Monster.test buf m ~monster:(fun m -> read_monster buf m) ~default:(fun _ -> 0);
  Rt.Option.iter (fun e -> sum := !sum + read_monster buf e) (Monster.enemy buf m);
  !sum
;;

let read buf () =
  let (Rt.Root (b, m)) = Monster.root P.Bytes buf in
  read_monster b m
;;

let verify buf () =
  match Monster.verify P.Bytes buf with
  | Ok () -> 0
  | Error _ -> failwith "unexpected verification failure"
;;

let verify_then_read buf () =
  match Monster.root_verified P.Bytes buf with
  | Error _ -> failwith "unexpected verification failure"
  | Ok (Rt.Root (b, m)) -> read_monster b m
;;

let allocated_per_call ~iter f =
  let before = Gc.allocated_bytes () in
  for _ = 1 to iter do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.allocated_bytes () in
  (after -. before) /. float_of_int iter
;;

let repeat = 3
let iter = 200_000L

let bench name buf =
  Printf.printf "\n== %s (%d bytes) ==\n%!" name (Bytes.length buf);
  (* Sanity: the shape must actually verify, or the numbers are meaningless. *)
  (match Monster.verify P.Bytes buf with
   | Ok () -> ()
   | Error e -> failwith (Flatbuffers.Verifier.error_to_string e));
  Benchmark.tabulate
    (Benchmark.latencyN
       ~repeat
       iter
       [ "read", read buf, ()
       ; "verify", verify buf, ()
       ; "verify+read", verify_then_read buf, ()
       ]);
  Printf.printf
    "Allocated bytes per call: read %.1f, verify %.1f, verify+read %.1f\n%!"
    (allocated_per_call ~iter:10_000 (read buf))
    (allocated_per_call ~iter:10_000 (verify buf))
    (allocated_per_call ~iter:10_000 (verify_then_read buf))
;;

let () =
  Option.iter (Printf.printf "MEMTRACE: %s\n") (Sys.getenv_opt "MEMTRACE");
  Memtrace.trace_if_requested ();
  bench "small scalar table" (small_monster ());
  bench "vector-heavy monster" (wide_monster ());
  bench "deep table chain (32)" (deep_monster 32)
;;
