module Leaf = Generated.Test64_bit.LeafStruct
module Root = Generated.Test64_bit.RootTable
module Rt = Generated.Test64_bit.Rt
module B = Rt.Builder
module P = Flatbuffers.Primitives

let values = Array.init 256 (fun i -> Int32.of_int i, Float.of_int i +. 0.5)
let repeat = 3
let traversal_iterations = 50_000L
let conversion_iterations = 10_000L

let consume32 buf vector =
  let sum = ref 0. in
  for i = 0 to Leaf.Vector.length buf vector - 1 do
    let leaf = Leaf.Vector.get buf vector i in
    sum := !sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf
  done;
  !sum
;;

let consume64 buf vector =
  let sum = ref 0. in
  for i = 0 to Leaf.Vector64.length buf vector - 1 do
    let leaf = Leaf.Vector64.get buf vector i in
    sum := !sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf
  done;
  !sum
;;

let consume_iter32 buf vector =
  let sum = ref 0. in
  Leaf.Vector.iter
    buf
    (fun leaf -> sum := !sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf)
    vector;
  !sum
;;

let consume_iter64 buf vector =
  let sum = ref 0. in
  Leaf.Vector64.iter
    buf
    (fun leaf -> sum := !sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf)
    vector;
  !sum
;;

let consume_array32 buf vector =
  Array.fold_left
    (fun sum leaf -> sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf)
    0.
    (Leaf.Vector.to_array buf vector)
;;

let consume_array64 buf vector =
  Array.fold_left
    (fun sum leaf -> sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf)
    0.
    (Leaf.Vector64.to_array buf vector)
;;

let consume_list32 buf vector =
  List.fold_left
    (fun sum leaf -> sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf)
    0.
    (Leaf.Vector.to_list buf vector)
;;

let consume_list64 buf vector =
  List.fold_left
    (fun sum leaf -> sum +. Int32.to_float (Leaf.a buf leaf) +. Leaf.b buf leaf)
    0.
    (Leaf.Vector64.to_list buf vector)
;;

let run32 label buf vector =
  let expected = 65_408. in
  assert (consume32 buf vector = expected);
  Printf.printf "\n%s\n%!" label;
  Benchmark.latencyN
    ~repeat
    traversal_iterations
    [ "indexed", (fun () -> consume32 buf vector), ()
    ; "iter", (fun () -> consume_iter32 buf vector), ()
    ]
  |> Benchmark.tabulate;
  Benchmark.latencyN
    ~repeat
    conversion_iterations
    [ "to_array", (fun () -> consume_array32 buf vector), ()
    ; "to_list", (fun () -> consume_list32 buf vector), ()
    ]
  |> Benchmark.tabulate
;;

let run64 label buf vector =
  let expected = 65_408. in
  assert (consume64 buf vector = expected);
  Printf.printf "\n%s\n%!" label;
  Benchmark.latencyN
    ~repeat
    traversal_iterations
    [ "indexed", (fun () -> consume64 buf vector), ()
    ; "iter", (fun () -> consume_iter64 buf vector), ()
    ]
  |> Benchmark.tabulate;
  Benchmark.latencyN
    ~repeat
    conversion_iterations
    [ "to_array", (fun () -> consume_array64 buf vector), ()
    ; "to_list", (fun () -> consume_list64 buf vector), ()
    ]
  |> Benchmark.tabulate
;;

let run_backend (type a) label (prim : a P.t) =
  let builder = B.create () in
  let vector32 = Leaf.Vector.create builder values in
  let vector64 = Leaf.Vector64.create builder values in
  let root =
    Root.Builder.(
      start builder
      |> add_far_struct_vector vector32
      |> add_big_struct_vector vector64
      |> finish)
  in
  let storage = Root.finish_buf prim builder root in
  let (Rt.Root (buf, root)) = Root.root prim storage in
  let vector32 = Root.far_struct_vector buf root |> Rt.Option.get in
  let vector64 = Root.big_struct_vector buf root |> Rt.Option.get in
  run32 (label ^ " / vector") buf vector32;
  run64 (label ^ " / vector64") buf vector64
;;

let () =
  run_backend "Bytes" P.Bytes;
  run_backend "Bigstringaf" P.Bigstring
;;
