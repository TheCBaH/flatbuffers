open Generated.Casing_test
open Casing

let build_and_read () =
  let b = Rt.Builder.create () in
  let lstm =
    Lstmoptions.Builder.(start b |> add_num_units 128l |> add_cell_clip 1.5 |> finish)
  in
  let rnn =
    Rnnconfig.Builder.(start b |> add_hidden_size 64l |> add_time_major true |> finish)
  in
  let topk = TopKv2Params.Builder.(start b |> add_k 5l |> finish) in
  let conv =
    Conv2Doptions.Builder.(
      start b |> add_stride_w 1l |> add_stride_h 2l |> add_padding 0l |> finish)
  in
  let svdf =
    Svdfparams.Builder.(start b |> add_rank 2l |> add_activation 1l |> finish)
  in
  let root =
    SimpleTable.Builder.(
      start b |> add_value 42l |> add_lstm lstm |> add_rnn rnn |> add_topk topk
      |> add_conv conv |> add_svdf svdf |> finish)
  in
  let buf = SimpleTable.finish_buf Flatbuffers.Primitives.String b root in
  let (Rt.Root (buf, tbl)) = SimpleTable.root Flatbuffers.Primitives.String buf in
  Alcotest.(check int32) "value" 42l (SimpleTable.value buf tbl);
  let lstm = Rt.Option.get (SimpleTable.lstm buf tbl) in
  Alcotest.(check int32) "lstm.num_units" 128l (Lstmoptions.num_units buf lstm);
  Alcotest.(check (float 0.01)) "lstm.cell_clip" 1.5 (Lstmoptions.cell_clip buf lstm);
  let rnn = Rt.Option.get (SimpleTable.rnn buf tbl) in
  Alcotest.(check int32) "rnn.hidden_size" 64l (Rnnconfig.hidden_size buf rnn);
  Alcotest.(check bool) "rnn.time_major" true (Rnnconfig.time_major buf rnn);
  let topk = Rt.Option.get (SimpleTable.topk buf tbl) in
  Alcotest.(check int32) "topk.k" 5l (TopKv2Params.k buf topk);
  let conv = Rt.Option.get (SimpleTable.conv buf tbl) in
  Alcotest.(check int32) "conv.stride_w" 1l (Conv2Doptions.stride_w buf conv);
  Alcotest.(check int32) "conv.stride_h" 2l (Conv2Doptions.stride_h buf conv);
  Alcotest.(check int32) "conv.padding" 0l (Conv2Doptions.padding buf conv);
  let svdf = Rt.Option.get (SimpleTable.svdf buf tbl) in
  Alcotest.(check int32) "svdf.rank" 2l (Svdfparams.rank buf svdf);
  Alcotest.(check int32) "svdf.activation" 1l (Svdfparams.activation buf svdf)

let test_cases = [ Alcotest.test_case "casing round-trip" `Quick build_and_read ]
