
let file = "monsterdata.bin"

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let data = load_file file

let fb = ByteBuffer.of_string data

open MyGame.Sample

let monster = Monster.getRootAsMonster fb

let _ = (ByteBuffer.__offset fb monster.Monster.pos 8) + monster.Monster.pos



let _ = Monster.hp monster
let _ = Monster.name monster
let v = Monster.pos monster
let _opt  = function Some v -> v | None -> failwith "Empty"

let _ = _opt v |> Vec3.x

let _ = Vec3.y v
let _ = Vec3.z v




let _ = 1
