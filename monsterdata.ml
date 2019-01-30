
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




let _ = Monster.hp monster
let _ = Monster.mana monster
let _ = Monster.color monster

let _ = (ByteBuffer.__offset fb monster.Monster.pos 4) + monster.Monster.pos
let _ = ByteBuffer.__offset fb monster.Monster.pos 10

let _ = Monster.name monster
let _opt  = function Some v -> v | None -> failwith "Empty"

let v = Monster.pos monster |> _opt

let _ = Vec3.x v
let _ = Vec3.y v
let _ = Vec3.z v

let _ = Monster.weaponsLength monster

let w = Monster.weapons monster 0 |> _opt

let _ = Weapon.damage w
let _ = Weapon.name w

let w = Monster.weapons monster 1 |> _opt

let _ = Weapon.damage w
let _ = Weapon.name w

let w = Monster.weapons monster 2 |> _opt

let _ = 1
