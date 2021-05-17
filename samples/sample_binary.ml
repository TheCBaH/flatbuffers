(*
Copyright 2019 Google Inc. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
open Monster_generated
open FlatBuffers
open MyGame.Sample

(* Example how to use FlatBuffers to create and read binary buffers. *)

let file = "monsterdata.bin"

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let main () =
  let builder = Builder.create () in
  let weapon_one_name = Builder.createString builder "Sword"
  and weapon_one_damage = 3
  and weapon_two_name = Builder.createString builder "Axe"
  and weapon_two_damage = 5 in
  let sword = Weapon.create builder weapon_one_name weapon_one_damage in
  let axe = Weapon.create builder weapon_two_name weapon_two_damage in
  (sword,axe)


(*

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

let _ = Monster.inventoryLength monster

let _ = Monster.weaponsLength monster

let w = Monster.weapons monster 0 |> _opt

let _ = Weapon.damage w
let _ = Weapon.name w

let w = Monster.weapons monster 1 |> _opt

let _ = Weapon.damage w
let _ = Weapon.name w

let _ = Monster.weapons monster 2 |> _opt

let _ = Monster.equipped_type monster

let w = Monster.equipped monster |> _opt |> Weapon.of_union

let _ = Weapon.name w


let _ = Weapon.name monster



let _ = 1

module Buf = struct
  type 'a offset = {
      b: ByteBuffer.t;
      pos: int;
    }
end

let weapon_ w : Weapon.t Buf.offset = {Buf.b = w.Weapon.b;Buf.pos=w.Weapon.pos}

let _ = 1
*)
