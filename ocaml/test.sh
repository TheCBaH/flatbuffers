#!/bin/sh
set -e
set -x
make -j4 flatc 
./flatc --ocaml samples/monster.fbs 
cat monster_generated.ml 
ocaml </dev/null 
echo OK
