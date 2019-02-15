#!/bin/sh
set -e
set -x
make -j4 flatc 
./flatc --ocaml samples/monster.fbs 
#cat monster_generated.ml 
./flatc --ocaml -I tests/include_test tests/monster_test.fbs 
#cat monster_test_generated.ml 
ocaml </dev/null 
./flatc --gen-object-api --ocaml samples/monster.fbs 
./flatc --gen-object-api --ocaml -I tests/include_test tests/monster_test.fbs 
echo ocaml </dev/null 
echo OK
