#!/bin/sh
set -e
set -x
ocaml ocaml/flatBuffers_priv.ml
make -j4 flatc
./flatc --ocaml -o ocaml samples/monster.fbs
ocaml ocaml/monster.test.ml
./flatc --ocaml -o ocaml -I tests/include_test tests/monster_test.fbs
ocaml ocaml/monster_test.test.ml
exit 0
./flatc --gen-object-api --ocaml samples/monster.fbs
./flatc --gen-object-api --ocaml -I tests/include_test tests/monster_test.fbs
echo ocaml </dev/null
echo OK
