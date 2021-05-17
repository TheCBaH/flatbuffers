#!/bin/sh
set -eu
set -x
ocaml ocaml/flatBuffers_priv.ml
NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
make -j$NCPU flatc
./flatc --ocaml -o ocaml samples/monster.fbs
ocaml -I ocaml ocaml/dev/monster.test.ml
./flatc --ocaml -o ocaml -I tests/include_test tests/monster_test.fbs
ocaml -I ocaml ocaml/dev/monster_test.test.ml
