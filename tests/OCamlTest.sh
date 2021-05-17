#!/bin/bash -eu
#
# Copyright 2021 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#set -x
testdir=$(dirname $0)
targetdir="${testdir}/ocaml"

if [ -e "${targetdir}" ]; then
    echo "cleaning target"
    rm -rf "${targetdir}"
fi

mkdir -v "${targetdir}"
runtimelibrarydir=${testdir}/../ocaml

function run_test () {
    if $1 ; then
      echo "OK: OCaml $2 passed."
    else
      echo "KO: OCaml $2 failed."
      exit 1
    fi
}

function tools () {
  if [ "$1" = "native" ]; then
    opt="opt"
    obj="cmx"
  else
    opt="c"
    obj="cmo"
  fi
}
function setup () {
  tools $1
  test=flatBuffers_test
  ocaml$opt -c -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers_priv.ml &&
  ocaml$opt -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers_priv.$obj ${runtimelibrarydir}/$test.ml -o ${targetdir}/$test.$opt &&
  ${targetdir}/$test.$opt &&
  ocaml$opt -c -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers.ml
}


function run_flatc () {
  t=$1
  tools $2
  file=${targetdir}/${t}_generated
  ${testdir}/../flatc --ocaml -I ${testdir}/include_test -o ${targetdir} ${testdir}/$t.fbs &&
  ocaml$opt -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers_priv.$obj ${runtimelibrarydir}/flatBuffers.$obj $file.ml -o $file.$opt &&
  $file.$opt &&
  cp $file.ml ${testdir}
}

run_test "setup bytecode" "Setup test [bytecode]"

tests="\
 monster_extra\
 monster_test\
 native_type_test\
"
# arrays_test\
# more_defaults\
# optional_scalars\
for t in $tests; do
  run_test "run_flatc $t bytecode" "$t [bytecode]"
done

run_test "setup native" "Setup test [native]"
for t in $tests; do
  run_test "run_flatc $t native" "$t [native]"
done
