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

set -x
testdir=$(dirname $0)
targetdir="${testdir}/ocaml"

if [ -e "${targetdir}" ]; then
    echo "cleaning target"
    rm -rf "${targetdir}"
fi

mkdir -v "${targetdir}"
runtimelibrarydir=${testdir}/../ocaml

# Emit Python code for the example schema in the test dir:
tests="\
 monster_extra\
 monster_test\
 native_type_test\
"
# arrays_test\
# more_defaults\
# optional_scalars\
ocamlc -c -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers_priv.ml
ocamlc -c -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers.ml
for t in $tests; do
  file=${targetdir}/${t}_generated
  ${testdir}/../flatc --ocaml -I ${testdir}/include_test -o ${targetdir} ${testdir}/$t.fbs
  ocamlc -I ${runtimelibrarydir} ${runtimelibrarydir}/flatBuffers_priv.cmo ${runtimelibrarydir}/flatBuffers.cmo $file.ml -o $file
  $file
  cp $file.ml ${testdir}
done
