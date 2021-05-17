#!/bin/sh
set -eu
set -x
base=$(git merge-base HEAD remotes/flatbuffers/master)
git diff $base HEAD >.ocaml.patch