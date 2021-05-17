#!/bin/sh
set -eu
set -x
base=$(git merge-base HEAD remotes/flatbuffers/master)
commit=86533fc32d23a6e9b7dba71df6e7e7d16b597572
git diff $base HEAD >.ocaml.patch
echo 'git branch -D pullrequest.ocaml' >.ocaml.branch
echo 'git checkout -b pullrequest.ocaml flatbuffers/master' >>.ocaml.branch
echo 'git apply --exclude=.vscode/\* --exclude=ocaml/dev/\* --verbose --check  .ocaml.patch' >.ocaml.apply
echo 'git apply --exclude=.vscode/\* --exclude=ocaml/dev/\* --index .ocaml.patch' >>.ocaml.apply
echo "git commit -C $commit" >>.ocaml.apply
