#!/bin/sh
set -eu
set -x
base=$(git merge-base HEAD remotes/flatbuffers/master)
commit=847da5734c6addd67b5f933a036b20440d15f6a0
git diff $base HEAD >.ocaml.patch
version=0
echo 'git branch -D pullrequest.ocaml' >.ocaml.branch
echo 'git checkout -b pullrequest.ocaml flatbuffers/master' >>.ocaml.branch
echo 'git apply --exclude=.vscode/\* --exclude=ocaml/dev/\* --verbose --check  .ocaml.patch' >.ocaml.apply
echo 'git apply --exclude=.vscode/\* --exclude=ocaml/dev/\* --index .ocaml.patch' >>.ocaml.apply
echo "git commit -C $commit" >>.ocaml.apply
echo 'git push origin +ocaml_support.pr' > .ocaml.push
echo "git push origin +ocaml_support.pr.v$version" >> .ocaml.push
