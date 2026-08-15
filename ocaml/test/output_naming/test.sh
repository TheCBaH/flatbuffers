#!/bin/sh
set -eu

flatc=${1:-flatc.ocaml}
case "$flatc" in
  /*) ;;
  *) flatc="$(pwd)/$flatc" ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
include_dir="$script_dir/include"
cases_dir="$script_dir/cases"
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/flatbuffers-ocaml-output.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

assert_files() {
  output_dir=$1
  expected=$2
  actual=$(
    find "$output_dir" -mindepth 1 -maxdepth 1 -type f -exec basename {} \; |
      LC_ALL=C sort
  )
  if [ "$actual" != "$expected" ]; then
    echo "unexpected generated files in $output_dir" >&2
    echo "expected:" >&2
    echo "$expected" >&2
    echo "actual:" >&2
    echo "$actual" >&2
    exit 1
  fi
}

mkdir "$work_dir/default" "$work_dir/explicit" "$work_dir/rootless"

"$flatc" --ocaml -I "$include_dir" -o "$work_dir/default" \
  "$cases_dir/rooted.fbs"
assert_files "$work_dir/default" "rooted.ml
rooted.mli"

"$flatc" --ocaml --bfbs-filenames "$script_dir" -I "$include_dir" \
  -o "$work_dir/explicit" "$cases_dir/rooted.fbs"
assert_files "$work_dir/explicit" "rooted.ml
rooted.mli"

"$flatc" --ocaml --bfbs-filenames "$script_dir" \
  -o "$work_dir/rootless" "$cases_dir/rootless.fbs"
assert_files "$work_dir/rootless" "flatc_output.ml
flatc_output.mli"

echo "OCaml generator output naming tests passed"
