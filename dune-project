(lang dune 3.8)

(name flatbuffers)

(using melange 0.1)

(generate_opam_files true)

(source
 (github TheCBaH/flatbuffers))

(authors "Vladimir Silyaev")

(maintainers "Vladimir Silyaev <thecbah@gmail.com>")

(license Apache-2.0)

(documentation https://github.com/TheCBaH/flatbuffers)

(package
 (name flatbuffers)
 (synopsis "OCaml runtime and code generator for FlatBuffers serialization")
 (description
  "FlatBuffers is a cross-platform serialization library by Google. This package provides an OCaml runtime library and a flatc code generator backend that produces OCaml bindings from .fbs schema files.")
 (depends
  (ocaml (>= 4.13))
  bigstringaf
  (cppo (and :build (>= 1.1.0)))
  (melange :build)
  js_of_ocaml
  (js_of_ocaml-ppx :build)
  (memtrace :with-test)
  (benchmark :with-test)
  (alcotest (and :with-test (>= 1.7))))
 (tags
  (flatbuffers serialization binary ocaml)))

; to make running ocamldebug easier
(wrapped_executables false)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
