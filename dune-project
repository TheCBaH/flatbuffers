(lang dune 3.8)

(name flatbuffers)

(using melange 0.1)

(generate_opam_files false)

(source
 (github TheCBaH/flatbuffers))

(authors "Vladimir Silyaev")

(maintainers "Vladimir Silyaev <thecbah@gmail.com>")

(license Apache-2.0)

(documentation https://github.com/TheCBaH/flatbuffers)

(package
 (name flatbuffers)
 (version 0.1)
 (synopsis "OCaml runtime and code generator for FlatBuffers serialization")
 (description
  "FlatBuffers is a cross-platform serialization library by Google. This package provides an OCaml runtime library and a flatc code generator backend that produces OCaml bindings from .fbs schema files.")
 (depends
  (flatbuffers-tools (>= "0.1"))
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

(package
 (name flatbuffers-tools)
 (version 0.1)
 (synopsis "Flatc code generator with OCaml backend")
 (description
  "Builds the flatc code generator with the OCaml code generation backend from source. Requires cmake and a C++ compiler.")
 (allow_empty)
 (tags
  (flatbuffers serialization codegen ocaml)))

; to make running ocamldebug easier
(wrapped_executables false)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
