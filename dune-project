(lang dune 3.19)

(name regl)

(version 0.1.0)

(generate_opam_files true)

(source
 (github elm-messenger/ml-regl))

(authors "Yiming Xiang <yiming@cs.utexas.edu>")

(maintainers "Yiming Xiang <yiming@cs.utexas.edu>")

(license "BSD-3-Clause")

(documentation https://github.com/elm-messenger/ml-regl)

(package
 (name regl)
 (synopsis "OCaml bindings for the regl WebGL library")
 (description
  "This library provides OCaml bindings for the regl WebGL library, enabling functional and declarative graphics programming in web applications using OCaml.")
 (depends
  ("ocaml" (>= 4.13))
  ("dune" (>= 3.19))
  "js_of_ocaml"
  "js_of_ocaml-ppx")
 (tags
  (graphics web webgl bindings regl)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
