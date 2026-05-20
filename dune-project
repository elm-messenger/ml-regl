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
 (name regl_js)
 (synopsis "JS backend for ml_regl")
 (description
  "This library provides OCaml bindings for the regl WebGL library, enabling functional and declarative graphics programming in web applications using OCaml.")
 (depends
  ("ocaml" (>= 4.13))
  ("dune" (>= 3.19))
  "js_of_ocaml"
  "js_of_ocaml-ppx"
  "ml_regl_core"
  "regl_backend")
 (tags
  (graphics web webgl bindings regl)))

(package
 (name regl_desktop)
 (synopsis "Desktop backend for ml_regl")
 (description
  "This library provides OCaml bindings for the regl WebGL library, enabling functional and declarative graphics programming in desktop applications using OCaml.")
 (depends
  ("ocaml" (>= 4.13))
  ("dune" (>= 3.19))
  "ml_regl_core"
  "regl_backend")
 (tags
  (graphics protobuf bindings regl)))

(package
 (name ml_regl_core)
 (synopsis "Portable core of ml_regl (protobuf wire format, no js_of_ocaml)")
 (description
  "Pure OCaml core of ml_regl: smart constructors for backend commands and protobuf encoders/decoders. Has no Js_of_ocaml dependency so it can be linked from native backends such as declgl-desktop.")
 (depends
  ("ocaml" (>= 4.13))
  ("dune" (>= 3.19))
  "ocaml-protoc-plugin")
 (tags
  (graphics protobuf bindings regl)))

(package
 (name regl_backend)
 (synopsis "Virtual library declaring the unified ml_regl backend entry point")
 (description
  "Virtual library: declares the platform-agnostic [Regl_backend.create_app] signature implemented by both [regl_js] (browser) and [regl_desktop] (native). Cross-backend tests link against this plus exactly one implementation, picking the platform at link time.")
 (depends
  ("ocaml" (>= 4.13))
  ("dune" (>= 3.19))
  "ml_regl_core")
 (tags
  (graphics bindings regl virtual)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
