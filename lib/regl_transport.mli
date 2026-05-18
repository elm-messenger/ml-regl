open Js_of_ocaml

val uint8array_of_bytes : bytes -> Typed_array.uint8Array Js.t
val bytes_of_uint8array : Typed_array.uint8Array Js.t -> bytes
