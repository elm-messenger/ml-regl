open Js_of_ocaml

let uint8array_of_bytes bytes = Typed_array.Bytes.to_uint8Array bytes
let bytes_of_uint8array arr = Typed_array.Bytes.of_uint8Array arr
