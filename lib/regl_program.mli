open Js_of_ocaml
module Backend_pb = Transport_backend.Mlregl.Transport.Backend

type prog_value =
  | DynamicValue of string
  | StaticValue of Js.Unsafe.any
  | DynamicTextureValue of string

type regl_program = {
  frag : string;
  vert : string;
  attributes : (string * prog_value) list option;
  uniforms : (string * prog_value) list option;
  elements : prog_value option;
  primitive : prog_value option;
  count : prog_value option;
}

val encode_program_pb : regl_program -> Backend_pb.Program.t
val make_effect_program : string -> regl_program -> regl_program
val make_effect_simple : string -> (string * prog_value) list -> regl_program
val make_compositor_program : string -> string -> regl_program -> regl_program

val make_compositor_simple :
  string -> (string * prog_value) list -> regl_program
