module Backend_pb = Transport_backend.Mlregl.Transport.Backend
module Common_pb = Transport_common.Mlregl.Transport.Common

type prog_value =
  | DynamicValue of string
  | StaticValue of Common_pb.Value.t
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

val static_number : float -> prog_value
val static_numbers : float list -> prog_value
val static_string : string -> prog_value
val static_strings : string list -> prog_value
val static_bool : bool -> prog_value
