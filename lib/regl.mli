open Js_of_ocaml

type time_interval = AnimationFrame | Millisecond of float
type regl_config = { time_interval : time_interval }
type texture_mag_option = MagNearest | MagLinear

type texture_min_option =
  | MinNearest
  | MinLinear
  | NearestMipmapNearest
  | LinearMipmapNearest
  | NearestMipmapLinear
  | LinearMipmapLinear

type texture_options = {
  mag : texture_mag_option option;
  min : texture_min_option option;
  crop : ((int * int) * (int * int)) option;
}

type regl_start_config = {
  virt_width : float;
  virt_height : float;
  fbo_num : int;
  builtin_programs : string list option;
}

type texture = { name : string; width : int; height : int }

type regl_recv_msg =
  | REGLTextureLoaded of texture
  | REGLFontLoaded of string
  | REGLProgramCreated of string

val load_texture : string -> string -> texture_options option -> Js.Unsafe.any
val start_regl : regl_start_config -> Js.Unsafe.any
val create_regl_program : string -> Regl_program.regl_program -> Js.Unsafe.any
val config_regl : regl_config -> Js.Unsafe.any
val load_msdf_font : string -> string -> string -> Js.Unsafe.any
val decode_recv_msg : Js.Unsafe.any -> regl_recv_msg option
